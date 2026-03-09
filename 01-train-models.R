# ---
# Script: 01-train-models.R
# Purpose: Nested cross-validation pipeline for predicting raw and total
#          water use at copper mines.
#
# Architecture:
#   Nested cross-validation (NCV):
#     Outer loop  – 5-fold × 5 repeats, stratified.
#                   Provides an honest, bias-free estimate of generalisation
#                   error by keeping the test folds completely unseen during
#                   both feature selection and hyperparameter tuning.
#     Inner loop  – 5-fold inside each outer training set.
#                   Used simultaneously for:
#                     (a) Recursive Feature Elimination (RFE, rfFuncs): selects
#                         the minimal predictor set that minimises RMSE in
#                         log10 space.
#                     (b) Hyperparameter tuning of every candidate algorithm.
#
#   Preprocessing recipe (log10, centre, scale, dummy, interaction, NZV) is
#   fitted exclusively on each outer TRAINING fold to prevent any information
#   from the held-out fold leaking into transformations.
#
#   Five candidate algorithms are compared:
#     glmnet  – elastic net (strong regularisation baseline for small N)
#     earth   – MARS (piecewise linear, handles interactions)
#     rf      – random forest (ensemble, robust to outliers)
#     cubist  – rule-based boosted regression
#     kknn    – k-nearest neighbours (included for comparison;
#               expected to extrapolate poorly to unseen mines)
#
#   Final model:
#     Best algorithm (lowest mean outer-CV Log10RMSE) retrained on the full
#     dataset using the consensus feature set (selected in >= 50% of outer
#     folds).  A 10-fold CV on the full data is used only for hyperparameter
#     tuning; the outer-CV metrics are the reported generalisation estimates.
#
#   Chain structure (RW -> TW):
#     The TW model uses observed raw_water as a predictor during training.
#     For mines with missing raw_water at prediction time, the RW model is
#     applied first; see 02-predict-water-use.R for error propagation via
#     propagate_pred_error_rf().
#
#   NOTE: The final model objects saved by this script are lists with elements
#     $model    – fitted caret train object
#     $recipe   – prepped recipes::recipe (fitted on full data)
#     $features – character vector of consensus features used
#     $outer_cv – outer-CV model comparison summary
#   Script 02-predict-water-use.R must use this structure.
# ---

source("utils.R")
library(doParallel)

# ---------------------------------------------------------------------------
# Parallel backend: inner RFE + tuning run in parallel; outer loop sequential
# to avoid nested parallelism conflicts.
# ---------------------------------------------------------------------------
n_cores <- max(1L, detectCores() - 1L)
cl <- makeCluster(n_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl), add = TRUE)
message(sprintf("Parallel backend: %d cores", n_cores))

# ---------------------------------------------------------------------------
# Output directory
# ---------------------------------------------------------------------------
results_dir <- file.path("results", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(results_dir, recursive = TRUE)
message("Results will be saved to: ", results_dir)

# ---------------------------------------------------------------------------
# Candidate models
# ---------------------------------------------------------------------------
models <- c("glmnet", "earth", "rf", "cubist", "kknn")
names(models) <- models

# ---------------------------------------------------------------------------
# Input data
# ---------------------------------------------------------------------------
model_data_dir <- tail(sort(Filter(function(x) grepl("\\d{8}_\\d{6}$", x),
                                   list.dirs("./model_data", recursive = FALSE))), 1)
message("Loading model data from: ", model_data_dir)

model_data <- read_csv2(file.path(model_data_dir, "ts_model_data.csv"),
                        show_col_types = FALSE) |>
  mutate_if(is.character, as.factor) |>
  select(id, raw_water, total_water, production, ore_grade, mine_type,
         ore_body_group, process_route, et0_annual, byproduct_production)

# Geographic metadata for LORO / LOCO validation.
# region and country_code are not model predictors; kept separate so they
# never enter the recipe or training loop.
geo_meta <- read_csv2(file.path(model_data_dir, "ts_data_raw.csv"),
                      show_col_types = FALSE) |>
  select(id, country_code, region) |>
  mutate(id = as.integer(id))

# Predictor sets
# Both models share all structural predictors.
# TW additionally uses raw_water (chain structure).
# ore_extracted excluded: collinear with production + ore_grade in log space.
rw_predictors <- c("production", "ore_grade", "mine_type", "process_route",
                   "et0_annual", "ore_body_group", "byproduct_production")
tw_predictors <- c("raw_water", "ore_grade", "mine_type", "process_route",
                   "et0_annual", "ore_body_group", "byproduct_production")

# ---------------------------------------------------------------------------
# Performance metrics
# ---------------------------------------------------------------------------
# All metrics are computed on the original (back-transformed) scale for
# physical interpretability.
#
# Log10RMSE = sqrt(mean((log10(obs) - log10(pred))^2))
#   Primary selection criterion.  10^Log10RMSE is the typical multiplicative
#   prediction factor.  e.g. Log10RMSE = 0.30 -> predictions off by ~2x.
# MAPE: scale-free, treats large and small mines equally.
# Rsquared: variance explained after back-transforming.
#
# data$obs and data$pred arrive in log10 space from caret.
mining_metrics <- function(data, lev = NULL, model = NULL) {
  obs_orig  <- 10^data$obs
  pred_orig <- 10^data$pred
  c(
    Log10RMSE = sqrt(mean((data$obs - data$pred)^2)),
    MAPE      = mean(abs(obs_orig - pred_orig) / obs_orig, na.rm = TRUE) * 100,
    Rsquared  = cor(obs_orig, pred_orig, use = "complete.obs")^2
  )
}

# Evaluate a vector of log10-scale predictions against log10 observations.
# Returns a named numeric vector (Log10RMSE, MAPE, Rsquared).
eval_log10 <- function(obs_log, pred_log) {
  obs_orig  <- 10^obs_log
  pred_orig <- 10^pred_log
  c(
    Log10RMSE = sqrt(mean((obs_log - pred_log)^2)),
    MAPE      = mean(abs(obs_orig - pred_orig) / obs_orig, na.rm = TRUE) * 100,
    Rsquared  = cor(obs_orig, pred_orig, use = "complete.obs")^2
  )
}

################################################################################
####
####  NESTED CROSS-VALIDATION (shared for RW and TW)
####
################################################################################

# run_nested_cv()
#
# Performs nested cross-validation for one outcome.
#
# Design:
#   Outer folds: stratified createMultiFolds(strata, k, times).
#   For each outer fold:
#     1. Fit preprocessing recipe on outer TRAINING data only.
#     2. Bake train and test separately -- no leakage.
#     3. Run RFE (rfFuncs, inner_k-fold) on baked training data to select
#        the minimal feature set that minimises RMSE in log10 space.
#        ntree_rfe controls the RF size used inside RFE (smaller = faster,
#        still sufficient for importance ranking).
#     4. Train each candidate model with inner_k-fold CV on the RFE-selected
#        features.
#     5. Evaluate each model on the outer test fold.
#
# Arguments:
#   df           data.frame with column 'target' and predictor columns
#   base_recipe  unprepped recipes::recipe defined on df
#   strata       column name used for stratified outer folds (character)
#   outer_k      outer CV folds  (default 5)
#   outer_times  outer repeats   (default 5; gives 25 outer evaluations)
#   inner_k      inner CV folds  (default 5)
#   models       named character vector of caret method names
#   ntree_rfe    ntree for RF inside RFE  (default 300)
#   seed         random seed
#
# Returns a list:
#   $fold_metrics   data.frame: per-fold x per-model metrics
#   $feature_votes  named integer: selection count per feature across folds
#   $n_outer_folds  integer: total outer folds evaluated
run_nested_cv <- function(df,
                          base_recipe,
                          strata,
                          outer_k     = 5L,
                          outer_times = 5L,
                          inner_k     = 5L,
                          models,
                          ntree_rfe   = 300L,
                          seed        = 9867L) {

  set.seed(seed)
  strata_vec  <- interaction(df[, strata, drop = FALSE], drop = TRUE)
  outer_folds <- createMultiFolds(strata_vec, k = outer_k, times = outer_times)

  # Inner controls (parallel allowed -- outer loop is sequential)
  rfe_ctrl <- rfeControl(
    functions     = rfFuncs,
    method        = "cv",
    number        = inner_k,
    allowParallel = TRUE,
    returnResamp  = "final",
    verbose       = FALSE
  )
  inner_ctrl <- trainControl(
    method          = "cv",
    number          = inner_k,
    summaryFunction = mining_metrics,
    allowParallel   = TRUE,
    verboseIter     = FALSE
  )

  all_metrics       <- vector("list", length(outer_folds))
  all_feature_votes <- vector("list", length(outer_folds))

  for (i in seq_along(outer_folds)) {
    fold_name   <- names(outer_folds)[i]
    idx_train   <- outer_folds[[i]]
    train_outer <- df[ idx_train, ]
    test_outer  <- df[-idx_train, ]

    # ---- Preprocessing: fit on outer TRAIN only ----
    rec_prep    <- prep(base_recipe, training = train_outer)
    train_baked <- bake(rec_prep, new_data = train_outer)
    test_baked  <- bake(rec_prep, new_data = test_outer)

    x_train <- as.data.frame(select(train_baked, -target))
    y_train <- train_baked$target
    x_test  <- as.data.frame(select(test_baked,  -target))
    y_test  <- test_baked$target

    n_feat <- ncol(x_train)

    # ---- RFE: feature selection on outer training set ----
    rfe_result <- tryCatch(
      rfe(
        x          = x_train,
        y          = y_train,
        sizes      = seq(2L, n_feat),
        metric     = "RMSE",
        ntree      = ntree_rfe,
        rfeControl = rfe_ctrl
      ),
      error = function(e) {
        message(sprintf("  RFE failed in fold %s: %s", fold_name, e$message))
        NULL
      }
    )

    selected_vars <- if (!is.null(rfe_result)) {
      predictors(rfe_result)
    } else {
      colnames(x_train)   # fallback: use all features
    }
    all_feature_votes[[i]] <- selected_vars

    # ---- Train each candidate model on RFE-selected features ----
    fold_metrics <- lapply(models, function(method) {
      fit <- tryCatch(
        train(
          x         = x_train[, selected_vars, drop = FALSE],
          y         = y_train,
          method    = method,
          trControl = inner_ctrl,
          metric    = "Log10RMSE",
          maximize  = FALSE
        ),
        error = function(e) {
          message(sprintf("  %s failed in fold %s: %s", method, fold_name, e$message))
          NULL
        }
      )
      if (is.null(fit)) return(NULL)

      pred    <- predict(fit, newdata = x_test[, selected_vars, drop = FALSE])
      metrics <- eval_log10(y_test, pred)

      data.frame(
        fold       = fold_name,
        model      = method,
        as.data.frame(as.list(metrics)),
        n_features = length(selected_vars),
        features   = paste(sort(selected_vars), collapse = "|"),
        stringsAsFactors = FALSE
      )
    })

    all_metrics[[i]] <- bind_rows(fold_metrics)
    message(sprintf("  Outer fold %d / %d done (%s)",
                    i, length(outer_folds), fold_name))
  }

  list(
    fold_metrics  = bind_rows(all_metrics),
    feature_votes = table(unlist(all_feature_votes)),
    n_outer_folds = length(outer_folds)
  )
}

################################################################################
####
####  FINAL MODEL (shared for RW and TW)
####
################################################################################

# fit_final_model()
#
# Retrains the selected algorithm on ALL available data using the consensus
# feature set.  A 10-fold CV is used here solely for hyperparameter tuning;
# the generalisation estimate comes from the outer NCV loop above.
#
# Returns a list:
#   $model    caret train object
#   $recipe   prepped recipe (fitted on full df)
#   $features character vector: features used
fit_final_model <- function(df,
                            base_recipe,
                            consensus_features,
                            best_method,
                            seed = 9867L) {
  set.seed(seed)
  rec_prep <- prep(base_recipe, training = df)
  df_baked <- bake(rec_prep, new_data = df)

  x_full <- as.data.frame(select(df_baked, -target))
  y_full <- df_baked$target

  # Intersect with columns that survived NZV on full data
  final_features <- intersect(consensus_features, colnames(x_full))
  if (length(final_features) == 0L) {
    warning("No consensus features survived NZV on full data; using all features.")
    final_features <- colnames(x_full)
  }

  final_ctrl <- trainControl(
    method          = "cv",
    number          = 10L,
    summaryFunction = mining_metrics,
    allowParallel   = TRUE,
    verboseIter     = FALSE
  )

  model <- train(
    x         = x_full[, final_features, drop = FALSE],
    y         = y_full,
    method    = best_method,
    trControl = final_ctrl,
    metric    = "Log10RMSE",
    maximize  = FALSE
  )

  list(model = model, recipe = rec_prep, features = final_features)
}

################################################################################
####
####  DIAGNOSTIC / PLOT HELPERS
####
################################################################################

# Outer-CV model comparison: boxplots of fold-level metrics per model.
plot_model_comparison <- function(fold_metrics, model_summary, title) {
  model_order <- model_summary$model[order(model_summary$Log10RMSE_mean)]
  best        <- model_order[1]
  fold_metrics |>
    mutate(model = factor(model, levels = model_order)) |>
    pivot_longer(c(Log10RMSE, MAPE, Rsquared),
                 names_to = "Metric", values_to = "Value") |>
    mutate(Metric = factor(Metric, levels = c("Log10RMSE", "MAPE", "Rsquared"))) |>
    ggplot(aes(x = model, y = Value)) +
    geom_boxplot(fill = "grey85", outlier.size = 1) +
    facet_wrap(~ Metric, scales = "free_y") +
    labs(x = "Model", y = "Outer CV metric", title = title,
         subtitle = sprintf(
           "Best: %s  (Log10RMSE = %.3f \u00b1 %.3f)",
           best,
           model_summary$Log10RMSE_mean[model_summary$model == best],
           model_summary$Log10RMSE_sd[model_summary$model == best]
         )) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# RFE feature selection frequency across outer folds.
# Dashed line at 50% = consensus threshold.
plot_feature_votes <- function(feature_votes, n_outer_folds, title) {
  data.frame(
    Feature = names(feature_votes),
    Freq    = as.numeric(feature_votes) / n_outer_folds
  ) |>
    mutate(
      Selected = Freq >= 0.5,
      Feature  = reorder(Feature, Freq)
    ) |>
    ggplot(aes(x = Feature, y = Freq, fill = Selected)) +
    geom_col() +
    geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
    scale_fill_manual(
      values = c("TRUE" = "grey30", "FALSE" = "grey80"),
      labels = c("TRUE" = "Selected (\u226550%)", "FALSE" = "Excluded"),
      name   = NULL
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    coord_flip() +
    labs(x = NULL, y = "Selection frequency across outer folds", title = title) +
    theme_minimal()
}

# Variable importance for the final caret model.
plot_variable_importance <- function(final_obj, title) {
  vi <- varImp(final_obj$model)$importance
  vi$Feature <- rownames(vi)
  vi |>
    arrange(Overall) |>
    mutate(Feature = factor(Feature, levels = Feature)) |>
    ggplot(aes(x = Feature, y = Overall)) +
    geom_col(fill = "grey40") +
    coord_flip() +
    labs(x = NULL, y = "Importance (scaled 0-100)", title = title) +
    theme_minimal()
}

# Three-panel residual diagnostics: QQ, histogram, predicted vs observed.
plot_diagnostics <- function(residuals_df, x_label, y_label, colour_var) {
  qq <- ggplot(residuals_df, aes(sample = residuals)) +
    stat_qq() + stat_qq_line() +
    labs(x = "Theoretical quantiles", y = "Sample quantiles (log10)") +
    theme_minimal()

  hist_p <- ggplot(residuals_df, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.1,
                   fill = "grey60", colour = "white", alpha = 0.8) +
    labs(x = "Residuals (log10)", y = "Density") +
    theme_minimal()

  pred_obs <- ggplot(residuals_df,
                     aes(x = pred_log, y = obs_log,
                         colour = .data[[colour_var]])) +
    geom_point(alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = x_label, y = y_label) +
    theme_minimal()

  qq | hist_p | pred_obs
}

# Helper: build residuals data frame from a final_obj and its training df.
build_residuals <- function(final_obj, df, colour_var) {
  x <- as.data.frame(
    select(bake(final_obj$recipe, new_data = df), -target)
  )[, final_obj$features, drop = FALSE]
  pred_log <- predict(final_obj$model, newdata = x)
  obs_log  <- bake(final_obj$recipe, new_data = df)$target
  df_out   <- data.frame(obs_log = obs_log, pred_log = pred_log,
                         residuals = obs_log - pred_log)
  df_out[[colour_var]] <- df[[colour_var]]
  df_out
}

################################################################################
####
####  GEOGRAPHIC GENERALISATION FUNCTIONS
####
################################################################################

# run_geographic_cv()
#
# Leave-one-group-out CV for geographic generalisation auditing.
# Trains on all observations NOT in the held-out group, tests on the group.
# Uses the fixed consensus features and best method (no re-selection) --
# this is an audit of generalization, not a model selection step.
#
# Arguments:
#   df                 data.frame: target + predictor columns (no geo columns)
#   geo_vec            character: group label per row (same length as nrow(df))
#   base_recipe        unprepped recipe (fitted fresh on each training fold)
#   groups             character: groups to evaluate (filtered for min size)
#   consensus_features character: features from the full nested CV
#   best_method        character: caret method name
#   seed               integer
#
# Returns list($summary, $preds) -- one row per group in summary.
run_geographic_cv <- function(df, geo_vec, base_recipe, groups,
                              consensus_features, best_method, seed = 9867L) {
  results <- lapply(groups, function(g) {
    in_group <- !is.na(geo_vec) & geo_vec == g
    n_test   <- sum(in_group)
    if (n_test < 3L) return(NULL)

    train_df <- df[!in_group, ]
    test_df  <- df[ in_group, ]

    prepped      <- prep(base_recipe, training = train_df)
    x_train_full <- bake(prepped, new_data = NULL)
    x_test_full  <- bake(prepped, new_data = test_df)

    avail   <- intersect(consensus_features, names(x_train_full))
    if (length(avail) < 2L) return(NULL)

    x_train  <- as.data.frame(x_train_full[, avail, drop = FALSE])
    x_test   <- as.data.frame(x_test_full[,  avail, drop = FALSE])
    y_train  <- x_train_full$target
    y_test   <- x_test_full$target

    set.seed(seed)
    model <- train(x = x_train, y = y_train, method = best_method,
                   trControl = trainControl(method = "none"),
                   metric = "Log10RMSE", maximize = FALSE)

    pred_log <- predict(model, newdata = x_test)
    metrics  <- eval_log10(y_test, pred_log)

    list(
      summary = data.frame(
        group     = g,
        n_test    = n_test,
        Log10RMSE = metrics[["Log10RMSE"]],
        MAPE      = metrics[["MAPE"]],
        Rsquared  = metrics[["Rsquared"]]
      ),
      preds = data.frame(
        group     = g,
        obs_log   = y_test,
        pred_log  = pred_log,
        residuals = y_test - pred_log
      )
    )
  })

  results <- Filter(Negate(is.null), results)
  list(
    summary = do.call(rbind, lapply(results, `[[`, "summary")),
    preds   = do.call(rbind, lapply(results, `[[`, "preds"))
  )
}

# Two-panel plot: Log10RMSE bar chart + predicted-vs-observed scatter.
# Groups worse than nested_cv_rmse * 1.25 are highlighted in red.
plot_geo_cv <- function(cv_result, title, nested_cv_rmse, xlab = "Group") {
  smry <- cv_result$summary |>
    arrange(Log10RMSE) |>
    mutate(
      group = factor(group, levels = group),
      flag  = Log10RMSE > nested_cv_rmse * 1.25
    )

  p_bar <- ggplot(smry, aes(x = group, y = Log10RMSE, fill = flag)) +
    geom_col() +
    geom_hline(yintercept = nested_cv_rmse, linetype = "dashed",
               colour = "grey40", linewidth = 0.8) +
    geom_text(aes(label = paste0("n=", n_test)), hjust = -0.15, size = 3) +
    coord_flip() +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "firebrick"),
                      guide  = "none") +
    annotate("text", y = nested_cv_rmse + 0.005, x = 0.65,
             label = "Nested CV mean", hjust = 0, size = 3, colour = "grey40") +
    labs(x = xlab, y = "Log10RMSE (held-out group)", title = title) +
    theme_minimal()

  p_scatter <- ggplot(cv_result$preds,
                      aes(x = pred_log, y = obs_log, colour = group)) +
    geom_point(alpha = 0.65, size = 1.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey30") +
    labs(x = "Predicted (log10)", y = "Observed (log10)", colour = xlab) +
    theme_minimal()

  p_bar | p_scatter
}

# Residual boxplot by geographic group using in-sample final-model residuals.
plot_residuals_geo <- function(resid_df, group_col, title, min_n = 3L) {
  resid_df |>
    filter(!is.na(.data[[group_col]])) |>
    group_by(.data[[group_col]]) |>
    filter(n() >= min_n) |>
    mutate(n = n(), med = median(residuals),
           label = paste0(.data[[group_col]], "  (n=", n, ")")) |>
    ungroup() |>
    mutate(label = reorder(label, med)) |>
    ggplot(aes(x = label, y = residuals)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "firebrick",
               linewidth = 0.7) +
    geom_boxplot(fill = "grey80", outlier.size = 1, outlier.alpha = 0.5) +
    coord_flip() +
    labs(x = NULL, y = "Residuals (log10 obs \u2212 pred)", title = title) +
    theme_minimal()
}

################################################################################
####
####                            RAW WATER
####
################################################################################
message("\n=== RAW WATER ===")

rw_df <- model_data |>
  select(id, target = raw_water, all_of(rw_predictors)) |>
  filter(production > 0, ore_grade > 0) |>
  drop_na()

rw_geo <- left_join(select(rw_df, id), geo_meta, by = "id")
rw_df  <- select(rw_df, -id)

message(sprintf("RW training set: %d observations, %d candidate predictors",
                nrow(rw_df), length(rw_predictors)))
message("RW process_route distribution:")
print(table(rw_df$process_route))

# Preprocessing recipe.
# step_log applied to ALL numeric (including target) so the model operates in
# log10 space throughout -- appropriate for right-skewed water volumes.
# step_interact: production x process_route allows different production-water
# slopes per route (hydromet vs flotation vs pyro_based differ physically).
# step_nzv removes near-zero-variance dummies that arise after merging sparse
# categories; also acts as a safety net after RFE reduces feature count.
rw_recipe <- recipe(target ~ ., data = rw_df) |>
  step_log(all_numeric(), base = 10) |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_unknown(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_interact(terms = ~ starts_with("production") *
                          starts_with("process_route")) |>
  step_nzv(all_predictors())

# ---- Nested CV ----
message("Running nested CV for raw water",
        " (outer 5x5, inner 5-fold RFE + tuning) ...")
rw_ncv <- run_nested_cv(
  df          = rw_df,
  base_recipe = rw_recipe,
  strata      = "process_route",
  outer_k     = 5L,
  outer_times = 5L,
  inner_k     = 5L,
  models      = models,
  ntree_rfe   = 300L,
  seed        = 9867L
)

# ---- Outer-CV model summary ----
rw_model_summary <- rw_ncv$fold_metrics |>
  group_by(model) |>
  summarise(
    Log10RMSE_mean = mean(Log10RMSE, na.rm = TRUE),
    Log10RMSE_sd   = sd(Log10RMSE,   na.rm = TRUE),
    MAPE_mean      = mean(MAPE,       na.rm = TRUE),
    Rsquared_mean  = mean(Rsquared,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(Log10RMSE_mean)

rw_best_method <- rw_model_summary$model[1]
message(sprintf(
  "Best model for raw water: %s  (outer CV Log10RMSE = %.3f +/- %.3f)",
  rw_best_method,
  rw_model_summary$Log10RMSE_mean[1],
  rw_model_summary$Log10RMSE_sd[1]
))

# ---- Consensus features ----
rw_consensus <- names(
  rw_ncv$feature_votes[rw_ncv$feature_votes >= 0.5 * rw_ncv$n_outer_folds]
)
message(sprintf("RW consensus features (%d): %s",
                length(rw_consensus),
                paste(sort(rw_consensus), collapse = ", ")))

# ---- Save tabular outputs ----
write_csv(rw_model_summary,
          file.path(results_dir, "rw_model_comparison.csv"))
write_csv(rw_ncv$fold_metrics,
          file.path(results_dir, "rw_outer_cv_metrics.csv"))
write_csv(
  data.frame(
    Feature  = names(rw_ncv$feature_votes),
    Votes    = as.numeric(rw_ncv$feature_votes),
    Freq     = as.numeric(rw_ncv$feature_votes) / rw_ncv$n_outer_folds,
    Selected = as.numeric(rw_ncv$feature_votes) >= 0.5 * rw_ncv$n_outer_folds
  ),
  file.path(results_dir, "rw_feature_selection.csv")
)

# ---- Plots: model comparison and feature selection ----
png(file.path(results_dir, "rw_model_comparison.png"),
    width = 300, height = 140, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_model_comparison(rw_ncv$fold_metrics, rw_model_summary,
                            "Raw water: nested CV model comparison"))
dev.off()

png(file.path(results_dir, "rw_feature_selection.png"),
    width = 220, height = 180, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_feature_votes(rw_ncv$feature_votes, rw_ncv$n_outer_folds,
                         "Raw water: RFE feature selection frequency"))
dev.off()

# ---- Final model ----
message("Fitting final raw water model on full data...")
rw_final <- fit_final_model(
  df                 = rw_df,
  base_recipe        = rw_recipe,
  consensus_features = rw_consensus,
  best_method        = rw_best_method,
  seed               = 9867L
)
rw_final$outer_cv <- rw_model_summary

capture.output(
  { cat("=== RW FINAL MODEL ===\n"); print(rw_final$model) },
  file  = file.path(results_dir, "rw_training_log.txt"),
  append = FALSE
)

write_rds(rw_final, file.path(results_dir, "rw_final_model.rds"))
message("rw_final_model.rds saved.")

# ---- Diagnostics ----
png(file.path(results_dir, "rw_variable_importance.png"),
    width = 220, height = 180, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_variable_importance(rw_final,
                               "Raw water: variable importance (final model)"))
dev.off()

rw_resid <- build_residuals(rw_final, rw_df, "process_route")
rw_resid$region       <- rw_geo$region
rw_resid$country_code <- rw_geo$country_code
shapiro.test(rw_resid$residuals)

png(file.path(results_dir, "rw_diagnostics.png"),
    width = 350, height = 120, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_diagnostics(
  rw_resid,
  x_label    = "Predicted log10(raw water)",
  y_label    = "Observed log10(raw water)",
  colour_var = "process_route"
))
dev.off()

################################################################################
####
####                            TOTAL WATER  (chain: uses raw_water)
####
################################################################################
message("\n=== TOTAL WATER ===")

# raw_water here is the OBSERVED value -- the training set only contains
# mine-years for which both raw_water and total_water are recorded.
# For prediction on mines without observed raw_water, rw_final is applied
# first (see 02-predict-water-use.R).
tw_df <- model_data |>
  select(id, target = total_water, all_of(tw_predictors)) |>
  filter(raw_water > 0, ore_grade > 0) |>
  drop_na()

tw_geo <- left_join(select(tw_df, id), geo_meta, by = "id")
tw_df  <- select(tw_df, -id)

message(sprintf("TW training set: %d observations, %d candidate predictors",
                nrow(tw_df), length(tw_predictors)))
message("TW mine_type distribution:")
print(table(tw_df$mine_type))

# raw_water x process_route interaction: the total-to-raw ratio (recirculation
# fraction) differs by process route -- hydromet recirculates far more water
# than flotation concentrators, so a common slope on raw_water is incorrect.
tw_recipe <- recipe(target ~ ., data = tw_df) |>
  step_log(all_numeric(), base = 10) |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_unknown(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_interact(terms = ~ starts_with("raw_water") *
                          starts_with("process_route")) |>
  step_nzv(all_predictors())

# ---- Nested CV ----
# Stratify by mine_type for TW (mine_type distribution is more balanced than
# process_route for the small TW set; process_route stratification risks
# folds with only 1-2 hydro or pyro_based samples at N=90).
message("Running nested CV for total water",
        " (outer 5x5, inner 5-fold RFE + tuning) ...")
tw_ncv <- run_nested_cv(
  df          = tw_df,
  base_recipe = tw_recipe,
  strata      = "mine_type",
  outer_k     = 5L,
  outer_times = 5L,
  inner_k     = 5L,
  models      = models,
  ntree_rfe   = 300L,
  seed        = 9867L
)

# ---- Outer-CV model summary ----
tw_model_summary <- tw_ncv$fold_metrics |>
  group_by(model) |>
  summarise(
    Log10RMSE_mean = mean(Log10RMSE, na.rm = TRUE),
    Log10RMSE_sd   = sd(Log10RMSE,   na.rm = TRUE),
    MAPE_mean      = mean(MAPE,       na.rm = TRUE),
    Rsquared_mean  = mean(Rsquared,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(Log10RMSE_mean)

tw_best_method <- tw_model_summary$model[1]
message(sprintf(
  "Best model for total water: %s  (outer CV Log10RMSE = %.3f +/- %.3f)",
  tw_best_method,
  tw_model_summary$Log10RMSE_mean[1],
  tw_model_summary$Log10RMSE_sd[1]
))

# ---- Consensus features ----
tw_consensus <- names(
  tw_ncv$feature_votes[tw_ncv$feature_votes >= 0.5 * tw_ncv$n_outer_folds]
)
message(sprintf("TW consensus features (%d): %s",
                length(tw_consensus),
                paste(sort(tw_consensus), collapse = ", ")))

# ---- Save tabular outputs ----
write_csv(tw_model_summary,
          file.path(results_dir, "tw_model_comparison.csv"))
write_csv(tw_ncv$fold_metrics,
          file.path(results_dir, "tw_outer_cv_metrics.csv"))
write_csv(
  data.frame(
    Feature  = names(tw_ncv$feature_votes),
    Votes    = as.numeric(tw_ncv$feature_votes),
    Freq     = as.numeric(tw_ncv$feature_votes) / tw_ncv$n_outer_folds,
    Selected = as.numeric(tw_ncv$feature_votes) >= 0.5 * tw_ncv$n_outer_folds
  ),
  file.path(results_dir, "tw_feature_selection.csv")
)

# ---- Plots: model comparison and feature selection ----
png(file.path(results_dir, "tw_model_comparison.png"),
    width = 300, height = 140, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_model_comparison(tw_ncv$fold_metrics, tw_model_summary,
                            "Total water: nested CV model comparison"))
dev.off()

png(file.path(results_dir, "tw_feature_selection.png"),
    width = 220, height = 180, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_feature_votes(tw_ncv$feature_votes, tw_ncv$n_outer_folds,
                         "Total water: RFE feature selection frequency"))
dev.off()

# ---- Final model ----
message("Fitting final total water model on full data...")
tw_final <- fit_final_model(
  df                 = tw_df,
  base_recipe        = tw_recipe,
  consensus_features = tw_consensus,
  best_method        = tw_best_method,
  seed               = 9867L
)
tw_final$outer_cv <- tw_model_summary

capture.output(
  { cat("=== TW FINAL MODEL ===\n"); print(tw_final$model) },
  file  = file.path(results_dir, "tw_training_log.txt"),
  append = FALSE
)

write_rds(tw_final, file.path(results_dir, "tw_final_model.rds"))
message("tw_final_model.rds saved.")

# ---- Diagnostics ----
png(file.path(results_dir, "tw_variable_importance.png"),
    width = 220, height = 180, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_variable_importance(tw_final,
                               "Total water: variable importance (final model)"))
dev.off()

tw_resid <- build_residuals(tw_final, tw_df, "process_route")
tw_resid$region       <- tw_geo$region
tw_resid$country_code <- tw_geo$country_code
shapiro.test(tw_resid$residuals)

png(file.path(results_dir, "tw_diagnostics.png"),
    width = 350, height = 120, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_diagnostics(
  tw_resid,
  x_label    = "Predicted log10(total water)",
  y_label    = "Observed log10(total water)",
  colour_var = "process_route"
))
dev.off()

################################################################################
####
####  GEOGRAPHIC GENERALISATION AUDIT  (LORO + LOCO)
####
################################################################################
message("\n=== GEOGRAPHIC GENERALISATION AUDIT ===")

# ---- Define groups ----
# LORO: regions with >= 5 test observations
# LOCO: countries with >= 10 total observations in the respective training set
rw_loro_groups <- names(which(table(rw_geo$region[!is.na(rw_geo$region)]) >= 5L))
rw_loco_groups <- names(which(table(rw_geo$country_code[!is.na(rw_geo$country_code)]) >= 10L))
tw_loro_groups <- names(which(table(tw_geo$region[!is.na(tw_geo$region)]) >= 5L))
tw_loco_groups <- names(which(table(tw_geo$country_code[!is.na(tw_geo$country_code)]) >= 10L))

message(sprintf("RW LORO groups (%d): %s", length(rw_loro_groups),
                paste(sort(rw_loro_groups), collapse = ", ")))
message(sprintf("RW LOCO groups (%d): %s", length(rw_loco_groups),
                paste(sort(rw_loco_groups), collapse = ", ")))
message(sprintf("TW LORO groups (%d): %s", length(tw_loro_groups),
                paste(sort(tw_loro_groups), collapse = ", ")))
message(sprintf("TW LOCO groups (%d): %s", length(tw_loco_groups),
                paste(sort(tw_loco_groups), collapse = ", ")))

# ---- LORO ----
message("Running LORO for raw water...")
rw_loro <- run_geographic_cv(rw_df, rw_geo$region, rw_recipe,
                             rw_loro_groups, rw_consensus, rw_best_method)

message("Running LORO for total water...")
tw_loro <- run_geographic_cv(tw_df, tw_geo$region, tw_recipe,
                             tw_loro_groups, tw_consensus, tw_best_method)

# ---- LOCO ----
message("Running LOCO for raw water...")
rw_loco <- run_geographic_cv(rw_df, rw_geo$country_code, rw_recipe,
                             rw_loco_groups, rw_consensus, rw_best_method)

message("Running LOCO for total water...")
tw_loco <- run_geographic_cv(tw_df, tw_geo$country_code, tw_recipe,
                             tw_loco_groups, tw_consensus, tw_best_method)

# ---- Save tabular results ----
write_csv(rw_loro$summary, file.path(results_dir, "rw_loro_summary.csv"))
write_csv(tw_loro$summary, file.path(results_dir, "tw_loro_summary.csv"))
write_csv(rw_loco$summary, file.path(results_dir, "rw_loco_summary.csv"))
write_csv(tw_loco$summary, file.path(results_dir, "tw_loco_summary.csv"))
write_csv(rw_resid[, c("obs_log","pred_log","residuals","region","country_code")],
          file.path(results_dir, "rw_residuals_geo.csv"))
write_csv(tw_resid[, c("obs_log","pred_log","residuals","region","country_code")],
          file.path(results_dir, "tw_residuals_geo.csv"))

message("RW LORO summary:")
print(rw_loro$summary |> arrange(Log10RMSE))
message("TW LORO summary:")
print(tw_loro$summary |> arrange(Log10RMSE))
message("RW LOCO summary (top 10 by Log10RMSE):")
print(rw_loco$summary |> arrange(Log10RMSE) |> head(10))

# ---- Plots: LORO ----
rw_ncv_rmse <- rw_model_summary$Log10RMSE_mean[rw_model_summary$model == rw_best_method]
tw_ncv_rmse <- tw_model_summary$Log10RMSE_mean[tw_model_summary$model == tw_best_method]

png(file.path(results_dir, "rw_loro.png"),
    width = 320, height = 160, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_geo_cv(rw_loro, "Raw water: Leave-One-Region-Out",
                  rw_ncv_rmse, xlab = "Region"))
dev.off()

png(file.path(results_dir, "tw_loro.png"),
    width = 320, height = 160, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_geo_cv(tw_loro, "Total water: Leave-One-Region-Out",
                  tw_ncv_rmse, xlab = "Region"))
dev.off()

# ---- Plots: LOCO ----
png(file.path(results_dir, "rw_loco.png"),
    width = 320, height = 200, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_geo_cv(rw_loco, "Raw water: Leave-One-Country-Out",
                  rw_ncv_rmse, xlab = "Country"))
dev.off()

png(file.path(results_dir, "tw_loco.png"),
    width = 320, height = 200, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_geo_cv(tw_loco, "Total water: Leave-One-Country-Out",
                  tw_ncv_rmse, xlab = "Country"))
dev.off()

# ---- Plots: in-sample residuals by region and country ----
png(file.path(results_dir, "rw_residuals_by_region.png"),
    width = 240, height = 160, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_residuals_geo(rw_resid, "region",
                         "Raw water: in-sample residuals by region"))
dev.off()

png(file.path(results_dir, "rw_residuals_by_country.png"),
    width = 240, height = 220, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_residuals_geo(rw_resid, "country_code",
                         "Raw water: in-sample residuals by country"))
dev.off()

png(file.path(results_dir, "tw_residuals_by_region.png"),
    width = 240, height = 160, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_residuals_geo(tw_resid, "region",
                         "Total water: in-sample residuals by region"))
dev.off()

png(file.path(results_dir, "tw_residuals_by_country.png"),
    width = 240, height = 180, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(plot_residuals_geo(tw_resid, "country_code",
                         "Total water: in-sample residuals by country"))
dev.off()

# ---------------------------------------------------------------------------
message("\nDone. All results saved to: ", results_dir)

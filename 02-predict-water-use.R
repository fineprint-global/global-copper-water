# ---
# Script: 02-predict-water-use.R
# Purpose: Apply trained ML models to generate water use predictions with full
#          uncertainty quantification and Bayesian mine-level trend estimation.
#
# Key Steps:
# 1. Predict raw water (RW) and total water (TW) for all mine-year observations.
# 2. Propagate RW prediction uncertainty into TW for mines where raw_water was
#    missing and had to be imputed from the RW model:
#      tw_sd_total_log10 = sqrt(tw_sd_log10^2 + rw_sd_log10^2)  [quadrature]
# 3. Compute 95% prediction intervals in original scale:
#      lower/upper = 10^(log10(pred) +/- 1.96 * sd_log10)
# 4. Fit a Bayesian mixed-effects model with AR(1) errors to estimate mine-level
#    annual trends in raw water use (2015-2019):
#      log(RW) ~ year_c + (year_c | id_mine) + AR(1)
#    Model results are cached to avoid expensive re-fitting on reruns.
# 5. Spatial autocorrelation (Moran's I) is tested as a pre-modelling diagnostic.
# 6. Mine-level slope posteriors (median, 95% CI, P(slope>0)) are extracted and
#    joined to the predictions dataset.
# 7. Save final_predictions.csv with all predictions, uncertainty, spatial
#    attributes, and slope estimates.
#
# Output columns (key):
#   rw_pred / tw_pred            – predicted water use (ML)
#   rw_pred_sd_log10             – SD of RF tree predictions in log10 space
#   tw_sd_total_log10            – propagated TW uncertainty (log10 SD)
#   rw_pi_lower / rw_pi_upper    – 95% prediction interval (ML)
#   tw_pi_lower / tw_pi_upper    – 95% prediction interval (ML), propagated
#   tw_uncertainty_source        – "observed_rw" or "imputed_rw"
#   rw_filled / tw_filled        – observed when available, predicted otherwise (ML)
#   rw_median_slope              – Bayesian posterior median slope (log(Mm3)/yr)
#   rw_ci_lower / rw_ci_upper    – 95% credible interval for slope
#   rw_prob_gt0                  – P(slope > 0) across posterior draws
#   rw_slope_uncertainty         – 0 = certain direction, 1 = maximally uncertain
# ---


source("utils.R")
set.seed(96297)

################################################################################
### Auto-detect latest versioned directories
ts_pattern     <- "\\d{8}_\\d{6}$"
results_dir    <- tail(sort(Filter(function(x) grepl(ts_pattern, x),
                                   list.dirs("./results",    recursive = FALSE))), 1)
model_data_dir <- tail(sort(Filter(function(x) grepl(ts_pattern, x),
                                   list.dirs("./model_data", recursive = FALSE))), 1)
message("Loading models from: ", results_dir)
message("Loading data from:   ", model_data_dir)

# Bayesian model cache: always use the current results directory so the
# model is refit whenever the data version changes.
bayes_cache <- file.path(results_dir, "fit_bayes_trends.rds")
message("Bayesian cache: ", bayes_cache)

################################################################################
### Load models
rw_model <- read_rds(file.path(results_dir, "rw_final_model.rds"))
tw_model <- read_rds(file.path(results_dir, "tw_final_model.rds"))

# Original (pre-dummy) predictor variable names from each model's prepped recipe
rw_raw_vars <- rw_model$recipe$var_info$variable[rw_model$recipe$var_info$role != "outcome"]
tw_raw_vars <- tw_model$recipe$var_info$variable[tw_model$recipe$var_info$role != "outcome"]
all_vars    <- union(rw_raw_vars, tw_raw_vars)

################################################################################
### Load data
raw_data <- read_csv2(file.path(model_data_dir, "ts_data_raw.csv"),
                      show_col_types = FALSE)
sf_data  <- st_read(file.path(model_data_dir, "sf_data_raw.gpkg"), quiet = TRUE)

new_data <- read_csv2(file.path(model_data_dir, "ts_pred_data.csv"),
                      show_col_types = FALSE) |>
  dplyr::select(id,
                any_of(c("id_mine", "year", "country_code")),
                all_of(all_vars),
                any_of(c("raw_water", "total_water",
                          "raw_water_intensity", "total_water_intensity"))) |>
  dplyr::mutate_if(is.character, as.factor) |>
  filter(production > 0, ore_grade > 0)

# Track which rows have observed (non-imputed) raw_water before filling
rw_observed_mask <- !is.na(new_data$raw_water)

################################################################################
### Step 1: Predict raw water with log10 uncertainty
# raw_water is renamed 'target' to match the RW recipe's outcome role.
rw_pred <- predict_intervals_rf(
  object = rw_model,
  df     = dplyr::rename(new_data, target = raw_water)
) |>
  transmute(
    rw_pred            = predicted,
    rw_pred_sd         = predicted_sd,
    rw_pred_sd_log10   = predicted_sd_log10
  )

### Step 2: Fill missing raw_water with model predictions
# A flag tracks whether TW uncertainty will need propagation (imputed RW).
new_data_filled <- new_data |>
  bind_cols(rw_pred) |>
  dplyr::mutate(
    raw_water             = ifelse(is.na(raw_water), rw_pred, raw_water),
    tw_uncertainty_source = ifelse(rw_observed_mask, "observed_rw", "imputed_rw")
  )

### Step 3: Predict total water with log10 uncertainty
# A placeholder target column (NA) is added so the TW recipe can find its
# outcome variable; observed total_water values are not used for prediction.
tw_pred <- predict_intervals_rf(
  object = tw_model,
  df     = dplyr::mutate(new_data_filled, target = NA_real_)
) |>
  transmute(
    tw_pred            = predicted,
    tw_pred_sd         = predicted_sd,
    tw_pred_sd_log10   = predicted_sd_log10
  )

### Step 4: Propagate RW uncertainty into TW for imputed-RW rows
# For mines where raw_water was missing, the TW model's input (raw_water) was
# itself uncertain. We propagate this uncertainty via addition in quadrature in
# log10 space, assuming ~unit coefficient of log10(RW) in the TW model.
# For mines with observed RW, TW uncertainty comes from the TW model alone.
predictions <- bind_cols(
    select(new_data, id, raw_water, total_water),
    rw_pred,
    tw_pred,
    tibble(tw_uncertainty_source = new_data_filled$tw_uncertainty_source)
  ) |>
  mutate(
    tw_sd_total_log10 = ifelse(
      tw_uncertainty_source == "imputed_rw",
      sqrt(tw_pred_sd_log10^2 + rw_pred_sd_log10^2),
      tw_pred_sd_log10
    ),
    # 95% prediction intervals in original scale (ML)
    rw_pi_lower = 10^(log10(rw_pred) - 1.96 * rw_pred_sd_log10),
    rw_pi_upper = 10^(log10(rw_pred) + 1.96 * rw_pred_sd_log10),
    tw_pi_lower = 10^(log10(tw_pred) - 1.96 * tw_sd_total_log10),
    tw_pi_upper = 10^(log10(tw_pred) + 1.96 * tw_sd_total_log10)
  )

################################################################################
### Coverage diagnostics
n_total   <- nrow(predictions)
n_rw_obs  <- sum(!is.na(predictions$raw_water))
n_tw_obs  <- sum(!is.na(predictions$total_water))
n_rw_pred <- sum(!is.na(predictions$rw_pred))
n_tw_pred <- sum(!is.na(predictions$tw_pred))
n_imputed <- sum(predictions$tw_uncertainty_source == "imputed_rw", na.rm = TRUE)

message(sprintf(
  "Coverage: %d/%d rows have RW prediction (%.1f%%)  [%d observed + %d imputed]",
  n_rw_pred, n_total, 100 * n_rw_pred / n_total,
  n_rw_obs, n_rw_pred - n_rw_obs
))
message(sprintf(
  "Coverage: %d/%d rows have TW prediction (%.1f%%)  [%d with propagated uncertainty]",
  n_tw_pred, n_total, 100 * n_tw_pred / n_total, n_imputed
))
if (n_rw_pred < n_total) {
  missing_rw <- new_data[is.na(predictions$rw_pred), ] |>
    dplyr::count(ore_body_group, process_route, mine_type) |>
    dplyr::arrange(desc(n))
  message("RW: unpredicted rows by feature combination:")
  print(missing_rw)
}

################################################################################
### Build filled dataset joined with raw metadata
predictions_filled <- predictions |>
  # Drop raw_water / total_water here to avoid column-name conflicts after the
  # left_join with raw_data (which supplies those same columns with metadata).
  dplyr::select(id,
                rw_pred, rw_pred_sd, rw_pred_sd_log10,
                rw_pi_lower, rw_pi_upper,
                tw_pred, tw_pred_sd, tw_pred_sd_log10,
                tw_sd_total_log10, tw_pi_lower, tw_pi_upper,
                tw_uncertainty_source) |>
  left_join(raw_data, by = "id") |>
  dplyr::mutate(
    rw_filled = ifelse(is.na(raw_water), rw_pred, raw_water),
    tw_filled = ifelse(is.na(total_water), tw_pred, total_water)
  )

# Global totals
message("Global totals (Bm3):")
print(predictions_filled |>
  dplyr::transmute(rw_filled, tw_filled) |>
  dplyr::summarise(across(everything(), \(x) sum(x, na.rm = TRUE)) * 1e-6))

################################################################################
### Performance evaluation against available reference values
predictions |>
  dplyr::select(id, raw_water, total_water, rw_pred, tw_pred) |>
  mutate(rw_err = raw_water - rw_pred, rw_abs_err = abs(rw_err), rw_sq_err = rw_err^2,
         tw_err = total_water - tw_pred, tw_abs_err = abs(tw_err), tw_sq_err = tw_err^2) |>
  summarise(
    rw_mean.ref    = log10(mean(raw_water,  na.rm = TRUE)),
    rw_mean.pred   = mean(rw_pred,          na.rm = TRUE),
    rw_median.ref  = median(raw_water,      na.rm = TRUE),
    rw_median.pred = median(rw_pred,        na.rm = TRUE),
    rw_rmse.pred   = sqrt(mean(rw_sq_err,   na.rm = TRUE)),
    rw_mae.pred    = mean(rw_abs_err,       na.rm = TRUE),
    tw_mean.ref    = mean(total_water,      na.rm = TRUE),
    tw_mean.pred   = mean(tw_pred,          na.rm = TRUE),
    tw_median.ref  = median(total_water,    na.rm = TRUE),
    tw_median.pred = median(tw_pred,        na.rm = TRUE),
    tw_rmse.pred   = sqrt(mean(tw_sq_err,   na.rm = TRUE)),
    tw_mae.pred    = mean(tw_abs_err,       na.rm = TRUE)
  ) |>
  pivot_longer(cols = everything()) |>
  separate(name, into = c("variable", "metric", "type"), sep = "_|\\.") |>
  pivot_wider(names_from = type, values_from = value) |>
  select(metric, variable, ref, pred) |>
  print()

################################################################################
### Density plot: reference vs predicted distributions
png(filename = file.path(results_dir, "density_plot_final_predictions.png"),
    width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")

pred_long <- predictions |>
  dplyr::select(`New Water.Reference`   = raw_water,
                `Total Water.Reference` = total_water,
                `New Water.Predicted`   = rw_pred,
                `Total Water.Predicted` = tw_pred) |>
  pivot_longer(cols = everything(), names_to = c("Water", "Source"), names_sep = "\\.") |>
  drop_na() |>
  dplyr::mutate(value = value * 1e-3)  # ML -> Mm3

pred_means <- pred_long |>
  dplyr::group_by(Water, Source) |>
  dplyr::summarise(value = mean(value), .groups = "drop")

print(
  ggplot(pred_long, aes(x = value, color = Source, fill = Source)) +
    facet_wrap(Water ~ ., scales = "free_x") +
    geom_histogram(aes(y = after_stat(density)), position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.6, adjust = 1.5) +
    geom_vline(data = pred_means, aes(xintercept = value, color = Source),
               linetype = "dashed") +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual(values  = c("#999999", "#E69F00", "#56B4E9")) +
    labs(x = expression("Volume (Million " * phantom(" ") * m^3 * ")"), y = "Density") +
    theme_classic()
)
dev.off()

################################################################################
# Note: spatial attributes (freshwater_availability, aware_*, aqueduct_*) are
# already present in raw_data (ts_data_raw.csv) and were joined in the step
# above. No additional join from sf_data is needed for these columns.
################################################################################
####
####  BAYESIAN TREND MODEL  (Bayesian mixed model for mine-level slope CIs)
####
################################################################################

# Extract mine centroids in EPSG:3857 for Moran's I spatial test.
sf_coords <- sf_data |>
  st_transform(crs = 3857) |>
  mutate(
    lon = st_coordinates(geom)[, 1],
    lat = st_coordinates(geom)[, 2]
  ) |>
  st_drop_geometry() |>
  select(id_mine, lon, lat)

# Prepare mine-level time-series data.
# Only mines with > 2 years of data are included (minimum for slope estimation).
# Uses rw_filled (observed when available, predicted otherwise), in Mm3.
mine_data <- predictions_filled |>
  left_join(sf_coords, by = "id_mine") |>
  mutate(
    raw_water_mm3 = rw_filled * 1e-3,   # ML -> Mm3
    raw_water_log = log(raw_water_mm3),  # log-normal assumption for brm
    year_c        = year - 2017          # centre at midpoint for better mixing
  ) |>
  drop_na(raw_water_mm3, lon, lat) |>
  group_by(id_mine) |>
  mutate(n_years = n()) |>
  ungroup() |>
  filter(n_years > 2)

message(sprintf("Trend analysis: %d mine-year observations, %d mines",
                nrow(mine_data), n_distinct(mine_data$id_mine)))

# ---- Spatial autocorrelation (Moran's I) ----
# Moran's I requires production, so use the subset of mine_data with non-NA
# production. This keeps the Bayesian trend model data (mine_data) broader.
moran_data <- mine_data |> drop_na(production)

# Jitter coordinates slightly to avoid identical-coordinate errors in knn2nb.
coords <- moran_data |>
  transmute(
    lon = jitter(lon, amount = 1e-5),
    lat = jitter(lat, amount = 1e-5)
  )
nb    <- knn2nb(knearneigh(coords, k = 5))
listw <- nb2listw(nb)

moran_results <- list(
  raw_water     = moran.test(moran_data$raw_water_mm3,  listw),
  raw_water_log = moran.test(moran_data$raw_water_log,  listw),
  production    = moran.test(moran_data$production,     listw)
)

moran_table <- do.call(rbind, lapply(names(moran_results), function(v) {
  t <- moran_results[[v]]
  data.frame(variable    = v,
             moran_I     = round(t$statistic, 4),
             p_value     = signif(t$p.value, 3),
             expectation = round(t$estimate["Expectation"], 5))
}))
message("Moran's I test for spatial autocorrelation:")
print(moran_table)
write_csv(moran_table, file.path(results_dir, "morans_i_results.csv"))

# ---- Bayesian mixed-effects model ----
# Model: log(raw_water) ~ year_c + (year_c | id_mine)
#
# Rationale:
#   - Random slope on year_c allows each mine to have its own trend.
#   - Log scale is appropriate for right-skewed water volumes.
#   - AR(1) autocorrelation was removed: with only 5 time points per mine
#     the parameter is not identifiable, producing a multimodal posterior
#     that prevents convergence and contaminates slope estimates.
#
# Priors: weakly informative brms defaults (normal(0,1) on slopes after
# standardisation, half-Student-t on variance components).
if (file.exists(bayes_cache)) {
  message("Loading Bayesian model from cache: ", bayes_cache)
  fit_bayes <- read_rds(bayes_cache)
} else {
  message("Fitting Bayesian mixed model (this may take 10-30 minutes)...")
  fit_bayes <- brm(
    raw_water_log ~ year_c + (year_c | id_mine),
    data    = mine_data,
    family  = gaussian(),
    chains  = 4,
    cores   = 4,
    iter    = 4000,
    warmup  = 1000,
    seed    = 123,
    control = list(
      adapt_delta   = 0.95
    )
  )
  write_rds(fit_bayes, file.path(results_dir, "fit_bayes_trends.rds"))
  message("Model saved to: ", file.path(results_dir, "fit_bayes_trends.rds"))
}

message("Bayesian model summary:")
print(summary(fit_bayes))

# ---- Model diagnostics ----
# R-hat: all chains should have R-hat < 1.01 (convergence criterion).
# pp_check: posterior predictive samples vs. observed distribution.

png(file.path(results_dir, "bayes_rhat.png"),
    width = 200, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
print(bayesplot::mcmc_rhat(rhat(fit_bayes)) +
      labs(title = "R-hat diagnostics (convergence check; all should be < 1.01)"))
dev.off()

png(file.path(results_dir, "bayes_ppcheck.png"),
    width = 200, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
print(pp_check(fit_bayes, type = "dens_overlay") +
      labs(title = "Posterior predictive check"))
dev.off()

png(file.path(results_dir, "bayes_trace.png"),
    width = 280, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
print(plot(fit_bayes, ask = FALSE))
dev.off()

# ---- Extract mine-level slopes ----
# Population-level slope (b_year_c) + mine-specific deviation (r_id_mine[,year_c])
# = mine-specific annual trend in log(raw_water_mm3).
draws <- fit_bayes |>
  spread_draws(b_year_c, r_id_mine[id_mine, year_c]) |>
  mutate(slope = b_year_c + r_id_mine)

trend_by_mine <- draws |>
  group_by(id_mine) |>
  summarise(
    rw_median_slope      = median(slope),
    rw_ci_lower          = quantile(slope, 0.025),
    rw_ci_upper          = quantile(slope, 0.975),
    rw_prob_gt0          = mean(slope > 0),
    rw_prob_lt0          = mean(slope < 0),
    ci_width             = rw_ci_upper - rw_ci_lower,
    # 0 = certain direction, 1 = maximally uncertain (equal prob +/-)
    rw_slope_uncertainty = 1 - abs(mean(slope > 0) - 0.5) * 2,
    .groups = "drop"
  )

# ---- Slope distribution plot ----
png(file.path(results_dir, "trend_slope_distribution.png"),
    width = 200, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
print(
  ggplot(trend_by_mine, aes(x = rw_prob_gt0)) +
    geom_histogram(bins = 30, fill = "steelblue", colour = "white", alpha = 0.8) +
    geom_vline(xintercept = 0.5, linetype = "dashed", colour = "firebrick") +
    labs(x = "P(slope > 0)", y = "Number of mines",
         title = "Probability of increasing raw water trend by mine") +
    theme_minimal()
)
dev.off()

# ---- Mine-level slope caterpillar plot ----
png(file.path(results_dir, "trend_slope_caterpillar.png"),
    width = 280, height = 180, units = "mm", pointsize = 12, res = 300, bg = "white")
print(
  trend_by_mine |>
    arrange(rw_median_slope) |>
    mutate(rank = row_number()) |>
    ggplot(aes(x = rank, y = rw_median_slope)) +
    geom_point(size = 0.8, alpha = 0.6) +
    geom_errorbar(aes(ymin = rw_ci_lower, ymax = rw_ci_upper,
                      colour = rw_slope_uncertainty),
                  width = 0, linewidth = 0.3, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "firebrick") +
    scale_colour_viridis_c(
      name   = "Directional uncertainty\n(0=certain, 1=uncertain)",
      option = "plasma"
    ) +
    labs(x = "Mine (ranked by slope)", y = "Annual trend in log(raw water)",
         title = "Mine-level raw water use trends (2015\u20132019)") +
    theme_minimal()
)
dev.off()

################################################################################
### Join slopes and coordinates to predictions_filled
# Only mines with > 2 years of data receive Bayesian slope estimates;
# all other mines will have NA in slope columns.
predictions_filled <- predictions_filled |>
  left_join(trend_by_mine, by = "id_mine") |>
  left_join(sf_coords,     by = "id_mine")

################################################################################
### Save final_predictions.csv
write_csv2(predictions_filled, file.path(results_dir, "final_predictions.csv"))
message("final_predictions.csv saved to: ", results_dir)

library(viridis)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(s2)
library(readxl)
library(caret)
library(ggplot2)
library(scales)
library(terra)
library(tune)
library(readr)
library(rsample)
library(recipes)
library(RSQLite)
library(DBI)
library(randomForest)
library(RANN)
library(gridExtra)
library(lattice)
library(recipes)
library(Metrics)
library(themis)
library(embed)
library(patchwork)
library(spgwr)
library(xml2)
library(rvest)
library(nlme)
library(broom.mixed)
library(spdep)
library(spatialreg)
library(knitr)
library(lme4)
library(spaMM)
library(splm)
library(plm)
library(elasticnet)
library(gbm)
library(bayesplot)
library(brms)
library(kernlab)
library(quantregForest)

dir.create("./data", showWarnings = FALSE, recursive = TRUE)
dir.create("./results", showWarnings = FALSE, recursive = TRUE)

training_oversampling_quantile <- function(df, outcome, over_ratio = 0.5) {

  # Check initial class distribution
  Q1 <- quantile(df[[outcome]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[outcome]], 0.75, na.rm = TRUE)
  IQR <- IQR(df[[outcome]], na.rm = TRUE)

  # Get training data and create quantile distribution classes
  df <- df |>
    mutate(q_class = case_when(
      df[[outcome]] < (Q1 - 1.5 * IQR) ~ "very_low",
      df[[outcome]] >= (Q1 - 1.5 * IQR) & df[[outcome]] < Q1 ~ "low",
      df[[outcome]] >= Q1 & df[[outcome]] < Q3 ~ "medium",
      df[[outcome]] >= Q3 & df[[outcome]] < (Q3 + 1.5 * IQR) ~ "high",
      df[[outcome]] >= (Q3 + 1.5 * IQR) ~ "very_high"
    ))

  # Data augmentation by oversampling underrepresented quantile
  recipe <- recipe(q_class ~ ., data = df) |>
    step_smotenc(q_class, over_ratio = over_ratio) |>
    prep()

  # Return the baked data
  bake(recipe, new_data = NULL)
}

# Function to parse HTML and extract specific values
extract_aware_values <- function(html_text) {
  page <- read_html(html_text)
  keys <- c("Consumption_m3", "Annual agri", "Annual non-agri", "Annual unknown")
  
  # Extract all values in one pass
  values <- page %>%
    html_nodes(xpath = "//tr[td]/td") %>%
    html_text(trim = TRUE)
  
  # Create a named vector with extracted values
  result <- sapply(keys, function(key) {
    match_idx <- which(values == key)
    if (length(match_idx) > 0 && (match_idx + 1) <= length(values)) {
      as.numeric(values[match_idx + 1])
    } else {
      NA
    }
  })
  
  return(as.list(result))
}

training_oversampling_quantile <- function(df, outcome, over_ratio = 0.5) {

  # Check initial class distribution
  Q1 <- quantile(df[[outcome]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[outcome]], 0.75, na.rm = TRUE)
  IQR <- IQR(df[[outcome]], na.rm = TRUE)

  # Get training data and create quantile distribution classes
  df <- df |>
    mutate(q_class = case_when(
      df[[outcome]] < (Q1 - 1.5 * IQR) ~ "very_low",
      df[[outcome]] >= (Q1 - 1.5 * IQR) & df[[outcome]] < Q1 ~ "low",
      df[[outcome]] >= Q1 & df[[outcome]] < Q3 ~ "medium",
      df[[outcome]] >= Q3 & df[[outcome]] < (Q3 + 1.5 * IQR) ~ "high",
      df[[outcome]] >= (Q3 + 1.5 * IQR) ~ "very_high"
    ))

  # Data augmentation by oversampling underrepresented quantile
  recipe <- recipe(q_class ~ ., data = df) |>
    step_smotenc(q_class, over_ratio = over_ratio) |>
    prep()

  # Return the baked data
  bake(recipe, new_data = NULL)
}

training_oversampling_high_quantile <- function(df, outcome, prob = .75, over_ratio = 0.5) {

  # Calculate the third quartile
  out_q <- quantile(df[[outcome]], prob, na.rm = TRUE)

  # Get training data and create binary high-value class
  df <- df |>
    mutate(q_class = ifelse(df[[outcome]] >= out_q, "high", "low"))

  # Data augmentation by oversampling underrepresented quantile
  recipe <- recipe(q_class ~ ., data = df) |>
    step_smotenc(q_class, over_ratio = over_ratio) |>
    prep()

  # Return the baked data
  bake(recipe, new_data = NULL)

}

make_water_use_grid <- function(data, cell_size=50000){
  
  calculate_slope <- function(x, y) {
  if (length(y) < 2) {
    return(NA) # Not enough points to calculate a slope
  }
    model <- lm(y ~ x)
    return(coef(model)[2]) # Return the slope
  }

  mine_features_points <- data |> 
    mutate(geom = st_centroid(geom))
  
  mine_features_points_goode <- 
    st_transform(mine_features_points, crs = "+proj=igh +ellps=WGS84 +units=m +no_defs")
  
  grid_50 <- st_make_grid(mine_features_points_goode, cellsize = cell_size) |> 
    st_as_sf() |> 
    st_filter(mine_features_points_goode) |>
    dplyr::mutate(id_grid = row_number())

  slope <- st_join(mine_features_points_goode, grid_50) |>
    arrange(id_mine, year) |>
    group_by(id_grid) |>
    summarize(
      slope_total_water = calculate_slope(year, total_water),
      slope_raw_water = calculate_slope(year, raw_water),
      .groups = 'drop' # drop the grouping to return a regular dataframe
    ) |>
    st_drop_geometry() 

  out <- mine_features_points_goode |>
    pivot_wider(names_from = year, values_from = c(total_water, raw_water), id_cols = c(id_mine, geom), names_sep = ".") |>
    select(-c(id_mine)) |>
    aggregate(grid_50, sum, na.rm = TRUE) |>
    bind_cols(slope)

  return(out)
  
}

# adapted from https://wilkelab.org/practicalgg/articles/goode.html
plot_goode_homolosine_world_map <- function(ocean_color = "#56B4E950", 
                                            land_color = "gray95",
                                            grid_color = "grey60",
                                            grid_size = 0.1,
                                            country_borders_color = "grey60",
                                            country_borders_size = 0.1,
                                            family = "sans"){
  
  crs_goode <- "+proj=igh +ellps=WGS84 +units=m +no_defs"
  
  world_sf <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) |> 
    sf::st_transform(crs = crs_goode)
  
  # projection outline in long-lat coordinates
  lats <- c(
    90:-90, # right side down
    -90:0, 0:-90, # third cut bottom
    -90:0, 0:-90, # second cut bottom
    -90:0, 0:-90, # first cut bottom
    -90:90, # left side up
    90:0, 0:90, # cut top
    90 # close
  )
  longs <- c(
    rep(180, 181), # right side down
    rep(c(80.01, 79.99), each = 91), # third cut bottom
    rep(c(-19.99, -20.01), each = 91), # second cut bottom
    rep(c(-99.99, -100.01), each = 91), # first cut bottom
    rep(-180, 181), # left side up
    rep(c(-40.01, -39.99), each = 91), # cut top
    180 # close
  )
  
  goode_outline <- 
    list(cbind(longs, lats)) |>
    sf::st_polygon() |>
    sf::st_sfc(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") |> 
    sf::st_transform(crs = crs_goode)
  
  # get the bounding box in transformed coordinates and expand by 10%
  xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
  ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1
  
  # turn into enclosing rectangle
  goode_encl_rect <- 
    list(
      cbind(
        c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
        c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
      )
    ) |>
    sf::st_polygon() |>
    sf::st_sfc(crs = crs_goode)
  
  goode_without <- sf::st_difference(goode_encl_rect, goode_outline)
  
  y_grid <- lapply(seq(-80, 90, 20)   + 2, function(y) sf::st_point(c(longitude = -175, latitude =   y)))
  x_grid <- lapply(seq(-150, 150, 50) - 9, function(x) sf::st_point(c(longitude =    x, latitude = -62)))
  
  # goode_grid <- sf::st_as_sfc(c(y_grid, x_grid), crs = "+proj=longlat +datum=WGS84 +no_defs") |> 
  #   sf::st_as_sf() |> 
  #   dplyr::mutate(label = c(paste0(abs(seq(-80, 90, 20)),   "°", c(rep("S", 4), "", rep("N", 4))), 
  #                           paste0(abs(seq(-160, 180, 20)), "°", c(rep("W", 8), "", rep("E", 8))))) |> 
  #   sf::st_transform(crs = crs_goode)
  
  goode_grid <- sf::st_as_sfc(c(y_grid, x_grid), crs = "+proj=longlat +datum=WGS84 +no_defs") |> 
    sf::st_as_sf() |> 
    dplyr::mutate(label = c(paste0(abs(seq(-80, 90, 20)),   "°", c(rep("S", 4), "", rep("N", 4))), 
                            paste0(abs(seq(-150, 150, 50)), "°", c(rep("W", 3), "", rep("E", 3))))) |> 
    sf::st_transform(crs = crs_goode)
  
  gp_map <- ggplot(world_sf) + 
    cowplot::theme_minimal_grid() +
    geom_sf(fill = land_color, color = country_borders_color, size = country_borders_size/.pt) +
    geom_sf(data = goode_without, fill = "white", color = "NA") +
    coord_sf(crs = crs_goode, xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE) +
    geom_sf_text(data = goode_grid, colour = grid_color, size = 3, family = family,
                 mapping = aes(label = label)) + 
    theme(
      axis.title = element_blank(),
      panel.background = element_rect(fill = ocean_color, color = NA, linewidth = 2),
      panel.grid.major = element_line(color = grid_color, linewidth = grid_size)
    )
  gp_map
  return(gp_map)
  
}

# Function to format numeric columns to scientific notation with 3 decimals
format_to_sci <- function(x) {
  if(is.numeric(x)) {
    return(sprintf("%.3e", x))
  } else {
    return(x)
  }
}

calc_slope <- function(x) {
    if (all(is.na(x))) {
        NA_real_  # Return NA if all values are NA
    } else {
        times <- 1:length(x)  # Assuming equal time intervals
        coef(lm(x ~ times))[2]  # Extract the slope (coefficient of times)
    }
}

prepare_new_data <- function(object, newdata, na.action = na.omit){
  newdata <- as.data.frame(newdata)
  rn <- row.names(newdata)
  Terms <- delete.response(object$terms)
  m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses")))  .checkMFClasses(cl, m)
  keep <- match(row.names(m), rn)
  newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
  xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
  if (xint > 0) newdata <- newdata[, -xint, drop = FALSE]
  predict(object$preProcess, newdata)
}

# Function to revert transformations applied to the independent variable
revert_transformations <- function(rec, values) {
  
  transformed_values <- values
  
  for (step in rev(rec$steps)) {
    #step = rev(rec$steps)[[6]]
    step_class <- class(step)[1]
    if (rec$var_info$variable[rec$var_info$role=="outcome"] %in% step$columns) {
      if (step_class == "step_log") {
        base <- step$base
        transformed_values <- base ^ transformed_values
        
      } else if (step_class == "step_normalize") {
        transformed_values <- transformed_values * step$sds + step$means
      # # Add other transformations and their reverses as needed
      } 
    }
  }
  
  return(transformed_values)
}

predict_intervals_rf <- function(object, df, individuals = FALSE){

  df_prep <- prep(object$recipe)

  pred_data <- dplyr::select(df, object$recipe$var_info$variable) |>
    bake(df_prep, new_data = _) |>
    select(object$recipe$term_info$variable[object$recipe$term_info$role!="outcome"]) |>
    drop_na()

  preds <- stats::predict(object$finalModel, newdata = pred_data, predict.all = TRUE)

  prediction <- revert_transformations(object$recipe, preds$aggregate)
  prediction_sd <- apply(preds$individual, 1, function(x) sd(revert_transformations(object$recipe, x)))

  # if (is.null(log_base)) {
  #   prediction <- preds$aggregate
  #   prediction_sd <- apply(preds$individual, 1, sd)
  # } else {
  #   prediction <- log_base^preds$aggregate
  #   prediction_sd <- apply(preds$individual, 1, function(x) sd(log_base^x))
  # }

  out <- tibble(predicted = prediction, predicted_sd = prediction_sd) 

  if (individuals) {
    out$individual <- lapply(1:nrow(preds$individual), function(i) 
        if (is.null(log_base)) {
          preds$individual[i,,drop=TRUE]
        } else { 
          log_base^preds$individual[i,,drop=TRUE]
        })
  }

  return(out) 

}

propagate_pred_error_rf <- function(rf1, rf2, df, n = 100, rf1_log_base = NULL, rf2_log_base = NULL){

  # Simulate inputs based on RF distribution, generate outputs and intervals from LM
  # To predict total water use known raw_water and fill gaps with raw_water predictions
  # It error propagates oly for points where raw_water is predicted otherwise the error is only due to the total water model uncertainty
  rf1_preds <- predict_intervals_rf(object = rf1, df)

  # Extract the name of the independent variable
  rf1_independent_var <- rf1$recipe$term_info |> 
    filter(role == "outcome") |> 
    pull(variable)

  rf2_results <- lapply(1:n, function(i){

    # Sample once per observation across RF's tree predictions
    #sampled_rf1_outputs <- sapply(rf1_preds$individual, sample, size = 1)
    sampled_rf1_outputs <- rnorm(nrow(df), rf1_preds$predicted, rf1_preds$predicted_sd)
    if (!is.null(rf1_log_base)){
      sampled_rf1_outputs <- rf1_log_base^sampled_rf1_outputs
    }

    # Update df with predicted raw_water
    pred_data <- df
    pred_data[[rf1_independent_var]] <- ifelse(is.na(pred_data[[rf1_independent_var]]), sampled_rf1_outputs, pred_data[[rf1_independent_var]])

    predict_intervals_rf(object = rf2, pred_data, log_base = rf2_log_base) |>
      dplyr::select(predicted, predicted_sd)

  })
  
  # Aggregating the results
  rf2_aggregated_pred <- do.call(cbind, lapply(rf2_results, function(x) x$predicted))
  rf2_aggregated_sd <- do.call(cbind, lapply(rf2_results, function(x) x$predicted_sd))

  # Calculate mean and quantiles for each observation
  predictions <- apply(rf2_aggregated_pred, 1, mean)
  predicted_sd <- apply(rf2_aggregated_sd, 1, function(x) quantile(x, 0.025, na.rm = TRUE))

  # Combine into a dataframe
  data.frame(
    df,
    rf1_pred = rf1_preds$predicted,
    rf1_sd = rf1_preds$predicted_sd,
    rf2_pred = predictions,
    rf2_sd = predicted_sd
  ) |> as_tibble()

}

predict_transform <- function(m, newdata) {
  if ( str_detect(all.vars(formula(m))[1], "log") ) return(exp(predict(m, newdata)))
  if ( str_detect(all.vars(formula(m))[1], "log1p") ) return(expm1(predict(m, newdata)))
  return(predict(m, newdata))
}

summary_metrics_inverse_log <- function(data, lev = NULL, model = NULL) {
  
  # Exponentiate the predictions and observed values using base 10
  exp_pred <- 10^data$pred
  exp_obs <- 10^data$obs
  
  # Calculate RMSE and MAE on the original scale
  rmse_val <- sqrt(mean((exp_obs - exp_pred)^2))
  mae_val <- mean(abs(exp_obs - exp_pred))
  r2_val <- cor(exp_obs, exp_pred)^2
  
  # Return the metrics
  out <- c(Rsquared = r2_val, RMSE = rmse_val, MAE = mae_val)
  return(out)

}



metrics_log_models <- function(data, lev = NULL, model = NULL) {

    # Calculate default summary statistics (MAE, RMSE, R^2)
    stats <- defaultSummary(data.frame(obs = exp(data$obs), pred = exp(data$pred)))

    # Return all the stats; RMSE will be used for optimization if specified in trainControl
    return(stats)
}

metrics_log1p_models <- function(data, lev = NULL, model = NULL) {

    # Calculate default summary statistics (MAE, RMSE, R^2)
    stats <- defaultSummary(data.frame(obs = expm1(data$obs), pred = expm1(data$pred)))

    # Return all the stats; RMSE will be used for optimization if specified in trainControl
    return(stats)
}

create_model_comparison_csv <- function(comparison_table) {
  
  # Define a function to add confidence levels
  add_confidence_levels <- function(p_value) {
    if (grepl("<", p_value)) {  # Check if the value is a "< e-16" type string
      return("***")
    }
    
    p_value <- as.numeric(p_value)
    if (is.na(p_value) || p_value == "") {
      return("")
    } else if (p_value < 0.001) {
      return("***")
    } else if (p_value < 0.01) {
      return("**")
    } else if (p_value < 0.05) {
      return("*")
    } else if (p_value < 0.1) {
      return(".")
    } else {
      return("")
    }
  }
  
  # Process each metric
  format_table <- function(metric_name, metric_data) {
    df <- as.data.frame(metric_data)
    df <- cbind(Metric = metric_name, Model = row.names(df), df)
    row.names(df) <- NULL
    
    # Apply confidence levels only to the lower triangle (p-values)
    for (i in seq_len(nrow(df))) {
      for (j in seq_len(ncol(df) - 2)) {  # Adjusted loop range to match column indexing
        if (i > j) {  # Check if it's the lower triangle
          df[i, j + 2] <- paste(df[i, j + 2], add_confidence_levels(df[i, j + 2]))
        }
      }
    }
    
    return(df)
  }
  
  # Apply the function to each metric in the list
  mae_df <- format_table("MAE", comparison_table$MAE)
  rmse_df <- format_table("RMSE", comparison_table$RMSE)
  rsquared_df <- format_table("Rsquared", comparison_table$Rsquared)
  
  # Combine all metrics into a single data frame
  combined_df <- rbind(mae_df, rmse_df, rsquared_df)
  
  return(combined_df)
}

get_resamples_stats <- function(models) {
  resamps <- resamples(models)
  resamps_stats <- summary(resamps)$statistics
  resamps_stats <- do.call("rbind", lapply(names(resamps_stats), function(i) data.frame(Metric = i, Model = row.names(resamps_stats[[i]]), resamps_stats[[i]][,-7])))
  rownames(resamps_stats) <- NULL
  return(resamps_stats)
}

plot_models_performance <- function(models){
  resamps <- resamples(models)
  theme1 <- trellis.par.get()
  theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
  theme1$plot.symbol$pch = 16
  theme1$plot.line$col = rgb(1, 0, 0, .7)
  theme1$plot.line$lwd <- 2
  trellis.par.set(theme1)
  bwplot(resamps, layout = c(3, 1), scales = list(x = "free"))
}

round_numeric_columns <- function(df, digits) {
  mutate(df, across(where(is.numeric), ~ format(round(., digits), scientific = FALSE)))
}

custom_preprocess <- function(data, method = c("center", "scale", "nzv")) {

  # Identify numeric columns
  numeric_columns <- sapply(data, is.numeric)
  
  # Apply preprocessing to the numeric columns
  pre_process <- preProcess(data[, numeric_columns], method = method)
  data[, numeric_columns] <- predict(pre_process, data[, numeric_columns])

  # Create dummy variables from factors
  data <- dummyVars(~ ., data = data, fullRank = TRUE) |>
    predict(newdata = data) |>
    as_tibble()
  
  return(data)

}

# Define the function to create stratified bootstrapped indices
createStratifiedBootstrap <- function(data, strat_var, n) {
  strata <- unique(data[[strat_var]])
  boot_indices <- lapply(1:n, function(i) {
    unlist(lapply(strata, function(s) {
      idx <- which(data[[strat_var]] == s)
      sample(idx, length(idx), replace = TRUE)
    }))
  })
  return(boot_indices)
}


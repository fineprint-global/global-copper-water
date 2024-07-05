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

dir.create("./data", showWarnings = FALSE, recursive = TRUE)
dir.create("./results", showWarnings = FALSE, recursive = TRUE)

# adapted from https://wilkelab.org/practicalgg/articles/goode.html
plot_goode_homolosine_world_map <- function(ocean_color = "#56B4E950", 
                                            land_color = "gray95",
                                            grid_color = "grey60",
                                            grid_size = 0.1,
                                            country_borders_color = "grey60",
                                            country_borders_size = 0.1,
                                            family = "sans"){
  
  crs_goode <- "+proj=igh +ellps=WGS84 +units=m +no_defs"
  
  world_sf <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
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
    list(cbind(longs, lats)) %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
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
    ) %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = crs_goode)
  
  goode_without <- sf::st_difference(goode_encl_rect, goode_outline)
  
  y_grid <- lapply(seq(-80, 90, 20)   + 2, function(y) sf::st_point(c(longitude = -175, latitude =   y)))
  x_grid <- lapply(seq(-150, 150, 50) - 9, function(x) sf::st_point(c(longitude =    x, latitude = -62)))
  
  # goode_grid <- sf::st_as_sfc(c(y_grid, x_grid), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  #   sf::st_as_sf() %>% 
  #   dplyr::mutate(label = c(paste0(abs(seq(-80, 90, 20)),   "°", c(rep("S", 4), "", rep("N", 4))), 
  #                           paste0(abs(seq(-160, 180, 20)), "°", c(rep("W", 8), "", rep("E", 8))))) %>% 
  #   sf::st_transform(crs = crs_goode)
  
  goode_grid <- sf::st_as_sfc(c(y_grid, x_grid), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(label = c(paste0(abs(seq(-80, 90, 20)),   "°", c(rep("S", 4), "", rep("N", 4))), 
                            paste0(abs(seq(-150, 150, 50)), "°", c(rep("W", 3), "", rep("E", 3))))) %>% 
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

predict_intervals_rf <- function(object, df, alpha = 0.05, log_base = NULL){

  # alpha = 0.05 for a 95% prediction interval
  pred_data <- bake(prep(object$recipe), df) |>
    drop_na(any_of(prep(object$recipe)$term_info$variable[prep(object$recipe)$term_info$role=="predictor"])) |>
    select(-prep(object$recipe)$term_info$variable[prep(object$recipe)$term_info$role=="outcome"])

  preds <- predict(object$finalModel, newdata = pred_data, predict.all = TRUE)

  if (is.null(log_base)) {
    prediction <- preds$aggregate
    prediction_sd <- apply(preds$individual, 1, sd)
  } else {
    prediction <- log_base^preds$aggregate
    prediction_sd <- apply(preds$individual, 1, function(x) sd(log_base^x))
  }

  data.frame(
    predicted = prediction,
    lower = pmax(prediction - qnorm(1 - alpha / 2) * prediction_sd, 0),
    upper = prediction + qnorm(1 - alpha / 2) * prediction_sd) |> 
    as_tibble() |>
    dplyr::mutate(individual = lapply(1:nrow(preds$individual), function(i) 
        if (is.null(log_base)) {
          preds$individual[i,,drop=TRUE]
        } else { 
          log_base^preds$individual[i,,drop=TRUE]
        })) 

}

predict_intervals_lm <- function(object, df, alpha = 0.05){

  fit <- predict(object$finalModel, newdata = bake(prep(object$recipe), df), se.fit = TRUE, interval = "prediction", level = 0.95)
  
  # Extract standard errors
  pred <- fit$fit[,'fit']
  pred_se <- fit$se.fit

  # Compute prediction intervals
  t_value <- qt(1 - alpha/2, df = object$finalModel$df.residual)  # T-distribution critical value

  lower_bound <- pred - t_value * pred_se
  upper_bound <- pred + t_value * pred_se

  data.frame(df,
    predicted = pred,
    lower = lower_bound,
    upper = upper_bound) |> as_tibble()

}

propagate_pred_error_rf <- function(rf1, rf2, df, n = 100, alpha = 0.05, rf1_log_base = NULL, rf2_log_base = NULL){


  rf1_preds <- predict_intervals_rf(object = rf1, df, alpha = alpha, log_base = rf1_log_base)
  
  # Simulate inputs based on RF distribution, generate outputs and intervals from LM
  # To predict total water use known raw_water and fill gaps with raw_water predictions
  # It error propagates oly for points where raw_water is predicted otherwise the error is only due to the total water model uncertainty
  rf2_results <- lapply(1:n, function(i){

    # Sample once per observation across RF's tree predictions
    sampled_rf1_outputs <- sapply(rf1_preds$individual, function(x) sample(x, size = 1))
    
    # Update df with predicted raw_water
    pred_data <- df |>
      dplyr::mutate(rf1_pred = sampled_rf1_outputs, raw_water = ifelse(is.na(raw_water), rf1_pred, raw_water)) |>
      dplyr::select(-rf1_pred)

    predict_intervals_rf(object = rf2, pred_data, alpha = alpha, log_base = rf2_log_base) |>
      dplyr::select(predicted, lower, upper)

  })
  
  # Aggregating the results
  rf2_aggregated_pred <- do.call(cbind, lapply(rf2_results, function(x) x$predicted))
  rf2_aggregated_lwr <- do.call(cbind, lapply(rf2_results, function(x) x$lower))
  rf2_aggregated_upp <- do.call(cbind, lapply(rf2_results, function(x) x$upper))

  # Calculate mean and quantiles for each observation
  predictions <- apply(rf2_aggregated_pred, 1, mean)
  lower_bounds <- apply(rf2_aggregated_lwr, 1, function(x) quantile(x, 0.025, na.rm = TRUE))
  upper_bounds <- apply(rf2_aggregated_upp, 1, function(x) quantile(x, 0.975, na.rm = TRUE))

  # Combine into a dataframe
  data.frame(
    df,
    rf1_pred = rf1_preds$predicted,
    rf1_lw = rf1_preds$lower,
    rf1_up = rf1_preds$upper,
    rf2_pred = predictions,
    rf2_lw = lower_bounds,
    rf2_up = upper_bounds
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
  bwplot(resamps, layout = c(3, 1), scales = list(x = "free"), strip = strip.custom(factor.levels = c("RMSE", "MAE", "Rsquared")))
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


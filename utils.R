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


predict_intervals_rf <- function(object, newdata, na.action = na.omit, alpha = 0.05){

  # alpha = 0.05 for a 95% prediction interval
  out_data <- newdata

  newdata <- as.data.frame(newdata)
  rn <- row.names(newdata)
  Terms <- delete.response(object$terms)
  m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses")))  .checkMFClasses(cl, m)
  keep <- match(row.names(m), rn)
  newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
  xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
  if (xint > 0) newdata <- newdata[, -xint, drop = FALSE]

  preds <- predict(object$finalModel, newdata, predict.all = TRUE)
  prediction_sd <- apply(preds$individual, 1, sd)

  alpha <- alpha 
  z_value <- qnorm(1 - alpha / 2)
  lower_bound <- preds$aggregate - z_value * prediction_sd
  upper_bound <- preds$aggregate + z_value * prediction_sd

  data.frame(out_data, 
    predicted = preds$aggregate,
    lower = lower_bound,
    upper = upper_bound) |> as_tibble()

}


predict_intervals_lm <- function(object, newdata, alpha = 0.05){

  # alpha = 0.05 for a 95% prediction interval
  out_data <- newdata

  newdata <- as.data.frame(newdata)
  rn <- row.names(newdata)
  Terms <- delete.response(object$terms)
  m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses")))  .checkMFClasses(cl, m)
  keep <- match(row.names(m), rn)
  newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
  xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
  if (xint > 0) newdata <- newdata[, -xint, drop = FALSE]

  fit <- predict(object$finalModel, newdata = as.data.frame(newdata), se.fit = TRUE, interval = "prediction", level = 0.95)
  
  # Extract standard errors
  pred <- fit$fit[,'fit']
  pred_se <- fit$se.fit

  # Compute prediction intervals
  t_value <- qt(1 - alpha/2, df = object$finalModel$df.residual)  # T-distribution critical value

  lower_bound <- prediction_fit - t_value * pred_se
  upper_bound <- prediction_fit + t_value * pred_se

  data.frame(out_data,
    predicted = prediction_fit,
    lower = lower_bound,
    upper = upper_bound) |> as_tibble()

}

##### TODO: 
propagate_pred_intervals <- function(m, pred_int, n = 1000){

  # Prepare storage for simulated LM outputs
  simulated_lm_outputs <- vector("list", length = length(pred_int$predicted))

  # Perform simulations
  for (i in seq_along(rf_predictions)) {
    # Simulate inputs assuming a normal distribution for simplicity
    simulated_inputs <- rnorm(n, mean = rf_predictions[i], 
                              sd = (rf_upper[i] - rf_lower[i]) / (2 * qnorm(0.975)))
    
    # Predict using LM model
    simulated_lm_outputs[[i]] <- predict(lm_model, newdata = data.frame(predictor = simulated_inputs))
  }

  # Calculate summary statistics for each set of simulations
  lm_predictions <- sapply(simulated_lm_outputs, mean)
  lm_lower <- sapply(simulated_lm_outputs, function(x) quantile(x, 0.025))
  lm_upper <- sapply(simulated_lm_outputs, function(x) quantile(x, 0.975))

  # Combine into a dataframe
  prediction_intervals_lm <- data.frame(
    Prediction = lm_predictions,
    Lower = lm_lower,
    Upper = lm_upper
  )

} 

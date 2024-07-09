# load dependencies
source("utils.R")

# set seed for reproducibility - results are similar using several different seeds
set.seed(96297)

################################################################################
### Read data
rw_model <- read_rds("./results/rw_final_model.rds")
tw_model <- read_rds("./results/tw_final_model.rds")
new_data <- read_csv2("./data/ts_pred_data.csv") |>
    dplyr::mutate_if(is.character, as.factor)

## Check error dependency 
rw_pred <- predict_intervals_rf(rw_model, new_data) |>
    select(rw_pred = predicted, rw_sd = predicted_sd)

new_data_filled <- new_data
new_data_filled$raw_water <- ifelse(is.na(new_data_filled$raw_water), rw_pred$rw_pred, new_data_filled$raw_water)

tw_pred <- predict_intervals_rf(tw_model, new_data_filled) |>
    select(tw_pred = predicted, tw_sd = predicted_sd)

bind_cols(new_data, rw_pred, tw_pred) |>
    dplyr::mutate(rw_err = rw_pred - log10(raw_water), tw_err = tw_pred - log10(total_water)) |>
    dplyr::select(rw_err, tw_err) |>
    drop_na() |>
    summarise(err_cov = cov(rw_err, tw_err), err_coef = err_cov / (sd(rw_err) * sd(tw_err))) 

# err_coef: from -0.2 to +0.2 very weak or no association - error independency
rw_pred <- predict_intervals_rf(rw_model, new_data, log_base = 10) |>
    select(rw_pred = predicted, rw_sd = predicted_sd)

new_data_filled <- new_data
new_data_filled$raw_water <- ifelse(is.na(new_data_filled$raw_water), rw_pred$rw_pred, new_data_filled$raw_water)

tw_pred <- predict_intervals_rf(tw_model, new_data_filled, log_base = 10) |>
    select(tw_pred = predicted, tw_sd = predicted_sd)

# Propagate uncertanty assuming error independency err_coef close to zero
predictions <- bind_cols(new_data, rw_pred, tw_pred) |>
    dplyr::mutate(tw_sd_prop = ifelse(is.na(new_data$raw_water), sqrt(tw_sd^2 + rw_sd^2), tw_sd))

# Check overall MAE and RMSE against available data points
predictions |>
    dplyr::select(id, raw_water, total_water, rw_pred, tw_pred) |>
    mutate(rw_err = raw_water - rw_pred, rw_abs_err = abs(rw_err), rw_squared_err = rw_err^2,
           tw_err = total_water - tw_pred, tw_abs_err = abs(tw_err), tw_squared_err = tw_err^2) |>
    summarise(
        rw_mean = mean(raw_water, na.rm = TRUE), rw_median = median(raw_water, na.rm = TRUE), rw_sd = sd(raw_water, na.rm = TRUE),
        rw_mean_pred = mean(rw_pred, na.rm = TRUE), rw_median_pred = median(rw_pred, na.rm = TRUE), rw_sd_pred = sd(rw_pred, na.rm = TRUE),
        rw_pred_mae = mean(rw_abs_err, na.rm = TRUE), rw_pred_rmse = sqrt(mean(rw_squared_err, na.rm = TRUE)),
        tw_mean = mean(total_water, na.rm = TRUE), tw_median = median(total_water, na.rm = TRUE), tw_sd = sd(total_water, na.rm = TRUE),
        tw_mean_pred = mean(tw_pred, na.rm = TRUE), tw_median_pred = median(tw_pred, na.rm = TRUE), tw_sd_pred = sd(tw_pred, na.rm = TRUE),
        tw_pred_mae = mean(tw_abs_err, na.rm = TRUE), tw_pred_rmse = sqrt(mean(tw_squared_err, na.rm = TRUE)))  |>
        pivot_longer(cols = everything())

predictions |>
  dplyr::transmute(rw_pred, rw_sd, tw_pred, tw_sd) |>
  dplyr::summarise(across(everything(), sum)*1e-6)

write_csv2(predictions, "./data/final_predictions.csv")

# water use distribution 
predictions |>
    dplyr::select(`Raw Water.Reference` = raw_water, `Total Water.Reference` = total_water, `Raw Water.Predicted` = rw_pred, `Total Water.Predicted` = tw_pred) |>
    pivot_longer(cols = everything(), names_to = c("Water", "Source"), names_sep = "\\.") |>
    drop_na() |>
    ggplot(aes(x = value / 1e6, color = Source, fill = Source)) +
    facet_wrap(Water ~ ., scales = "free_x") + 
    geom_histogram(aes(y = after_stat(density)), position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.6, adjust = 1.5) +
    geom_vline(
        data = predictions |>
            dplyr::select(`Raw Water.Reference` = raw_water, `Total Water.Reference` = total_water, `Raw Water.Predicted` = rw_pred, `Total Water.Predicted` = tw_pred) |>
            pivot_longer(cols = everything(), names_to = c("Water", "Source"), names_sep = "\\.") |>
            drop_na() |>
            dplyr::group_by(Water, Source) |>
            dplyr::summarise(value = mean(value)),
        aes(xintercept = value / 1e6, color = Source), linetype = "dashed"
    ) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    labs(x = expression("Volume (M " * m^3 * ")"), y = "Density") +
    theme_classic()




rm(list = ls())
gc(reset = TRUE, full = TRUE)
# load dependencies
source("utils.R")

# set seed for reproducibility - results are similar using several different seeds
set.seed(96297)

################################################################################
### Read data
rw_model <- read_rds("./results/rw_final_model.rds")
tw_model <- read_rds("./results/tw_final_model.rds")
new_data <- read_csv2("./data/ts_pred_data.csv") |>
    mutate_if(is.character, as.factor)

## Predict and propagate prediction error
predictions <- propagate_pred_error_rf(rf1 = rw_model, rf2 = tw_model, df = new_data, n = 100, alpha = 0.05, rf1_log_base = 10, rf2_log_base = 10) |>
    dplyr::rename(rw_pred = rf1_pred, tw_pred = rf2_pred, rw_lw = rf1_lw, tw_lw = rf2_lw, rw_up = rf1_up, tw_up = rf2_up)

# Check overall MAE and RMSE against available data points
predictions |>
    dplyr::select(id, raw_water, total_water, rw_pred, tw_pred, rw_lw, tw_lw, rw_up, tw_up) |>
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
  select(rw_pred, tw_pred, rw_lw, tw_lw, rw_up, tw_up) |>
  dplyr::summarise(across(everything(), sum))


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
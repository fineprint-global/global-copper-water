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
predictions <- propagate_pred_error(tw_model, rw_model, new_data)

# Check overall MAE and RMSE against available data points
predictions |>
    select(id, raw_water, total_water, rw_pred, tw_pred, rw_lw, tw_lw, rw_up, tw_up) |>
    mutate(rw_err = raw_water - rw_pred, rw_abs_err = abs(rw_err), rw_squared_err = rw_err^2,
           tw_err = total_water - tw_pred, tw_abs_err = abs(tw_err), tw_squared_err = tw_err^2) |>
    summarise(
        rw_mean = mean(raw_water, na.rm = TRUE), rw_median = median(raw_water, na.rm = TRUE), rw_sd = sd(raw_water, na.rm = TRUE),
        rw_mean_pred = mean(rw_pred, na.rm = TRUE), rw_median_pred = median(rw_pred, na.rm = TRUE), rw_sd_pred = sd(rw_pred, na.rm = TRUE),
        rw_pred_mae = mean(rw_abs_err, na.rm = TRUE), rw_pred_rmse = sqrt(mean(rw_squared_err, na.rm = TRUE)),
        tw_mean = mean(total_water, na.rm = TRUE), tw_median = median(total_water, na.rm = TRUE), tw_sd = sd(total_water, na.rm = TRUE),
        tw_mean_pred = mean(tw_pred, na.rm = TRUE), tw_median_pred = median(tw_pred, na.rm = TRUE), tw_sd_pred = sd(tw_pred, na.rm = TRUE),
        tw_pred_mae = mean(tw_abs_err, na.rm = TRUE), tw_pred_rmse = sqrt(mean(tw_squared_err, na.rm = TRUE)))

write_csv2(predictions, "./data/final_predictions.csv")

rm(list = ls())
gc(reset = TRUE, full = TRUE)
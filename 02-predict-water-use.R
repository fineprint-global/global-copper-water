# load dependencies
source("utils.R")

# set seed for reproducibility - results are similar using several different seeds
set.seed(96297)

################################################################################
### Read data
rw_model <- read_rds("./results/rw_final_model.rds")
tw_model <- read_rds("./results/tw_final_model.rds")
model_vars <- unique(c(rw_model$recipe$var_info$variable[rw_model$recipe$var_info$role!="outcome"],
                       tw_model$recipe$var_info$variable[tw_model$recipe$var_info$role!="none"]))

raw_data <- read_csv2("./data/ts_data_raw.csv")
new_data <- read_csv2("./data/ts_pred_data.csv") |>
    select(id, country_code, model_vars) |>
    dplyr::mutate_if(is.character, as.factor)

## Check error dependency 
rw_pred <- predict_intervals_rf(object = rw_model,  df = rename(new_data, target = raw_water), log_base = 10) |>
    transmute(rw_pred = predicted * new_data$production, rw_pred_sd = sqrt(predicted_sd^2 + 0^2))

## convert raw water intensity to raw water volume
new_data_filled <- new_data |>
    bind_cols(rw_pred) |>
    dplyr::mutate(raw_water = ifelse(is.na(raw_water), rw_pred, raw_water))

tw_pred <- predict_intervals_rf(tw_model, df = new_data_filled, log_base = 10) |>
    select(tw_pred = predicted, tw_pred_sd = predicted_sd)

predictions <- bind_cols(select(new_data, id, raw_water, total_water), rw_pred, tw_pred)

# check error covariance
# bind_cols(new_data, rw_pred, tw_pred) |>
#     dplyr::mutate(rw_pred = ifelse(is.na(raw_water), rw_intensity_pred * production, raw_water,
#                   rw_err = rw_pred - raw_water, tw_err = tw_pred - total_water)) |>
#     dplyr::select(rw_err, tw_err) |>
#     drop_na() |>
#     summarise(err_cov = cov(rw_err, tw_err), err_coef = err_cov / (sd(rw_err) * sd(tw_err))) 

# err_coef: from -0.2 to +0.2 very weak or no association - error independency
# rw_pred <- predict_intervals_rf(rw_model, new_data, log_base = 10) |>
#     select(rw_pred = predicted, rw_sd = predicted_sd)

# new_data_filled <- new_data
# new_data_filled$raw_water <- ifelse(is.na(new_data_filled$raw_water), rw_pred$rw_pred, new_data_filled$raw_water)

# tw_pred <- predict_intervals_rf(tw_model, new_data_filled, log_base = 10) |>
#     select(tw_pred = predicted, tw_sd = predicted_sd)

# Propagate uncertanty assuming error independency err_coef close to zero
# z_score <- qnorm(1 - 0.05 / 2)
# predictions <- bind_cols(new_data, rw_pred, tw_pred) |>
#     dplyr::mutate(tw_sd_prop = ifelse(is.na(new_data$raw_water), sqrt(tw_sd^2 + rw_sd^2), tw_sd),
#                   rw_lw = pmax(rw_pred - z_score * rw_sd, 0), rw_up = rw_pred + z_score * rw_sd,
#                   tw_lw = pmax(tw_pred - z_score * tw_sd_prop, 0), tw_up = tw_pred + z_score * tw_sd_prop)

# # Check overall MAE and RMSE against available data points
predictions |>
    dplyr::select(id, raw_water, total_water, rw_pred, tw_pred) |>
    mutate(rw_err = raw_water - rw_pred, rw_abs_err = abs(rw_err), rw_squared_err = rw_err^2,
           tw_err = total_water - tw_pred, tw_abs_err = abs(tw_err), tw_squared_err = tw_err^2) |>
    summarise(
        rw_mean.ref = mean(raw_water, na.rm = TRUE),
        rw_mean.pred = mean(rw_pred, na.rm = TRUE),
        rw_median.ref = median(raw_water, na.rm = TRUE),
        rw_median.pred = median(rw_pred, na.rm = TRUE), 
        rw_sd.ref = sd(raw_water, na.rm = TRUE),
        rw_sd.pred = sd(rw_pred, na.rm = TRUE),
        rw_rmse.pred = sqrt(mean(rw_squared_err, na.rm = TRUE)),
        rw_mae.pred = mean(rw_abs_err, na.rm = TRUE),
        tw_mean.ref = mean(total_water, na.rm = TRUE),
        tw_mean.pred = mean(tw_pred, na.rm = TRUE),
        tw_median.ref = median(total_water, na.rm = TRUE),
        tw_median.pred = median(tw_pred, na.rm = TRUE), 
        tw_sd.ref = sd(total_water, na.rm = TRUE),
        tw_sd.pred = sd(tw_pred, na.rm = TRUE),
        tw_mae.pred = mean(tw_abs_err, na.rm = TRUE),
        tw_rmse.pred = sqrt(mean(tw_squared_err, na.rm = TRUE)))  |>
        pivot_longer(cols = everything()) |>
        separate(name, into = c("variable", "metric", "type"), sep = "_|\\.") |>
        pivot_wider(names_from = type, values_from = value) |>
        select(metric, variable, ref, pred)

predictions_filled <- predictions |>
    dplyr::select(id, rw_pred, tw_pred) |>
    left_join(raw_data, by = join_by(id)) |>
    dplyr::mutate(raw_water = ifelse(is.na(raw_water), rw_pred, raw_water),
                  total_water = ifelse(is.na(total_water), tw_pred, total_water)) |>
    dplyr::select(-rw_pred, -tw_pred)

predictions_filled |>
  dplyr::transmute(raw_water, total_water) |>
  dplyr::summarise(across(everything(), sum) * 1e-6) # 1e3 to m3 1e-9 to Bm3

write_csv2(predictions_filled, "./results/final_predictions.csv")

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
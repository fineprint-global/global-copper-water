# load dependencies
source("utils.R")

# set seed for reproducibility - results are similar using several different seeds
set.seed(96297)

################################################################################
### Read data
rw_model <- read_rds("./results/rw_final_model.rds")
tw_model <- read_rds("./results/tw_final_model.rds")
rw_model_vars <- unique(rw_model$recipe$var_info$variable[rw_model$recipe$var_info$role!="outcome"])
tw_model_vars <- unique(tw_model$recipe$var_info$variable[tw_model$recipe$var_info$role!="outcome"])

raw_data <- read_csv2("./data/ts_data_raw.csv")
new_data <- read_csv2("./data/ts_pred_data.csv") |>
    dplyr::select(id, country_code, all_of(c(rw_model_vars, tw_model_vars, "raw_water", "total_water", "raw_water_intensity", "total_water_intensity"))) |>
    dplyr::mutate_if(is.character, as.factor) |>
    filter(production != 0, ore_extracted != 0) 

## Check error dependency 
#predict(rw_model, dplyr::mutate(new_data, target = NA)) # it seems recipe needs a placeholder for the target column
rw_pred <- predict_intervals_rf(object = rw_model,  df = dplyr::rename(new_data, target = raw_water)) |>
    transmute(rw_int_pred = predicted,
              rw_int_pred_sd = sqrt(predicted_sd^2 + 0^2))

## convert raw water intensity to raw water volume
new_data_filled <- new_data |>
    bind_cols(rw_pred) |>
    dplyr::mutate(raw_water_intensity = ifelse(is.na(raw_water_intensity), rw_int_pred, raw_water_intensity))

tw_pred <- predict_intervals_rf(tw_model, df = dplyr::mutate(new_data_filled, target = NA)) |>
    select(tw_int_pred = predicted, tw_int_pred_sd = predicted_sd)

predictions <- bind_cols(select(new_data, id, raw_water, total_water, raw_water_intensity, total_water_intensity), rw_pred, tw_pred) |>
    dplyr::mutate(rw_pred = rw_int_pred * new_data$production, tw_pred = tw_int_pred * new_data$production)

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
    # dplyr::mutate(raw_water = ifelse(is.na(production == 0), 0, raw_water),
    #               total_water = ifelse(is.na(production == 0), 0, total_water)) |> # set water use to zero is production equal to zero 
    dplyr::select(-rw_pred, -tw_pred)

predictions_filled |>
  dplyr::transmute(raw_water, total_water) |>
  dplyr::summarise(across(everything(), sum) * 1e-6) # 1e3 to m3 1e-9 to Bm3

png(filename = "./results/density_plot_final_predictions.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
predictions |>
    dplyr::select(`Raw Water.Reference` = raw_water, `Total Water.Reference` = total_water, `Raw Water.Predicted` = rw_pred, `Total Water.Predicted` = tw_pred) |>
    pivot_longer(cols = everything(), names_to = c("Water", "Source"), names_sep = "\\.") |>
    drop_na() |>
    dplyr::mutate(value = value * 1e-3) |> # convert to Mm3 ## 1e3 to m3 1e-6 to Mm3 = 1e-3
    ggplot(aes(x = value, color = Source, fill = Source)) +
    facet_wrap(Water ~ ., scales = "free_x") + 
    geom_histogram(aes(y = after_stat(density)), position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.6, adjust = 1.5) +
    geom_vline(
        data = predictions |>
            dplyr::select(`Raw Water.Reference` = raw_water, `Total Water.Reference` = total_water, `Raw Water.Predicted` = rw_pred, `Total Water.Predicted` = tw_pred) |>
            pivot_longer(cols = everything(), names_to = c("Water", "Source"), names_sep = "\\.") |>
            drop_na() |>
            dplyr::mutate(value = value * 1e-3) |> # convert to Mm3 ## 1e3 to m3 1e-6 to Mm3 = 1e-3
            dplyr::group_by(Water, Source) |>
            dplyr::summarise(value = mean(value)),
        aes(xintercept = value, color = Source), linetype = "dashed"
    ) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    labs(x = expression("Volume (Million " * phantom(" ") * m^3 * ")"), y = "Density") +
    theme_classic()
dev.off()

predictions_filled <- select(sf_data <- st_read("./data/sf_data_raw.gpkg"), id_mine, starts_with("aware_"), starts_with("aqueduct_")) |>
  st_drop_geometry() |>
  as_tibble() |>
  right_join(predictions_filled)

write_csv2(predictions_filled, "./results/final_predictions.csv")

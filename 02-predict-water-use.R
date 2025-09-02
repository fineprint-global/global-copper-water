# ---
# Script: 02-predict-water-use.R
# Purpose: This script applies the trained machine learning models to the full
#          dataset to generate comprehensive water use predictions and perform
#          an initial high-level evaluation.
#
# Key Steps:
# 1. Model and Data Loading: Loads the saved Random Forest models and the
#    pre-processed data.
# 2. Prediction Generation: Uses the loaded models to predict both raw water and
#    total water volumes for all mining sites and time-series observations.
# 3. Uncertainty Propagation: this script uses the prediction from the raw water 
#    model to fill in missing values, ensuring that the uncertainty from the first 
#    model is carried through to the total water prediction.
# 4. Performance Evaluation: Compares the generated predictions against any
#    available reference data points to calculate and summarize overall
#    performance metrics (e.g., RMSE and MAE).
# 5. Output: Saves the full dataset with the new, filled-in water use predictions
#    to a CSV file for the final analysis and visualization.
# ---


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
    transmute(rw_pred = predicted,
              rw_pred_sd = sqrt(predicted_sd^2 + 0^2))

## convert raw water intensity to raw water volume
new_data_filled <- new_data |>
    bind_cols(rw_pred) |>
    dplyr::mutate(raw_water = ifelse(is.na(raw_water), rw_pred, raw_water))

tw_pred <- predict_intervals_rf(tw_model, df = dplyr::mutate(new_data_filled, target = NA)) |>
    select(tw_pred = predicted, tw_pred_sd = predicted_sd)

predictions <- bind_cols(select(new_data, id, raw_water, total_water), rw_pred, tw_pred) |>
    dplyr::mutate(rw_int_pred = rw_pred / new_data$production, tw_int_pred = tw_pred / new_data$production)

# Check overall MAE and RMSE against available data points
predictions |>
    dplyr::select(id, raw_water, total_water, rw_pred, tw_pred) |>
    mutate(rw_err = raw_water - rw_pred, rw_abs_err = abs(rw_err), rw_squared_err = rw_err^2,
           tw_err = total_water - tw_pred, tw_abs_err = abs(tw_err), tw_squared_err = tw_err^2) |>
    summarise(
        rw_mean.ref = log10(mean(raw_water, na.rm = TRUE)),
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
    dplyr::mutate(rw_filled = ifelse(is.na(raw_water), rw_pred, raw_water),
                  tw_filled = ifelse(is.na(total_water), tw_pred, total_water)) 

predictions_filled |>
  dplyr::transmute(rw_filled, tw_filled) |>
  dplyr::summarise(across(everything(), sum) * 1e-6) # 1e3.ML -> m3, 1e-9.m3 -> Bm3

png(filename = "./results/density_plot_final_predictions.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
predictions |>
    dplyr::select(`New Water.Reference` = raw_water, `Total Water.Reference` = total_water, `New Water.Predicted` = rw_pred, `Total Water.Predicted` = tw_pred) |>
    pivot_longer(cols = everything(), names_to = c("Water", "Source"), names_sep = "\\.") |>
    drop_na() |>
    dplyr::mutate(value = value * 1e-3) |> # convert to Mm3 ## 1e3.ML -> m3, 1e-6.m3 -> Mm3
    ggplot(aes(x = value, color = Source, fill = Source)) +
    facet_wrap(Water ~ ., scales = "free_x") + 
    geom_histogram(aes(y = after_stat(density)), position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.6, adjust = 1.5) +
    geom_vline(
        data = predictions |>
            dplyr::select(`New Water.Reference` = raw_water, `Total Water.Reference` = total_water, `New Water.Predicted` = rw_pred, `Total Water.Predicted` = tw_pred) |>
            pivot_longer(cols = everything(), names_to = c("Water", "Source"), names_sep = "\\.") |>
            drop_na() |>
            dplyr::mutate(value = value * 1e-3) |> # convert to Mm3 ## 1e3.ML -> m3, 1e-6.m3 -> Mm3
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

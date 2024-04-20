# load dependencies
source("utils.R")

################################################################################
### Raw water
rw_model <- readRDS("./results/rw_models_20240419.rds")$rf_6

ts_data <- as_tibble(read.csv2("./data/ts_data_raw.csv"))

# Read clean data
rw_pred_data <- ts_data |>
  select(id, year, names(rw_model$trainingData)[-1]) |>
  drop_na()

rw_pred <- predict_intervals_rf(rw_model, rw_pred_data)


################################################################################
### Total water
tw_model <- readRDS("./results/tw_models_20240411.rds")$lm_0

tw_pred_data <- select(rw_pred, id, year, raw_water = predicted)

tw_pred <- predict_intervals_lm(tw_model, tw_pred_data)

##### TODO: 
# propagate_pred_intervals 
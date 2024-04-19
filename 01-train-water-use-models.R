# load dependencies
source("utils.R")

# Read clean data
ts_data <- read.csv2("./data/ts_data_raw.csv") |>
  as_tibble()

# set seed for reproducibility
set.seed(8572)

################################################################################
################################################################################
#
# Raw water
rw_models_file <- paste0("./results/rw_models_20240419.rds")

# Split raw water observations for training (80 %) and testing (20 %)
model_data_rw <- ts_data |>
  select(water = raw_water, production, average_production, ore_grade, process_route, mine_type_combined, by_prod_group, ai_annual) |>
  drop_na() |>
  initial_split(prop = .8)

rw_train <- training(model_data_rw) |> as_tibble()
rw_test  <- testing(model_data_rw) |> as_tibble()

#### Define models 
rw_m_0 <- water ~ production + ore_grade
rw_m_1 <- water ~ production + ore_grade + process_route
rw_m_2 <- water ~ production + ore_grade + mine_type_combined
rw_m_3 <- water ~ production + ore_grade + ai_annual + process_route + mine_type_combined
rw_m_4 <- water ~ production + ore_grade + average_production + ai_annual + process_route + mine_type_combined
rw_m_5 <- water ~ production + ore_grade + average_production + ai_annual + process_route + mine_type_combined + by_prod_group
rw_m_6 <- water ~ production + average_production + ai_annual + process_route + mine_type_combined

#### Tune models using cross validation
rw_fit_control <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 15,
                               savePredictions = "all",
                               allowParallel = TRUE)

if(!file.exists(rw_models_file)){

  models_rw <- list(
    lm_0        = train(rw_m_0, method = "lm", data = rw_train, trControl = rw_fit_control),
    svm_3       = train(rw_m_3, method = "svmRadial", preProc = c("center", "scale"), data = rw_train, trControl = rw_fit_control),
    svm_4       = train(rw_m_4, method = "svmRadial", preProc = c("center", "scale"), data = rw_train, trControl = rw_fit_control),
    rf_3        = train(rw_m_3, method = "rf", ntree = 2000, data = rw_train, trControl = rw_fit_control),
    rf_4        = train(rw_m_4, method = "rf", ntree = 2000, data = rw_train, trControl = rw_fit_control),
    rf_5        = train(rw_m_5, method = "rf", ntree = 2000, data = rw_train, trControl = rw_fit_control),
    rf_6        = train(rw_m_6, method = "rf", ntree = 2000, data = rw_train, trControl = rw_fit_control)
  )

  saveRDS(models_rw, rw_models_file)

} else {

  cat("\nLoading existing models. Remove the file ",rw_models_file, "to train the models again.\n")
  
  models_rw <- readRDS(rw_models_file)

}

# Analyse results
resamps <- resamples(models_rw)
resamps
summary(resamps)

resamps_stats <- summary(resamps)$statistics
resamps_stats <- do.call("rbind", lapply(names(resamps_stats), function(i) data.frame(Metric = i, Model = row.names(resamps_stats[[i]]), resamps_stats[[i]][,-7])))
resamps_stats <- data.frame(lapply(resamps_stats, format_to_sci))
write.csv(resamps_stats, "./results/rw_model_performance.csv", quote = FALSE, na = "NA", row.names = FALSE)

png(filename = "./results/rw_model_performance.png",
    width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1), scales = list(x = "free"))
dev.off()

difValues <- diff(resamps)
difValues
summary(difValues)

pred <- sapply(models_rw, predict, rw_test) |> as.data.frame()

test_perform <- t(sapply(pred, postResample, obs = rw_test$water)) |>
  as.data.frame()  |>
  round(2)
test_perform <- mutate(test_perform, Model = rownames(test_perform), .before = RMSE) |>
  as_tibble() |>
  arrange(RMSE)

test_perform

write.csv(test_perform, "./results/rw_test_performance.csv", quote = FALSE, na = "NA", row.names = FALSE)

#### Bootstraping
# Check model's variability and potential generalizability based on the available data
if(!file.exists("./results/rw_boot_distribution.png")){
  
  rw_boot_control <- trainControl(
    method = "boot",
    number = 100,  
    savePredictions = "final",
    returnResamp = "all" 
  )

  rw_boot <- train(rw_m_4, method = "rf", ntree = 2000, data = bind_rows(rw_train, rw_test), trControl = rw_boot_control)

  rw_boot_res <- as_tibble(rw_boot$resample)

  png(filename = "./results/rw_boot_distribution.png",
      width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
  hist(rw_boot_res$RMSE, main = "", xlab = "RMSE", col = "lightblue", border = "black")
  dev.off()
}

# # Check number of trees
# ggplot(varImp(models_rw$rf_6))
# water ~ production + ore_grade + average_production + ai_annual + process_route + mine_type_combined
# rf_model <- randomForest(rw_m_4, data = rw_train, mtry = 5, ntree = 4000, do.trace = 50)

# error_data <- data.frame(Trees = seq(50, 4000, 50), OOB_Error = rf_model$mse)

# ggplot(error_data, aes(x = Trees, y = OOB_Error)) +
#   geom_point() +
#   labs(title = "OOB Error vs. Number of Trees", x = "Number of Trees", y = "Out-of-Bag Error")







################################################################################
########### Total water
tw_models_file <- paste0("./results/tw_models_20240419.rds")

model_data_tw <- ts_data |>
  select(total_water, raw_water, et0_annual) |>
  drop_na() |>
  initial_split(prop = .60)

tw_train <- training(model_data_tw) |> as_tibble()
tw_test  <- testing(model_data_tw) |> as_tibble()

#### Tune models using cross validation
tw_fit_control <- trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 10,
                               savePredictions = "all",
                               returnResamp = "all",
                               allowParallel = TRUE)


tw_m_0 <- total_water ~ raw_water
tw_m_1 <- total_water ~ raw_water + et0_annual

models_tw <- list(
  lm_0 = train(tw_m_0, method = "lm", data = tw_train, trControl = tw_fit_control),
  lm_1 = train(tw_m_1, method = "lm", data = tw_train, trControl = tw_fit_control)
  )

saveRDS(models_tw, tw_models_file)

resamps <- resamples(models_tw)
resamps
summary(resamps)

resamps_stats <- summary(resamps)$statistics
resamps_stats <- do.call("rbind", lapply(names(resamps_stats), function(i) data.frame(Metric = i, Model = row.names(resamps_stats[[i]]), resamps_stats[[i]][,-7])))
resamps_stats <- data.frame(lapply(resamps_stats, format_to_sci))
write.csv(resamps_stats, "./results/tw_model_performance.csv", quote = FALSE, na = "NA", row.names = FALSE)

png(filename = "./results/tw_model_performance.png",
    width = 200, height = 80, units = "mm", pointsize = 12, res = 300, bg = "white")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1), scales = list(x = "free"))
dev.off()

difValues <- diff(resamps)
difValues
summary(difValues)

pred <- sapply(models_tw, predict, tw_test) |> as.data.frame()

test_perform <- t(sapply(pred, postResample, obs = tw_test$total_water)) |>
  as.data.frame()  |>
  round(2)

test_perform <- mutate(test_perform, Model = rownames(test_perform), .before = RMSE) |>
  as_tibble()

test_perform

write.csv(test_perform, "./results/tw_test_performance.csv", quote = FALSE, na = "NA", row.names = FALSE)


#### Bootstraping
# Check model's variability and potential generalizability based on the available data
if(!file.exists("./results/tw_boot_distribution.png")){  
  tw_boot_control <- trainControl(
    method = "boot",
    number = 100,  
    savePredictions = "final",
    returnResamp = "all" 
  )

  tw_boot <- train(tw_m_0, method = "lm", data = bind_rows(tw_train, tw_test), trControl = tw_boot_control)

  tw_boot_res <- as_tibble(tw_boot$resample)

  png(filename = "./results/rw_boot_distribution.png",
      width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
  hist(tw_boot_res$RMSE, main = "", xlab = "RMSE", col = "lightblue", border = "black")
  dev.off()
}


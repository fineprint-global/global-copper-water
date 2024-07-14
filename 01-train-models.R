# load dependencies
source("utils.R")

# set seed for reproducibility - results are similar using several different seeds
set.seed(9867)

# Define models to try
models <- c("lm", "ridge", "lasso", "rpart", "rf", "gbm", "svmRadial", "qrf")
names(models) <- models

# Load input data and select model variables
model_data <- read_csv2("./data/ts_model_data.csv") |> 
  mutate_if(is.character, as.factor)  |>
  select(raw_water, total_water, production, average_production, ore_grade, raw_water_intensity, total_water_intensity,
  ore_extracted, ratio_raw_water_ore, ratio_total_water_ore, mine_type, byproduct_group, process_route, et0_annual)

################################################################################
####
####                            RAW WATER
####
################################################################################

rw_df <- model_data |>
  select(target = raw_water_intensity, production, average_production, ore_extracted, ore_grade, et0_annual, mine_type) |>
  drop_na()

# Splitting the data into training and test sets
rw_strata <- c("mine_type")
rw_trainIndex <- createDataPartition(interaction(rw_df[,rw_strata], drop = TRUE), p = 0.8, list = FALSE)

# Data augmentation by oversampling underrepresented quantile
#rw_train_data <- training_oversampling_high_quantile(rw_df[rw_trainIndex,], "target", 0.9, over_ratio = 0.5)
rw_train_data <- training_oversampling_quantile(rw_df[rw_trainIndex,], "target", over_ratio = 0.7)
table(rw_train_data$q_class)
rw_train_data <- select(rw_train_data, -q_class)
rw_test_data <- rw_df[-rw_trainIndex,]

# check representativit in strata
if(!all(sort(names(table(interaction(model_data[,rw_strata], drop = TRUE)))) == 
    sort(names(table(interaction(rw_df[,rw_strata], drop = TRUE)))) &
    sort(names(table(interaction(rw_df[,rw_strata], drop = TRUE)))) == 
    sort(names(table(interaction(rw_train_data[,rw_strata], drop = TRUE))))) ) stop("Missing strata in training set")

# Define k-fold cross-validation repetitions 
# use custom strata to ensure all levels are represented in each fold
folds <- 5
times <- 10
train_control <- trainControl(method = "repeatedcv",
                              number = folds,
                              repeats = times,
                              index = createMultiFolds(interaction(rw_train_data[,rw_strata], drop = TRUE), k = folds, times = times),
                              savePredictions = "all",
                              allowParallel = TRUE,
                              classProbs = FALSE,
                              selectionFunction = "best")

# Create recipe - the order is important
rw_recipe <- recipe(target ~ ., data = rw_train_data) |>
  step_log(all_numeric(), base = 10) |> 
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_interact(terms = ~ all_numeric_predictors() * all_numeric_predictors()) |>
  #step_poly(all_numeric_predictors(), degree = 2) |>
  #step_lencode_glm(all_nominal_predictors(), outcome = vars(target)) |>
  step_dummy(all_nominal_predictors()) |>
  step_nzv(all_predictors())

# Check sample features ratio (usually 5 times more samples than features)
if(!5 < nrow(bake(prep(rw_recipe), rw_train_data)) / ncol(bake(prep(rw_recipe), rw_train_data))-1){
  stop("Number of samples smaller than 5 times the number of features")
}

# Train models and store them in a list
rw_trained_models <- lapply(models, function(model) {
  cat("Training model:", model, "\n")
  capture.output(
      out <- train(rw_recipe,
        data = rw_train_data,
        method = model,
        trControl = train_control),
      file = "./results/rw_training_log"
  )
  return(out)
})

# Function to create predictions and residuals for a given model
get_residuals <- function(model, test_data) {
  test_data |>
    mutate(predicted = 10^predict(model, test_data),
           residuals = target - predicted)
}

# Create a combined data frame with residuals for each model
residuals_data <- bind_rows(
  lapply(names(rw_trained_models), function(model_name) {
    get_residuals(rw_trained_models[[model_name]], rw_test_data) %>%
      mutate(model = model_name)
  })
)

# Q-Q Plots of trained models
png(filename = "./results/rw_qq_plot_trained_models.png", width = 250, height = 350, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(residuals_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ model, scales = "fixed") +
  labs(title = "Q-Q Plot of Residuals by Model", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
dev.off()

png(filename = "./results/rw_distribution_trained_models.png", width = 250, height = 350, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(residuals_data, aes(x = residuals)) +
  geom_histogram(aes(y=after_stat(density)))+
  facet_wrap(~ model, scales = "free") +
  labs(title = "Distribution by Model") +
  theme_minimal()
dev.off()

# Models training performance
resamps_stats <- get_resamples_stats(rw_trained_models)
arrange(resamps_stats, Metric, Median)
write.csv(resamps_stats, "./results/rw_models_performance.csv", quote = FALSE, na = "NA", row.names = FALSE)

# Prepare to save the plot
png(filename = "./results/rw_models_performance.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
plot_models_performance(rw_trained_models)
dev.off()

# check significance
resamples(rw_trained_models) |>
  diff() |>
  summary()

# Predict and evaluate all models on the test set
rw_models_test_performance <- lapply(rw_trained_models, function(model) {
  return(postResample(10^predict(model, newdata = rw_test_data), rw_test_data$target))
}) |> 
  bind_rows() |>
  mutate(Model = models) |>
  arrange(RMSE)

rw_models_test_performance

##################################
#### tune best model for raw water

# Define the control using cross-validation
folds <- 10
train_control <- trainControl(
  method = "cv",
  number = folds)

# Define the tuning grid for RF
tune_grid <- expand.grid(mtry = seq(2, ncol(bake(prep(rw_recipe), rw_train_data)) - 1, by = 1))

# Train the RF model with hyperparameter tuning
rw_tuned <- train(rw_recipe,
                  data = rw_train_data, 
                  method = "rf", 
                  ntree = 1000,
                  trControl = train_control, 
                  tuneGrid = tune_grid)

png(filename = "./results/rw_rf_hyperparameter_tuning.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
plot(rw_tuned)
dev.off()

# Train the final model on the entire training set with the best hyperparameters
rw_final_model <- train(rw_recipe, 
                     data = rw_train_data, 
                     method = "rf",
                     ntree = 1000,
                     trControl = trainControl(method = "none"), 
                     tuneGrid = rw_tuned$bestTune)

# Predict on the test set
10^predict(rw_final_model, newdata = rw_test_data) |>
  postResample(rw_test_data$target) |>
  round(2)

summary(rw_df$target)

write_rds(rw_final_model, file = "./results/rw_final_model.rds")

predictions <- rw_test_data |>
  mutate(predicted = 10^predict(rw_final_model, rw_test_data), residuals = target - predicted)

# Q-Q Plots of trained models
png(filename = "./results/rw_qq_plot_distribution_final_model.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
qq_plot <- ggplot(predictions, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Residuals by Model", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Create the histogram plot
hist_plot <- ggplot(predictions, aes(x = residuals)) +
  geom_histogram(aes(y=after_stat(density)), binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution by Model", x = "Residuals", y = "Density") +
  theme_minimal()

# Combine the plots
qq_plot | hist_plot
dev.off()


################################################################################
####
####                            TOTAL WATER
####
################################################################################

tw_df <- model_data |>
  select(total_water, raw_water, average_production, process_route) |>
  drop_na()

# Check categories
table(interaction(model_data$process_route, drop = TRUE))
table(interaction(tw_df$process_route, drop = TRUE))

# Create recipe - the order is important
set.seed(3556)
tw_recipe <- recipe(total_water ~ ., data = tw_df) |>
  step_log(all_numeric(), base = 10) |>  
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_nzv(all_predictors())

# Splitting the data into training and test sets
tw_trainIndex <- createDataPartition(tw_df$process_route, p = 0.8, list = FALSE)

folds <- 5
times <- 10
train_control <- trainControl(method = "repeatedcv",
                              number = folds,
                              repeats = times,
                              index = createMultiFolds(interaction(tw_df$process_route[tw_trainIndex], drop = TRUE), k = folds, times = times),
                              savePredictions = "all",
                              allowParallel = TRUE,
                              classProbs = FALSE,
                              selectionFunction = "best")

# Train models and store them in a list
tw_trained_models <- lapply(models, function(model) {
  cat("Training model:", model, "\n")
  capture.output(
      out <- train(tw_recipe, 
          data = tw_df[tw_trainIndex, ],
          method = model,
          trControl = train_control),
      file = "./results/tw_training_log"
  )
  return(out)
})

# Models training performance
resamps_stats <- get_resamples_stats(tw_trained_models)
arrange(resamps_stats, Metric, Median)
write.csv(resamps_stats, "./results/tw_models_performance.csv", quote = FALSE, na = "NA", row.names = FALSE)

# Prepare to save the plot
png(filename = "./results/tw_models_performance.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
plot_models_performance(tw_trained_models)
dev.off()

# check significance
resamples(tw_trained_models) |>
  diff() |>
  summary()

# Predict and evaluate all models on the test set
tw_models_test_performance <- lapply(tw_trained_models, function(model) {
  return(postResample(10^predict(model, newdata = tw_df[-tw_trainIndex, ]), tw_df[-tw_trainIndex, ]$total_water))
}) |> 
  bind_rows() |>
  mutate(Model = models) |>
  arrange(RMSE)

tw_models_test_performance


#### Best model retrain 

# # Define the control using cross-validation
# folds <- 5
# train_control <- trainControl(
#   method = "cv",
#   index = createFolds(interaction(tw_df$process_route[tw_trainIndex], drop = TRUE), k = folds),
#   number = folds)

# # Train the final model 
# tw_final_model <- train(tw_recipe, 
#                      data = tw_df[tw_trainIndex, ], 
#                      method = "lm", 
#                      trControl = train_control)

# # Predict on the test set
# predict(tw_final_model, newdata = tw_df[-tw_trainIndex, ]) |>
#   postResample(tw_df[-tw_trainIndex, ]$total_water) |>
#   round(2)

# summary(tw_df[-tw_trainIndex, ]$total_water)

# write_rds(tw_final_model, file = "./results/tw_final_model.rds")

folds <- 5
train_control <- trainControl(
  method = "cv",
  index = createFolds(interaction(tw_df$process_route[tw_trainIndex], drop = TRUE), k = folds),
  number = folds)

# Define the tuning grid for RF
tune_grid <- expand.grid(mtry = seq(2, ncol(bake(prep(tw_recipe), tw_df[tw_trainIndex,])) - 1, by = 1))

# Train the RF model with hyperparameter tuning
tuned_rf <- train(tw_recipe,
                  data = tw_df[tw_trainIndex, ], 
                  method = "rf", 
                  ntree = 1000,
                  trControl = train_control, 
                  tuneGrid = tune_grid)

png(filename = "./results/tw_rf_hyperparameter_tuning.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
plot(tuned_rf)
dev.off()

# Train the final model on the entire training set with the best hyperparameters
tw_final_model <- train(tw_recipe, 
                     data = tw_df[tw_trainIndex, ], 
                     method = "rf", 
                     ntree = 1000,
                     trControl = trainControl(method = "none"), 
                     tuneGrid = tuned_rf$bestTune)

# Predict on the test set
10^predict(tw_final_model, newdata = tw_df[-tw_trainIndex, ]) |>
  postResample(tw_df[-tw_trainIndex, ]$total_water) |>
  round(2)

summary(tw_df[-tw_trainIndex, ]$total_water)

write_rds(rw_final_model, file = "./results/tw_final_model.rds")

# Create a data frame with the actual values, predicted values, and residuals
predictions <- tw_df |>
  mutate(predicted = 10^predict(tw_final_model, tw_df), residuals = total_water - predicted)

# Q-Q Plot
ggplot(predictions, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()




rm(list = ls())
gc(reset = TRUE, full = TRUE)

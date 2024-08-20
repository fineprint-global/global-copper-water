# load dependencies
source("utils.R")

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
# set seed for reproducibility - results are similar using several different seeds
set.seed(9867)

rw_df <- model_data |>
  select(target = raw_water_intensity, production, ore_extracted, average_production, et0_annual, process_route) |>
  drop_na()

table(interaction(model_data$process_route, drop = TRUE))
table(interaction(rw_df$process_route, drop = TRUE))

# Splitting the data into training and test sets
rw_strata <- c("process_route")
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
    get_residuals(rw_trained_models[[model_name]], rw_test_data) |>
      mutate(model = model_name)
  })
)

# Q-Q Plots of trained models
png(filename = "./results/rw_qq_plot_trained_models.png", width = 250, height = 250, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(residuals_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ model, scales = "fixed") +
  labs(title = "", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
dev.off()


png(filename = "./results/rw_distribution_trained_models.png", width = 250, height = 250, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(residuals_data, aes(x = residuals)) +
  geom_histogram(aes(y=after_stat(density)))+
  facet_wrap(~ model, scales = "fixed") +
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
rw_m_diff <- resamples(rw_trained_models) |>
  diff() |>
  summary()

rw_m_diff

write_csv(create_model_comparison_csv(rw_m_diff$table), "results/rw_models_diff.csv")

# Predict and evaluate all models on the test set
rw_models_test_performance <- lapply(rw_trained_models, function(model) {
  return(postResample(10^predict(model, newdata = rw_test_data), rw_test_data$target))
}) |> 
  bind_rows() |>
  mutate(Model = models) |>
  arrange(RMSE) |>
  select(Model, RMSE, MAE, Rsquared)

rw_models_test_performance

write_csv(rw_models_test_performance, "results/rw_models_test_performace.csv")

# Convert the data to long format for faceting
rw_models_long <- rw_models_test_performance |>
  pivot_longer(cols = c(RMSE, MAE, Rsquared), names_to = "Metric", values_to = "Value") |>
  mutate(Model = factor(Model, levels = rw_models_test_performance |> arrange(RMSE) |> pull(Model))) |>
  mutate(Metric = factor(Metric, levels = c("RMSE", "MAE", "Rsquared")))

png(filename = "./results/rw_models_test_performance.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(rw_models_long, aes(x = Model, y = Value)) +
  geom_bar(stat = "identity", fill = "gray") +  # Set all bars to gray
  facet_wrap(~ Metric, scales = "free_y") +
  labs(x = "Model", y = "Metric value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

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
  labs(title = "", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Create the histogram plot
hist_plot <- ggplot(predictions, aes(x = residuals)) +
  geom_histogram(aes(y=after_stat(density)), binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Residuals", y = "Density") +
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
  select(target = total_water_intensity, ore_grade, raw_water_intensity, mine_type) |>
  drop_na()

# Check categories
table(interaction(model_data$mine_type, drop = TRUE))
table(interaction(tw_df$mine_type, drop = TRUE))

# Create recipe - the order is important
set.seed(9867)
tw_recipe <- recipe(target ~ ., data = tw_df) |>
  step_log(all_numeric(), base = 10) |>  
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_interact(terms = ~ all_numeric_predictors() * all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_nzv(all_predictors())

# Splitting the data into training and test sets
tw_trainIndex <- createDataPartition(tw_df$mine_type, p = 0.8, list = FALSE)

# Data augmentation by oversampling underrepresented quantile
tw_train_data <- training_oversampling_high_quantile(tw_df[tw_trainIndex,], "target", over_ratio = 0.7)
table(tw_train_data$q_class)
tw_train_data <- select(tw_train_data, -q_class)
tw_test_data <- tw_df[-tw_trainIndex,]

folds <- 5
times <- 10
train_control <- trainControl(method = "repeatedcv",
                              number = folds,
                              repeats = times,
                              index = createMultiFolds(interaction(tw_df$mine_type[tw_trainIndex], drop = TRUE), k = folds, times = times),
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

# Function to create predictions and residuals for a given model
get_residuals <- function(model, test_data) {
  test_data |>
    mutate(predicted = 10^predict(model, test_data),
           residuals = target - predicted)
}

# Create a combined data frame with residuals for each model
tw_residuals_data <- bind_rows(
  lapply(names(tw_trained_models), function(model_name) {
    get_residuals(tw_trained_models[[model_name]], tw_test_data) |>
      mutate(model = model_name)
  })
)

# Q-Q Plots of trained models
png(filename = "./results/tw_qq_plot_trained_models.png", width = 250, height = 250, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(tw_residuals_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ model, scales = "fixed") +
  labs(title = "", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
dev.off()

# Models training performance
resamps_stats <- get_resamples_stats(tw_trained_models)
arrange(resamps_stats, Metric, Median)
write.csv(resamps_stats, "./results/tw_models_performance.csv", quote = FALSE, na = "NA", row.names = FALSE)

# Prepare to save the plot
png(filename = "./results/tw_models_performance.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
plot_models_performance(tw_trained_models)
dev.off()

# check significance
tw_m_diff <- resamples(tw_trained_models) |>
  diff() |>
  summary()

tw_m_diff

write_csv(create_model_comparison_csv(tw_m_diff$table), "results/tw_models_diff.csv")

# Predict and evaluate all models on the test set
tw_models_test_performance <- lapply(tw_trained_models, function(model) {
  return(postResample(10^predict(model, newdata = tw_df[-tw_trainIndex, ]), tw_df[-tw_trainIndex, ]$target))
}) |> 
  bind_rows() |>
  mutate(Model = models) |>
  arrange(RMSE)

tw_models_test_performance

write_csv(tw_models_test_performance, "results/rw_models_test_performace.csv")

# Convert the data to long format for faceting
tw_models_long <- tw_models_test_performance |>
  pivot_longer(cols = c(RMSE, MAE, Rsquared), names_to = "Metric", values_to = "Value") |>
  mutate(Model = factor(Model, levels = tw_models_test_performance |> arrange(RMSE) |> pull(Model))) |>
  mutate(Metric = factor(Metric, levels = c("RMSE", "MAE", "Rsquared")))

png(filename = "./results/tw_models_test_performance.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(tw_models_long, aes(x = Model, y = Value)) +
  geom_bar(stat = "identity", fill = "gray") +  # Set all bars to gray
  facet_wrap(~ Metric, scales = "free_y") +
  labs(x = "Model", y = "Metric value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#### Best model retrain 

folds <- 5
train_control <- trainControl(
  method = "cv",
  index = createFolds(interaction(tw_df$mine_type[tw_trainIndex], drop = TRUE), k = folds),
  number = folds)

# Define the tuning grid for RF
tune_grid <- expand.grid(mtry = seq(2, ncol(bake(prep(tw_recipe), tw_df[tw_trainIndex,])) - 1, by = 1))

# Train the RF model with hyperparameter tuning
tw_tuned_rf <- train(tw_recipe,
                  data = tw_df[tw_trainIndex, ], 
                  method = "rf", 
                  ntree = 1000,
                  trControl = train_control, 
                  tuneGrid = tune_grid)

png(filename = "./results/tw_rf_hyperparameter_tuning.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
plot(tw_tuned_rf)
dev.off()

# Train the final model on the entire training set with the best hyperparameters
tw_final_model <- train(tw_recipe, 
                     data = tw_df[tw_trainIndex, ], 
                     method = "rf", 
                     ntree = 1000,
                     trControl = trainControl(method = "none"), 
                     tuneGrid = tw_tuned_rf$bestTune)

# Predict on the test set
10^predict(tw_final_model, newdata = tw_df[-tw_trainIndex, ]) |>
  postResample(tw_df[-tw_trainIndex, ]$target) |>
  round(2)

summary(tw_df[-tw_trainIndex, ]$target)

write_rds(tw_final_model, file = "./results/tw_final_model.rds")

# Create a data frame with the actual values, predicted values, and residuals
predictions <- tw_df |>
  mutate(predicted = 10^predict(tw_final_model, tw_df), residuals = target - predicted)

# Q-Q Plots of trained models
png(filename = "./results/tw_qq_plot_distribution_final_model.png", width = 250, height = 120, units = "mm", pointsize = 12, res = 300, bg = "white")
qq_plot <- ggplot(predictions, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Create the histogram plot
hist_plot <- ggplot(predictions, aes(x = residuals)) +
  geom_histogram(aes(y=after_stat(density)), fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Residuals", y = "Density") +
  theme_minimal()

# Combine the plots
qq_plot | hist_plot
dev.off()


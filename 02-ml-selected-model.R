library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(s2)
library(readxl)
library(caret)
library(ggplot2)
library(scales)
library(terra)
library(tune)
library(readr)

if(!file.exists("data/WaterDepletion_WaterGap3.zip")){
 download.file("https://s3.us-east-2.amazonaws.com/earthstatdata/WaterDepletion_WaterGap3.zip", destfile = "data/WaterDepletion_WaterGap3.zip")
 unzip("data/WaterDepletion_WaterGap3.zip", exdir = "data/")
}

water_depletion <- rast("data/WaterDepletion_WaterGap3/WaterDepletionCat_WG3.tif")

### Add GRACE - Trends in Global Freshwater Availability from the Gravity Recovery and Climate Experiment (GRACE), v1 (2002 – 2016)
#   Rodell, M., J. S. Famiglietti, D. N. Wiese, J. T. Reager, H. K. Beaudoing, F. W. Landerer, and M.-H. Lo. 2019. Trends in Global Freshwater Availability from the Gravity Recovery and Climate Experiment (GRACE). Palisades, New York: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4TT4P2C.
#   Rodell, M., J. S. Famiglietti, D. N. Wiese, J. T. Reager, H. K. Beaudoing, F. W. Landerer, and M.-H. Lo. 2018. Emerging Trends in Global Freshwater Availability. Nature 557(7707): 651-659. https://doi.org/10.1038/s41586-018-0123-1.
# 
# Dowanload requires login to Earthdata. Direct links:
# Trends: https://sedac.ciesin.columbia.edu/downloads/data/sdei/sdei-trends-freshwater-availability-grace/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip
# Water mask: https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-data-quality-indicators-rev11/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip

if(file.exists("data/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip")){
 unzip("data/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip", exdir = "data/")
}
freshwater_availability <- rast("data/freshwater_availability.tif")

if(file.exists("data/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip")){
 unzip("data/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip", exdir = "data/")
}
water_mask <- rast("data/gpw_v4_data_quality_indicators_rev11_watermask_30_min.tif")

raw_data <- read_excel('./data/Copper data for analysis_20240405.xlsx') |> 
  mutate(id = row_number())

sf_data <- select(raw_data, id, Longitude, Latitude, REG_TOP_20, mine, snl_id, country, country_code, region, cumulative_production, average_production, 
                  by_prod_group = `by-prod-group 2`, mine_type_combined, ore_body_group = `Ore Body Group`, process_route = `Process route`, ore_grade = `Ore grade MI`) |> 
  mutate(
    Latitude  = as.numeric(ifelse(Latitude == "Copper", 0, Latitude)),
    check_0  = Longitude == 0 & Latitude == 0, 
    Longitude = ifelse(check_0, NA, Longitude),
    Latitude  = ifelse(check_0, NA, Latitude)) |> 
  select(-check_0) |> 
  filter(!is.na(Longitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

sf_data$water_depletion <- extract(water_depletion, sf_data)[,2]
sf_data$freshwater_availability <- extract(freshwater_availability, sf_data)[,2]

ts_data <- select(raw_data, id, `2015`, `2016`, `2017`, `2018`, `2019`, ToWa_2015, RaWa_2015, ToWa_2016, RaWa_2016, ToWa_2017, RaWa_2017, ToWa_2018, RaWa_2018, ToWa_2019, RaWa_2019) |> 
  pivot_longer(cols = -id, names_to = "year", values_to = "value") |> 
  mutate(row_data = ifelse(str_detect(year, "ToWa"), "total_water", 
                           ifelse(str_detect(year, "RaWa"), "raw_water", "production")), 
         year = str_replace_all(year, "ToWa_", ""),
         year = str_replace_all(year, "RaWa_", ""),
         year = as.numeric(year)) |> 
  pivot_wider(id_cols = c(id, year), names_from = row_data, values_from = value) |> 
  mutate(production = ifelse(production == 0, NA, production),
         total_water = ifelse(total_water == 0, NA, total_water),
         raw_water = ifelse(raw_water == 0, NA, raw_water)) |> 
  left_join(st_drop_geometry(sf_data)) |> 
  mutate(process_route = ifelse(process_route=="Other", "other", process_route),
         process_route = ifelse(process_route=="0", "other", process_route), 
         process_route = as.character(process_route),
         by_prod_group = as.character(by_prod_group), 
         mine_type_combined = as.character(mine_type_combined))

# CHeck remove process route "other" / "unknonwn" - currently ~1.5% of the recorded production 
ts_data |> 
  group_by(process_route) |> 
  summarise(production = sum(production, na.rm = T)) |>
  mutate(perc = production / sum(production)) |>
  arrange(desc(perc))

ts_data <- filter(ts_data, process_route != "other")

# create prediction set 
selected_predictors_numeric <- c("production") 
selected_predictors_factor <- c("process_route", "mine_type_combined", "ore_body_group", "by_prod_group")
selected_predictors <- c(selected_predictors_numeric, selected_predictors_factor)

no_missing_data <- ts_data |> 
  mutate(by_prod_group = ifelse(by_prod_group == "CuCu", "CuNN", by_prod_group)) |>
  select(id, year, total_water, raw_water, all_of(selected_predictors)) |> 
  drop_na(any_of(selected_predictors))

# Check data availability 
no_missing_data |> 
  select(total_water, raw_water, ore_body_group, process_route, mine_type_combined, by_prod_group) |> 
  pivot_longer(cols = -c(total_water, raw_water)) |> 
  pivot_longer(cols = c(total_water, raw_water), names_to = "water_use", values_to = "volume", values_drop_na = TRUE) |> 
  ggplot(aes(x = value, y = volume, colour = water_use)) + 
  facet_wrap(~name, scales = "free") + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE)

data_training_total_water <- drop_na(select(no_missing_data, -raw_water))
length(unique(data_training_total_water$id))

data_training_raw_water <- drop_na(select(no_missing_data, -total_water))
length(unique(data_training_raw_water$id))

setdiff(no_missing_data$process_route, data_training_raw_water$process_route)
setdiff(no_missing_data$mine_type_combined, data_training_raw_water$mine_type_combined)
setdiff(no_missing_data$by_prod_group, data_training_raw_water$by_prod_group)
setdiff(no_missing_data$ore_body_group, data_training_raw_water$ore_body_group)

setdiff(no_missing_data$process_route, data_training_total_water$process_route)
setdiff(no_missing_data$mine_type_combined, data_training_total_water$mine_type_combined)
setdiff(no_missing_data$by_prod_group, data_training_total_water$by_prod_group)
setdiff(no_missing_data$ore_body_group, data_training_total_water$ore_body_group)

data_prediction_raw_water <- no_missing_data |>
  filter(
    !process_route %in% setdiff(no_missing_data$process_route, data_training_raw_water$process_route),
    !mine_type_combined %in% setdiff(no_missing_data$mine_type_combined, data_training_raw_water$mine_type_combined),
    !by_prod_group %in% setdiff(no_missing_data$by_prod_group, data_training_raw_water$by_prod_group),
    !ore_body_group %in% setdiff(no_missing_data$ore_body_group, data_training_raw_water$ore_body_group)
  )

data_prediction_raw_water
length(unique(data_prediction_raw_water$id))

data_prediction_total_water <- no_missing_data |>
  filter(
    !process_route %in% setdiff(no_missing_data$process_route, data_training_total_water$process_route),
    !mine_type_combined %in% setdiff(no_missing_data$mine_type_combined, data_training_total_water$mine_type_combined),
    !by_prod_group %in% setdiff(no_missing_data$by_prod_group, data_training_total_water$by_prod_group),
    !ore_body_group %in% setdiff(no_missing_data$ore_body_group, data_training_total_water$ore_body_group)
  )

data_prediction_total_water
length(unique(data_prediction_total_water$id))

sum(data_prediction_total_water$production) / sum(ts_data$production, na.rm = T)

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,     # number of folds
                            repeats = 10)    # repeated ten times

fm <- as.formula(str_c("raw_water ~ ", str_c(selected_predictors, collapse = " + ")))

model_rf <- ?train(fm, data = data_training_raw_water, method = "rf",  trControl = fit_control)  

ggplot(varImp(model_rf))

model_rf

data_prediction <- data_prediction |> 
  mutate(water_var_pred = predict(model_rf, data_prediction), .after = water_var)

select(sf_data, id, country) |> 
  st_drop_geometry() |> 
  left_join(data_prediction, multiple = "all") |> 
  filter(!is.na(water_var)) |> 
  ggplot(aes(x = water_var_pred, y = water_var, label = id, colour = country)) + 
  geom_point() + 
  geom_abline(colour = "blue") + 
  geom_text(hjust=0, vjust=0, size = 3) +
  coord_obs_pred() + 
  scale_color_viridis_d(direction = -1)

source("make_map.R")
cells_size <- 200000
year <- 2019
grid_cells <- s2_data_countries() |> 
  st_as_sf() |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  st_make_grid(cellsize = cells_size)

# prediction 
grid_pred <- select(sf_data, id) |> 
  left_join(filter(data_prediction, year == year), multiple = "all") |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  select(total_water_pred) |> 
  aggregate(grid_cells, sum, na.rm = TRUE) |> 
  filter(!is.na(total_water_pred), total_water_pred > 0)

plot_goode_homolosine_world_map(ocean_color = "#e5f1f8", land_color = "gray95", 
                                grid_color = "grey75", grid_size = 0.1,
                                country_borders_color = "grey75", country_borders_size = 0.1) +
  ggplot2::geom_sf(data = grid_pred, mapping = aes(fill = total_water_pred * 1e-6), color = NA, lwd = 0, size = 0) + # * 100 from km2 to ha
  ggplot2::coord_sf(crs = "+proj=igh", expand = FALSE) +
  viridis::scale_fill_viridis(option = "turbo", begin = 0, end = 1, direction = 1, 
                              discrete = FALSE,
                              trans = log10_trans()
                              #labels = function(x) sprintf("%g", x)
  ) +
  theme(
    legend.spacing.x = unit(1.0, 'cm'),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.spacing = unit(0.0, "cm"),
    legend.key.size = unit(0.3, "cm"),
    legend.key.width = unit(20, "mm"),
    plot.margin = margin(t = -1, r = -1, b = 0, l = -1, unit = "cm")
  ) +
  labs(fill = bquote(Total~water~use~(M~m^3))) 

# training data 
grid_pred <- select(sf_data, id) |> 
  left_join(filter(data_prediction, year == year), multiple = "all") |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  select(total_water_pred = total_water) |> 
  aggregate(grid_cells, sum, na.rm = TRUE) |> 
  filter(!is.na(total_water_pred), total_water_pred > 0)

plot_goode_homolosine_world_map(ocean_color = "#e5f1f8", land_color = "gray95", 
                                grid_color = "grey75", grid_size = 0.1,
                                country_borders_color = "grey75", country_borders_size = 0.1) +
  ggplot2::geom_sf(data = grid_pred, mapping = aes(fill = total_water_pred * 1e-6), color = NA, lwd = 0, size = 0) + # * 100 from km2 to ha
  ggplot2::coord_sf(crs = "+proj=igh", expand = FALSE) +
  viridis::scale_fill_viridis(option = "turbo", begin = 0, end = 1, direction = 1, 
                              discrete = FALSE,
                              trans = log10_trans(),
                              labels = function(x) sprintf("%g", x)
  ) +
  theme(
    legend.spacing.x = unit(1.0, 'cm'),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.spacing = unit(0.0, "cm"),
    legend.key.size = unit(0.3, "cm"),
    legend.key.width = unit(20, "mm"),
    plot.margin = margin(t = -1, r = -1, b = 0, l = -1, unit = "cm")
  ) +
  labs(fill = bquote(Total~water~use~(M~m^3))) 

res <- left_join(ts_data, data_prediction) |> 
  relocate(total_water, .before = total_water_pred) |> 
  mutate(total_water_abs_diff = abs(total_water - total_water_pred)) 

hist(res$total_water_abs_diff)

write_csv(res, "data/total_water_prediction.csv")

# total water use in 2016
select(res, year, freshwater_availability, total_water_pred) |> 
  filter(!is.na(total_water_pred), year == 2016) |> 
  mutate(freshwater_availability = cut(freshwater_availability, breaks = -3:3)) |>
  group_by(freshwater_availability) |>
  summarise(total_water_pred = sum(total_water_pred)) |>
  ggplot(aes(x = freshwater_availability, y = total_water_pred)) + 
  geom_col() + 
  scale_color_viridis_d(direction = 1)

# mean total water use
select(res, id, freshwater_availability, total_water_pred) |> 
  filter(!is.na(total_water_pred)) |> 
  mutate(freshwater_availability = cut(freshwater_availability, breaks = -3:3)) |>
  group_by(id, freshwater_availability) |>
  summarise(total_water_pred = mean(total_water_pred)) |>
  ungroup() |>
  group_by(freshwater_availability) |>
  summarise(total_water_pred = sum(total_water_pred)) |>
  ggplot(aes(x = freshwater_availability, y = total_water_pred)) + 
  geom_col() + 
  scale_color_viridis_d(direction = 1)

# mean total water use
plot_data <- select(res, id, year, total_water_pred, freshwater_availability, production) |> 
  filter(!is.na(total_water_pred)) |> 
  mutate(id = factor(id)) |> 
  group_by(id) |> 
  summarise(
    production_mean = production,
    production_sum = sum(production),
    total_water_trend = lm(total_water_pred ~ year)$coefficients[2],
    freshwater_availability_trend = unique(freshwater_availability)) |>
  mutate(
    quadrant = case_when(
      total_water_trend >= 0 & freshwater_availability_trend >= 0 ~ "Q1",
      total_water_trend < 0 & freshwater_availability_trend >= 0 ~ "Q2",
      total_water_trend < 0 & freshwater_availability_trend < 0 ~ "Q3",
      total_water_trend >= 0 & freshwater_availability_trend < 0 ~ "Q4",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(quadrant))

prod_summary <- group_by(plot_data, quadrant) |>
  summarise(production_sum = sum(production_sum, na.rm = TRUE) / 1e6) |>
  mutate(
    production_perc = production_sum / sum(production_sum) * 100,
    prod_text = paste0(quadrant, ": ", sprintf("%.1f", production_sum), " Mt (", sprintf("%.1f", production_perc), "%)")
  )


ggplot(plot_data, aes(x = total_water_trend, y = freshwater_availability_trend, size = production_sum, color = quadrant)) + 
  geom_point(alpha = 0.2) + # Adjust alpha for visibility
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Thicker line for x = 0
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + # Thicker line for y = 0
  scale_size(range = c(1, 12)) + # Adjust the size range as needed
  scale_color_manual(values = c("Q1" = "blue", "Q2" = "green", "Q3" = "red", "Q4" = "purple")) + # Custom colors for each quadrant
  geom_text(x =  5000, y =  1.0, label = prod_summary$prod_text[1], color = "black", size = 5) +
  geom_text(x = -5000, y =  1.0, label = prod_summary$prod_text[2], color = "black", size = 5) +
  geom_text(x = -5000, y = -1.5, label = prod_summary$prod_text[3], color = "black", size = 5) +
  geom_text(x =  5000, y = -1.5, label = prod_summary$prod_text[4], color = "black", size = 5) +
  guides(color = FALSE) +
  theme_minimal()
  
select(res, year, production, freshwater_availability, total_water_pred) |> 
  filter(!is.na(total_water_pred), year == 2016) |> 
  mutate(freshwater_availability = cut(freshwater_availability, breaks = -3:3)) |>
  group_by(freshwater_availability) |>
  summarise(total_water_pred = sum(total_water_pred), production = sum(production)) |> 
  pivot_longer(cols = c(total_water_pred, production))

# cross-country check 

data_training_chl <-
  data_training |> 
  left_join(select(ts_data, id, year, country_code)) |> 
  filter(country_code == "CHL") |> 
  select(-country_code)

data_validation <-
  data_training |> 
  left_join(select(ts_data, id, year, country_code)) |> 
  filter(country_code != "CHL") |> 
  select(-country_code)

fm <- as.formula(str_c("total_water ~ ", str_c(selected_predictors, collapse = " + ")))

model_rf_chl <- train(fm, data = data_training_chl, method = "rf")  

model_rf_chl

data_validation <- data_validation |> 
  mutate(total_water_pred = predict(model_rf, data_validation), .after = total_water)

RMSE(data_validation$total_water_pred, data_validation$total_water)

MAE(data_validation$total_water_pred, data_validation$total_water)

select(sf_data, id, country) |> 
  st_drop_geometry() |> 
  left_join(data_validation, multiple = "all") |> 
  filter(!is.na(total_water)) |> 
  ggplot(aes(x = total_water_pred, y = total_water, label = id, colour = country)) + 
  geom_point() + 
  geom_abline(colour = "blue") + 
  geom_text(hjust=0, vjust=0, size = 3) +
  coord_obs_pred() + 
  scale_color_viridis_d(direction = -1)


fm <- as.formula(str_c("total_water ~ ", str_c(selected_predictors, collapse = " + ")))

model_rf_chl <- train(fm, data = data_validation, method = "rf")  

model_rf_chl

data_validation2 <- data_training_chl |> 
  mutate(total_water_pred = predict(model_rf, data_training_chl), .after = total_water)

RMSE(data_validation2$total_water_pred, data_validation2$total_water)

MAE(data_validation2$total_water_pred, data_validation2$total_water)

select(sf_data, id, country) |> 
  st_drop_geometry() |> 
  left_join(data_validation2, multiple = "all") |> 
  filter(!is.na(total_water)) |> 
  ggplot(aes(x = total_water_pred, y = total_water, label = id, colour = country)) + 
  geom_point() + 
  geom_abline(colour = "blue") + 
  geom_text(hjust=0, vjust=0, size = 3) +
  coord_obs_pred() + 
  scale_color_viridis_d(direction = 1)


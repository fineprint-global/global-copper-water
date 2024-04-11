library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(s2)
library(readxl)
library(caret)
library(ggplot2)
library(scales)
raw_data <- read_excel('./data/Copper data for analysis_20240304.xlsx') |> 
  mutate(id = row_number())

sf_data <- select(raw_data, id, Longitude, Latitude, REG_TOP_20, mine, snl_id, country, country_code, region, cumulative_production, average_production, 
                  by_prod_group = `by-prod-group`, mine_type_combined, ore_body_group = `Ore Body Group`, process_route = `Process route`, ore_grade = `Ore grade MI`) |> 
  mutate(
    Latitude  = as.numeric(ifelse(Latitude == "Copper", 0, Latitude)),
    check_0  = Longitude == 0 & Latitude == 0, 
    Longitude = ifelse(check_0, NA, Longitude),
    Latitude  = ifelse(check_0, NA, Latitude)) |> 
  select(-check_0) |> 
  filter(!is.na(Longitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

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
  mutate(process_route = ifelse(process_route=="Other", "other", process_route), process_route = as.character(process_route)) |> 
  # filter(!str_detect(process_route, "Other|other|0|pyro/hydro")) |> 
  filter(!str_detect(process_route, "0")) |> 
  mutate(process_route = as.character(process_route),
         by_prod_group = as.character(by_prod_group), 
         mine_type_combined = as.character(mine_type_combined))

data_prediction <- ts_data |> 
  select(id, year, water_var = raw_water, production, mine_type_combined, by_prod_group, process_route, ore_body_group) |> 
  filter(!str_detect(process_route, "other")) |> 
  #filter(!str_detect(by_prod_group, "CuAA|CuZn|CuNN|CuMo|CuNi|CuCu|CuCo")) |> 
  #mutate(mine_type_combined = ifelse(mine_type_combined=="Underground", "other", mine_type_combined)) |> 
  #filter(!str_detect(mine_type_combined, "other")) |> 
  filter(!is.na(production),
         !is.na(mine_type_combined),
         !is.na(by_prod_group),
         !is.na(process_route),
         !is.na(ore_body_group))

data_training <- drop_na(data_prediction)

# level not present in training data
unique(data_prediction$process_route)[!unique(data_prediction$process_route) %in% unique(data_training$process_route)]
unique(data_prediction$mine_type_combined)[!unique(data_prediction$mine_type_combined) %in% unique(data_training$mine_type_combined)]
unique(data_prediction$by_prod_group)[!unique(data_prediction$by_prod_group) %in% unique(data_training$by_prod_group)]

fit_control <- trainControl(method = "repeatedcv",   
                            number = 10,     # number of folds
                            repeats = 10)    # repeated ten times

fm <- water_var ~ production + by_prod_group + process_route + mine_type_combined + ore_body_group

model_rf <- train(fm, data = data_training, method = "cforest",  trControl = fit_control)

ggplot(varImp(model_rf))

model_rf

data_prediction$water_var_pred <- predict(model_rf, data_prediction)

select(sf_data, id) |> 
  left_join(data_prediction) |> 
  ggplot(aes(x = water_var_pred, y = water_var, label = id)) + 
  geom_point() + 
    geom_abline(colour = "blue") + 
  geom_text(hjust=0, vjust=0, size = 3) +
  scale_x_continuous(limits = c(0, 9e4)) + 
  scale_y_continuous(limits = c(0, 9e4))

source("make_map.R")
cells_size <- 200000
year <- 2019

# prediction 
grid_cells <- s2_data_countries() |> 
  st_as_sf() |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  st_make_grid(cellsize = cells_size)

grid_cells <- select(sf_data, id) |> 
  left_join(filter(data_prediction, year == year)) |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  select(water_var_pred) |> 
  aggregate(grid_cells, sum, na.rm = TRUE) |> 
  filter(!is.na(water_var_pred), water_var_pred > 0)

plot_goode_homolosine_world_map(ocean_color = "#e5f1f8", land_color = "gray95", 
                                grid_color = "grey75", grid_size = 0.1,
                                country_borders_color = "grey75", country_borders_size = 0.1) +
  ggplot2::geom_sf(data = grid_cells, mapping = aes(fill = water_var_pred * 1e-6), color = NA, lwd = 0, size = 0) + # * 100 from km2 to ha
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
grid_cells <- s2_data_countries() |> 
  st_as_sf() |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  st_make_grid(cellsize = cells_size)

grid_cells <- select(sf_data, id) |> 
  left_join(filter(data_prediction, year == year)) |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  select(water_var_pred = water_var) |> 
  aggregate(grid_cells, sum, na.rm = TRUE) |> 
  filter(!is.na(water_var_pred), water_var_pred > 0)

plot_goode_homolosine_world_map(ocean_color = "#e5f1f8", land_color = "gray95", 
                                grid_color = "grey75", grid_size = 0.1,
                                country_borders_color = "grey75", country_borders_size = 0.1) +
  ggplot2::geom_sf(data = grid_cells, mapping = aes(fill = water_var_pred * 1e-6), color = NA, lwd = 0, size = 0) + # * 100 from km2 to ha
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
  labs(fill = bquote(Water~use~(M~m^3))) 

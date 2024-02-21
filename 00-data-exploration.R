library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(readxl)
library(car)
library(ggplot)
library(lme4)
library(lmerTest)
library(haven)
library(geepack)
library(lme4)
library(rstanarm)
library(bayesplot)
library(bayestestR)
raw_data <- read_excel('./data/Copper data for analysis_20230215.xlsx') |> 
  mutate(id = row_number())

# Create sf object 
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

ts_data <- select(raw_data, id, mine, process_route = `Process route`, 
                  `2015`, `2016`, `2017`, `2018`, `2019`, 
                  ToWa_2015, RaWa_2015, ToWa_2016, RaWa_2016, 
                  ToWa_2017, RaWa_2017, ToWa_2018, RaWa_2018, ToWa_2019, RaWa_2019) |> 
  pivot_longer(cols = c(-id, -mine, -process_route), names_to = "year", values_to = "value") |> 
  mutate(row_data = ifelse(str_detect(year, "ToWa"), "total_water", 
                           ifelse(str_detect(year, "RaWa"), "raw_water", "production")), 
         year = str_replace_all(year, "ToWa_", ""),
         year = str_replace_all(year, "RaWa_", ""),
         year = as.numeric(year)) |> 
  pivot_wider(id_cols = c(id, mine, process_route, year), names_from = row_data, values_from = value) |> 
  mutate(production = ifelse(production == 0, NA, production),
         total_water = ifelse(total_water == 0, NA, total_water),
         raw_water = ifelse(raw_water == 0, NA, raw_water),
         water_intensity = total_water / production) |> 
  mutate(process_route = ifelse(process_route=="Other", "other", process_route), process_route = factor(process_route))

ts_clean <- ts_data |> 
  drop_na() |> 
  left_join(select(st_drop_geometry(sf_data), id, mine, process_route, ore_body_group, by_prod_group, ore_grade, mine_type_combined, REG_TOP_20)) |> 
  mutate(process_route = ifelse(process_route=="pyro", "other", process_route), process_route = factor(process_route)) |> 
  mutate(water_intensity = total_water / production,
         mine_type_combined = ifelse(mine_type_combined=="Underground", "Combined", mine_type_combined))

outleirs_ids <- ts_clean |> 
  group_by(id) |> 
  summarise(diff_water_intensity = diff(range(water_intensity))) |> 
  arrange(desc(diff_water_intensity)) |> 
  filter(diff_water_intensity>0.2)

ts_data |> 
  filter(id %in% outleirs_ids$id) |> 
  View()

raw_data |> 
  filter(id %in% c(c(216, 326, 308, 317, 301) -1 )) |> 
  transmute(table_row = id + 1, mine, snl_id)

ts_clean |> 
  arrange(water_intensity) |> 
  View()

ts_clean |> 
  group_by(id) |> 
  summarise(water_intensity = diff(range(water_intensity))) |> 
  arrange(desc(water_intensity))

ts_clean |> 
  group_by(process_route) |> 
  summarise(n = n())

ts_clean |> 
  group_by(mine_type_combined) |> 
  summarise(n = n())

ts_clean |> 
  group_by(ore_body_group) |> 
  summarise(n = n())

ts_clean |> 
  group_by(by_prod_group) |> 
  summarise(n = n())

ts_data |> 
  group_by(process_route) |> 
  filter(process_route != 0) |> 
  summarise(n = n())

ts_data |> 
  filter(process_route != 0) |> 
  ggplot(aes(x = production, y = total_water, colour = process_route, label = id)) + 
  geom_point() + 
  geom_text(hjust=0, vjust=0, size = 3) + 
  theme(legend.position = "bottom")

ts_data |> 
  filter(process_route != 0) |> 
  group_by(id, mine, process_route) |> 
  summarise(production = mean(production, na.rm = TRUE), total_water = mean(total_water, na.rm = TRUE)) |> 
  ggplot(aes(x = production, y = total_water, colour = process_route, label = id)) + 
  geom_point() + 
  geom_text(hjust=0, vjust=0, size = 3) + 
  theme(legend.position = "bottom")

# Correlated observations 
long_data <- ts_clean |> 
  select(id, year, total_water, production, process_route, ore_body_group, by_prod_group, mine_type_combined) |> 
  filter(process_route != 0) |> 
  drop_na() |> 
  mutate(process_route = as.character(process_route),
         ore_body_group = as.character(ore_body_group), 
         by_prod_group = as.character(by_prod_group),
         mine_type_combined = as.character(mine_type_combined))  
  # group_by(id, process_route) |> 
  # summarise(production = mean(production, na.rm = TRUE), total_water = mean(total_water, na.rm = TRUE)) 

fm <- total_water ~ production + mine_type_combined + by_prod_group

glm(formula = fm, data = long_data) |> 
  summary()
  #broom::tidy()

geeglm(formula = fm, id = id, data = long_data, waves = year, corstr = "independence") |> 
  summary()
  #broom::tidy()

lm(formula = total_water ~ production + process_route + by_prod_group + mine_type_combined, data = long_data) |> 
  summary()

model <- stan_glm(fm, data = data_train)
print(model, digits = 3)
hdi(model)
eti(model)

#fit multiple linear regression model
model <- lm(total_water ~ production + raw_water, data = ts_clean)

#view results of model
summary(model)

#produce added variable plots
avPlots(model)

#fit multiple linear regression model
model <- lm(total_water ~ production + process_route, data = ts_clean)

#view results of model
summary(model)

#produce added variable plots
avPlots(model)


#fit multiple linear regression model
model <- lm(production ~ total_water + raw_water, data = ts_clean)

#view results of model
summary(model)

#produce added variable plots
avPlots(model)


#fit multiple linear regression model
model <- lm(total_water ~ production, data = ts_clean)

#view results of model
summary(model)

#produce added variable plots
avPlots(model)

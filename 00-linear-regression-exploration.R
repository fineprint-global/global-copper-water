library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(readxl)

raw_data <- read_excel('./data/Copper data for analysis_20240304.xlsx') |> 
  mutate(id = row_number())

sf_data <- select(raw_data, id, Longitude, Latitude, REG_TOP_20, mine, snl_id, country, country_code, region, cumulative_production, average_production, 
                  by_prod_group = `by-prod-group`, mine_type_combined, ore_body_group = `Ore Body Group`, process_route = `Process route`, 
                  ore_grade = `Ore grade MI`) |> 
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
  left_join(st_drop_geometry(sf_data))

# remove outliers # Keeping this mine reduces more than 30% of the model's predictability power 
# ts_data <- ts_data |>
#   filter(id != 300)

data_train <- ts_data |> 
  mutate(water_var = raw_water) |> # Select the independent ariable. Either raw_water or total_water
  filter(!is.na(water_var)) |> 
  select(id, year, production, water_var, 
         cumulative_production, average_production,
         by_prod_group, mine_type_combined,
         ore_body_group, process_route, mine_type_combined)

rm(models_tbl)
add_model <- function(models_tbl = NULL, name, md){
  if(is.null("models_tbl")){
    models_tbl <- tibble(name = name,
                          model = list(md),
                          rmse = sqrt(mean(md$residuals^2)),
                          adj.r.squared = summary(md)$adj.r.squared)
  } else {
    models_tbl <- tibble(name = name,
                          model = list(md),
                          rmse = sqrt(mean(md$residuals^2)),
                          adj.r.squared = summary(md)$adj.r.squared) |> 
      bind_rows(models_tbl) 
  }
  return(models_tbl)
}

################################################ Base model ~ production + mine_type_combined
fm <- water_var ~ production + mine_type_combined
md <- lm(fm, data = data_train)
summary(md)
models_tbl <- add_model(name = "production", md = md)

################################################ production + process_route
fm <- water_var ~ production + mine_type_combined + process_route
md <- lm(fm, data = data_train)
summary(md)
models_tbl <- add_model(models_tbl, name = "production+process_route", md = md)

################################################ production + ore_body_group
fm <- water_var ~ production + mine_type_combined + ore_body_group
md <- lm(fm, data = data_train)
summary(md)
models_tbl <- add_model(models_tbl, name = "production+ore_body_group", md = md)

################################################ production + by_prod_group
fm <- water_var ~ production + mine_type_combined + by_prod_group
md <- lm(fm, data = data_train)
summary(md)
models_tbl <- add_model(models_tbl, name = "production+by_prod_group", md = md)

################################################ production + process_route + by_prod_group
fm <- water_var ~ production + mine_type_combined + process_route + by_prod_group
md <- lm(fm, data = data_train)
summary(md)
models_tbl <- add_model(models_tbl, name = "production+process_route+by_prod_group", md = md)

################################################ production + process_route + ore_body_group + by_prod_group
fm <- water_var ~ production + mine_type_combined + process_route + ore_body_group + by_prod_group
md <- lm(fm, data = data_train)
summary(md)
models_tbl <- add_model(models_tbl, name = "production+process_route+ore_body_group+by_prod_group", md = md)

################################################ Compare models 
anova_pvalues <- sapply(models_tbl$model, function(md1){
  sapply(models_tbl$model, function(md2){
    anova(md1, md2)$`Pr(>F)`[2]
  })
})

rownames(anova_pvalues) <- models_tbl$name
colnames(anova_pvalues) <- models_tbl$name
anova_pvalues[lower.tri(anova_pvalues)] <- NA
View(anova_pvalues)
models_tbl

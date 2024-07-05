source("utils.R")

# Global copper mines - this dataset is not provided due to copyright restrictions
raw_data <- read_excel('./data/Copper data for analysis_20240503.xlsx') |> 
  dplyr::mutate(id_mine = row_number())

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

# Water Depletion from WaterGap3
if(!file.exists("data/WaterDepletion_WaterGap3.zip")){
  print("Download Water Depletion from WaterGap3")
  download.file("https://s3.us-east-2.amazonaws.com/earthstatdata/WaterDepletion_WaterGap3.zip", destfile = "data/WaterDepletion_WaterGap3.zip")
  unzip("data/WaterDepletion_WaterGap3.zip", exdir = "data/")
}

water_depletion <- rast("data/WaterDepletion_WaterGap3/WaterDepletionCat_WG3.tif") 

# CGMW Bedrock and Structural geology - BRGM Bureau de Recherches Géologiques et Minières https://portal.onegeology.org/OnegeologyGlobal
if(!file.exists("./data/geological_unit.gpkg")){
  print("Download CGMW Bedrock and Structural geology")
  geological_unit <- read_sf("http://mapsref.brgm.fr/wxs/1GG/CGMW_Bedrock_and_Structural_Geology?request=GetCapabilities&service=WFS", layer = "World_CGMW_50M_GeologicalUnitsOnshore")
  st_write(geological_unit, "./data/geological_unit.gpkg")
}
geological_unit <- read_sf("./data/geological_unit.gpkg")


# Download from: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download
# Population Density, v4.11 (2000, 2005, 2010, 2015, 2020), 30 Min (55 km)
if(file.exists("./data/gpw-v4-population-density-rev11_totpop_30_min_nc/gpw_v4_population_density_rev11_30_min.nc")){
 unzip("data/gpw-v4-population-density-rev11_totpop_30_min_nc.zip", exdir = "data/")
}
population_density <- rast("./data/gpw-v4-population-density-rev11_totpop_30_min_nc/gpw_v4_population_density_rev11_30_min.nc", lyrs = 1:5)
pop_density_slope <- app(population_density, calc_slope)
pop_density_mean <- app(population_density, "mean")

# Global Aridity Index and Potential Evapotranspiration (ET0) Database: Version 3
# Paper: https://www.nature.com/articles/s41597-022-01493-1
if(!file.exists("./data/ai_et0/Global-AI_ET0_v3_annual/ai_v3_yr.tif")){
  print("Download Global Aridity Index and Potential Evapotranspiration (ET0) Database: Version 3")
  options(timeout = 1000)
  download.file("https://figshare.com/ndownloader/articles/7504448/versions/5", destfile = "data/ai_et0.zip")
  options(timeout = 60)
  unzip("data/ai_et0.zip", files = "Global-AI_ET0_annual_v3.zip", exdir = "data/ai_et0")
  unzip("data/ai_et0/Global-AI_ET0_annual_v3.zip", exdir = "data/ai_et0")
  unlink(c("data/ai_et0.zip", "data/ai_et0/Global-AI_ET0_annual_v3.zip"))
}
ai_annual <- rast("./data/ai_et0/Global-AI_ET0_v3_annual/ai_v3_yr.tif")
et0_annual <- rast("./data/ai_et0/Global-AI_ET0_v3_annual/et0_v3_yr.tif")

# pre-process copper data
sf_data <- select(raw_data, id_mine, Longitude, Latitude, REG_TOP_20, mine, snl_id, country, country_code, region, cumulative_production, average_production, 
                  byproduct_group = `by-prod-group\ 2`, mine_type = mine_type_combined, ore_body_group = `Ore Body Group`, process_route = `Process route`, ore_grade = `Ore Grade_combined`) |> 
  dplyr::mutate(
    Latitude  = as.numeric(ifelse(Latitude == "Copper", 0, Latitude)),
    check_0  = Longitude == 0 & Latitude == 0, 
    Longitude = ifelse(check_0, NA, Longitude),
    Latitude  = ifelse(check_0, NA, Latitude)) |> 
  select(-check_0) |> 
  dplyr::filter(!is.na(Longitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

# add extended layers
sf_data$water_depletion <- extract(water_depletion, sf_data)[,2]
sf_data$freshwater_availability <- extract(freshwater_availability, sf_data)[,2]
sf_data$pop_density_mean <- extract(pop_density_mean, sf_data)[,2]
sf_data$pop_density_slope <- extract(pop_density_slope, sf_data)[,2]
sf_data$ai_annual <- extract(ai_annual, sf_data)[,2] * 0.0001
sf_data$et0_annual <- extract(et0_annual, sf_data)[,2] * 0.0001
sf_use_s2(FALSE)
sf_data <- st_join(sf_data, geological_unit)
sf_use_s2(TRUE)

ts_data <- select(raw_data, id_mine, `2015`, `2016`, `2017`, `2018`, `2019`, ToWa_2015, RaWa_2015, ToWa_2016, RaWa_2016, ToWa_2017, RaWa_2017, ToWa_2018, RaWa_2018, ToWa_2019, RaWa_2019) |> 
  pivot_longer(cols = -id_mine, names_to = "year", values_to = "value") |> 
  dplyr::mutate(row_data = ifelse(str_detect(year, "ToWa"), "total_water", 
                           ifelse(str_detect(year, "RaWa"), "raw_water", "production")), 
         year = str_replace_all(year, "ToWa_", ""),
         year = str_replace_all(year, "RaWa_", ""),
         year = as.numeric(year)) |>
  pivot_wider(id_cols = c(id_mine, year), names_from = row_data, values_from = value) |> 
  dplyr::mutate(production = ifelse(production == 0, NA, production),
         total_water = ifelse(total_water == 0, NA, total_water),
         raw_water = ifelse(raw_water == 0, NA, raw_water)) |> 
  left_join(st_drop_geometry(sf_data)) |> 
  dplyr::mutate(process_route = ifelse(process_route=="Other", "other", process_route),
         process_route = ifelse(process_route=="0", "other", process_route), 
         process_route = ifelse(process_route=="pyro/hydro", "pyro", process_route), 
         process_route = as.character(process_route),
         byproduct_group = as.character(byproduct_group),
         mine_type = tolower(as.character(mine_type)),
         ore_body_group = tolower(as.character(ore_body_group)),
         mine_type = ifelse(mine_type=="open pit", "pit", mine_type),
         byproduct_group = tolower(ifelse(byproduct_group == "CuCu", "CuNN", byproduct_group)),
         process_route = tolower(ifelse(process_route == "other", NA, process_route))) |>
  dplyr::mutate(id = row_number()) |>
  relocate(id, .before = id_mine)

# Check data availability
png(filename = "./results/data_availability.png",
    width = 300, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
ts_data |>
  select(total_water, raw_water, ore_body_group, process_route, mine_type, byproduct_group) |>
  pivot_longer(cols = -c(total_water, raw_water)) |> 
  pivot_longer(cols = c(total_water, raw_water), names_to = "water_use", values_to = "volume", values_drop_na = TRUE) |> 
  ggplot(aes(x = value, y = after_stat(count), fill = water_use)) + 
  facet_wrap(~name, scales = "free") + 
  geom_bar(stat = 'count', position = "dodge", width = 0.7) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.1, position = position_dodge(width = 0.7)) +
  scale_fill_grey(start = 0.8, end = 0.4) +
  theme_bw()
dev.off()

png(filename = "./results/data_strata_boxplot.png",
    width = 300, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
ts_data |>
  select(total_water, raw_water, ore_body_group, process_route, mine_type, byproduct_group) |>
  pivot_longer(cols = -c(total_water, raw_water)) |> 
  pivot_longer(cols = c(total_water, raw_water), names_to = "water_use", values_to = "volume", values_drop_na = TRUE) |> 
  ggplot(aes(x = value, y = volume, colour = water_use)) + 
  facet_wrap(~name, scales = "free") + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  theme_bw()
dev.off()

png(filename = "./results/data_distribution.png",
    width = 300, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
transmute(ts_data, production, ore_grade, raw_water, total_water, 
  water_diff = total_water - raw_water, freshwater_availability) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  drop_na() |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, alpha = 0.7) +  # Adjust 'bins' as necessary
  facet_wrap(~ variable, scales = "free") +  # Free scales if variables have different ranges
  theme_bw() +
  labs(title = "Distribution of Variables", x = "Value", y = "Frequency")
dev.off()

# ts_data |>
#   dplyr::group_by(ore_body_group) |>
#   summarise(production = sum(production, na.rm = TRUE)) |>
#   arrange(desc(production)) |>
#   dplyr::mutate(perc = production / sum(production), cum_perc = cumsum(production) / sum(production))

# ts_data |>
#   dplyr::group_by(process_route) |>
#   summarise(production = sum(production, na.rm = TRUE)) |>
#   arrange(desc(production)) |>
#   dplyr::mutate(perc = production / sum(production), cum_perc = cumsum(production) / sum(production))

# ggplot(ts_data, aes(x = ai_annual, y = raw_water)) +
#   geom_point()

write_csv2(ts_data, "./data/ts_data_raw.csv")
st_write(sf_data, "./data/sf_data_raw.gpkg", delete_dsn = TRUE)

# Prepare model data
model_data <- ts_data |>
  drop_na(c(production, ore_grade, mine_type, byproduct_group, process_route)) |>
  select(id, raw_water, total_water, production, ore_grade, mine_type, byproduct_group, process_route)

write_csv2(model_data, "./data/ts_model_data.csv")

# Fill predictors gaps using knn
pred_data <- select(ts_data, id, production, total_water, raw_water, country_code, region, cumulative_production, average_production, byproduct_group, mine_type, ore_body_group, process_route, ore_grade) |>
  mutate_if(is.character, as.factor)
pred_data <- recipe(raw_water + total_water ~ ., data = pred_data) |>
  step_impute_knn(any_of(c("production", "ore_grade", "mine_type", "byproduct_group", "process_route")), impute_with = imp_vars(-any_of(c("id"))), neighbors = 5,) |>
  prep() |>
  bake(pred_data) |> 
  select(id, raw_water, total_water, production, ore_grade, mine_type, byproduct_group, process_route)

write_csv2(pred_data, "./data/ts_pred_data.csv")


################################################################################
######## Check outliers

# Fit a linear model
model_data <- select(ts_data, id, raw_water, production, ore_grade, process_route, mine_type, ore_body_group, byproduct_group) |>
  drop_na()

model <- lm(raw_water ~ production + ore_grade + process_route + mine_type + ore_body_group + byproduct_group, data = ts_data)

# Calculate residuals
model_data$residuals <- abs(residuals(model))

# Identify large residuals
outlier_threshold <- mean(model_data$residuals) + 2 * sd(model_data$residuals)
outliers <- model_data$residuals > outlier_threshold
outlier_data <- model_data[outliers, ]

# Cook's Distance
model_data$cooks_distance <- cooks.distance(model)

# Identify influential points
influential_threshold <- 4 / nrow(model_data)
influential_points <- model_data$cooks_distance > influential_threshold

# Calculate Z-scores
ts_data <- ts_data |>
  dplyr::mutate(raw_water_z = scale(raw_water, center = TRUE, scale = TRUE)[,1])

# Identify outliers using Z-score
outliers_z <- ts_data |>
  dplyr::filter(abs(raw_water_z) > 3)

# Calculate IQR and identify outliers
iqr_value <- IQR(ts_data$raw_water, na.rm = TRUE)
q1 <- quantile(ts_data$raw_water, 0.25, na.rm = TRUE)
q3 <- quantile(ts_data$raw_water, 0.75, na.rm = TRUE)

# Choose a multiplier
multiplier <- 2  # Less strict; adjust based on your data and needs

outliers_iqr <- dplyr::filter(ts_data, raw_water < (q1 - multiplier * iqr_value) | raw_water > (q3 + multiplier * iqr_value))

outliers_iqr


rm(list = ls())
gc(reset = TRUE, full = TRUE)

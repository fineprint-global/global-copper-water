# ---
# Script: 00-prepare-data.R
# Purpose: This script serves as the primary data ingestion and preprocessing
#          pipeline for the entire analysis. Its main goal is to collect, clean,
#          and integrate various datasets to create a single, comprehensive
#          input for the subsequent modeling phases.
#
# Key Steps:
# 1. Data Ingestion: Loads the raw, proprietary copper mine data from an
#    Excel file and assigns a unique identifier to each mine.
# 2. Geospatial and Environmental Data Integration: Downloads and merges several
#    publicly available geospatial datasets, including GRACE (for freshwater availability trends),
#    WaterGap3 (for water depletion), WRI Aqueduct (for water risk), and
#    geological data. This enriches the mine data with crucial environmental context.
# 3. Time-Series Transformation: Pivots the raw data from a wide format to a
#    long format, creating a tidy time-series dataset for each mine and year.
# 4. Feature Engineering and Imputation: Calculates new metrics such as water intensity.
#    It also cleans and harmonizes categorical variables and uses k-nearest neighbors (KNN)
#    imputation to fill in missing values in key predictors like production and ore grade.
# 5. Output: Saves the final, prepared datasets for use in the next scripts.
# ---

source("utils.R")

# Global copper mines - this dataset is not provided due to copyright restrictions
raw_data <- read_excel('./data/copper_data_20250227.xlsx') |> 
  dplyr::mutate(id_mine = row_number()) # add an unique id for each mine

### Add GRACE - Trends in Global Freshwater Availability from the Gravity Recovery and Climate Experiment (GRACE), v1 (2002 – 2016)
#   Rodell, M., J. S. Famiglietti, D. N. Wiese, J. T. Reager, H. K. Beaudoing, F. W. Landerer, and M.-H. Lo. 2019. Trends in Global Freshwater Availability from the Gravity Recovery and Climate Experiment (GRACE). Palisades, New York: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4TT4P2C.
#   Rodell, M., J. S. Famiglietti, D. N. Wiese, J. T. Reager, H. K. Beaudoing, F. W. Landerer, and M.-H. Lo. 2018. Emerging Trends in Global Freshwater Availability. Nature 557(7707): 651-659. https://doi.org/10.1038/s41586-018-0123-1.
# The Trends in Global Freshwater Availability from the Gravity Recovery and Climate Experiment (GRACE), 2002-2016, is a global gridded data set at a spatial resolution of 0.5 degrees that presents trends (rate of change measured in centimeters per year) in freshwater availability based on data obtained from 2002 to 2016 by NASA GRACE. Terrestrial water availability storage is the sum of groundwater, soil moisture, snow and ice, surface waters, and wet biomass, expressed as an equivalent height of water. GRACE measures changes in the terrestrial water cycle by assessing small changes in Earth's gravity field. This observation-based assessment of how the world's water cycle is responding to human impacts and climate variations provides an important tool for evaluating and predicting emerging threats to water and food security.
# Dowanload requires login to Earthdata. Direct links:
# Trends: https://sedac.ciesin.columbia.edu/downloads/data/sdei/sdei-trends-freshwater-availability-grace/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip
# Water mask: https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-data-quality-indicators-rev11/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip

if(file.exists("data/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip")){
 unzip("data/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip", exdir = "data/")
} else {
 stop("Dowanload requires login to Earthdata. Download trend data from: https://sedac.ciesin.columbia.edu/downloads/data/sdei/sdei-trends-freshwater-availability-grace/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip")
}
freshwater_availability <- rast("data/freshwater_availability.tif")

if(file.exists("data/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip")){
 unzip("data/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip", exdir = "data/")
} else {
stop("Dowanload requires login to Earthdata. Download water mask data from: https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-data-quality-indicators-rev11/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip")
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


###### AWARE
if(!file.exists("./data/aware.gpkg")){
  print("Download AWARE index")
  download.file("https://drive.usercontent.google.com/download?id=17HEOzLgVwWurZts1CkqYpX_tJw9WJ6kR&export=download&authuser=0", destfile = "data/aware.zip")
  unzip("data/aware.zip", exdir = "data/aware")
  aware <- read_sf("data/aware/AWARE_v1_2April_7th.kmz")
  aware_entries <- st_drop_geometry(aware) |>
    as_tibble() |>
    rowwise() |>
    mutate(data = list(extract_aware_values(description))) |>
    select(data) |>
    unnest_wider(data)
  names(aware_entries) <- c("aware_consumption_m3", "aware_annual_agri", "aware_annual_non_agri", "aware_annual_unknown")
  aware <- bind_cols(select(aware, geom = geometry), aware_entries)
  st_write(aware, "./data/aware.gpkg")
}
aware <- read_sf("./data/aware.gpkg")

###### aqueduct
if(!file.exists("./data/aqueduct.gpkg")){
  options(timeout = 300) 
  print("Download WRI aqueduct")
  download.file("https://files.wri.org/aqueduct/aqueduct-4-0-water-risk-data.zip", destfile = "data/aqueduct.zip")
  unzip("data/aqueduct.zip", exdir = "data/aqueduct")
  aqueduct <- read_sf("data/aqueduct/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gdb", layer = "baseline_annual")
  select(aqueduct, aqueduct_bws_raw = bws_raw, aqueduct_bws_score = bws_score, aqueduct_bws_cat = bws_cat, aqueduct_bws_label = bws_label) |>
    st_write("./data/aqueduct.gpkg")
}
aqueduct <- read_sf("./data/aqueduct.gpkg")

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
    Latitude  = ifelse(check_0, NA, Latitude),
    ore_grade = as.numeric(ifelse(ore_grade == "NA", "", ore_grade))) |> 
  select(-check_0) |> 
  dplyr::filter(!is.na(Longitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

# add extended layers
sf_data$water_depletion <- extract(water_depletion, sf_data)[,2]
sf_data$freshwater_availability <- extract(freshwater_availability, sf_data)[,2]
sf_data$pop_density_mean <- extract(pop_density_mean, sf_data)[,2]
sf_data$pop_density_slope <- extract(pop_density_slope, sf_data)[,2]
sf_data$ai_annual <- extract(ai_annual, sf_data)[,2] * 0.0001 # correct scale
sf_data$et0_annual <- extract(et0_annual, sf_data)[,2] * 0.0001 # correct scale
sf_use_s2(FALSE)
sf_data <- st_join(sf_data, geological_unit)
sf_data <- st_join(sf_data, aware)
sf_data <- st_join(sf_data, aqueduct)
sf_use_s2(TRUE)

ts_data <- select(raw_data, id_mine, `Prod_2015`, `Prod_2016`, `Prod_2017`, `Prod_2018`, `Prod_2019`, ToWa_2015, RaWa_2015, ToWa_2016, RaWa_2016, ToWa_2017, RaWa_2017, ToWa_2018, RaWa_2018, ToWa_2019, RaWa_2019) |> 
  mutate(across(all_of(starts_with("Prod_20")), ~ as.numeric(ifelse(.x == "NA", "", .x)))) |>
  pivot_longer(cols = -id_mine, names_to = "year", values_to = "value") |> 
  dplyr::mutate(row_data = ifelse(str_detect(year, "ToWa"), "total_water", 
                           ifelse(str_detect(year, "RaWa"), "raw_water", "production")), 
         year = str_replace_all(year, "ToWa_", ""),
         year = str_replace_all(year, "RaWa_", ""),
         year = str_replace_all(year, "Prod_", ""),
         year = as.numeric(year)) |>
  pivot_wider(id_cols = c(id_mine, year), names_from = row_data, values_from = value) |> 
  dplyr::mutate(
         total_water = ifelse(total_water == 0, NA, total_water),
         raw_water = ifelse(raw_water == 0, NA, raw_water)
  ) |> 
  left_join(st_drop_geometry(sf_data)) |> 
  dplyr::mutate(process_route = ifelse(process_route=="Other", "other", process_route), # harmonize to lowercase other 
         process_route = ifelse(process_route=="0", "other", process_route), # harmonize zero to other 
         process_route = ifelse(process_route=="pyro/hydro", "pyro", process_route), # merge "pyro/hydro" with "pyro" since there are not sufficient samples for "pyro/hydro"
         process_route = as.character(process_route),
         byproduct_group = as.character(byproduct_group),
         mine_type = tolower(as.character(mine_type)),
         ore_body_group = tolower(as.character(ore_body_group)),
         mine_type = ifelse(mine_type=="open pit", "pit", mine_type), # simplify pit category
         process_route = tolower(ifelse(process_route == "other", NA, process_route)), # replace other with NA since the data is actually missing
         average_production = ifelse(average_production==0,NA,average_production)) |> # replace 0 with NA since they are actually missing data not zero
  dplyr::mutate(id = row_number()) |> # add an unique id for each observation
  relocate(id, .before = id_mine)

# Check data availability
png(filename = "./results/data_availability.png",
    width = 300, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
ts_data |>
  select(`Total Water` = total_water, `New Water`= raw_water, ore_body_group, process_route, mine_type, byproduct_group, production_available = production, ore_grade_available = ore_grade) |>
  mutate(production_available = as.character(!is.na(production_available)), ore_grade_available = as.character(!is.na(ore_grade_available))) |>
  pivot_longer(cols = -c(`Total Water`, `New Water`)) |> 
  pivot_longer(cols = c(`Total Water`, `New Water`), names_to = "water_use", values_to = "volume", values_drop_na = TRUE) |> 
  ggplot(aes(x = value, y = after_stat(count), fill = water_use)) + 
  facet_wrap(~name, scales = "free") + 
  geom_bar(stat = 'count', position = "dodge", width = 0.7) + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1, position = position_dodge(width = 0.7)) +
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

write_csv2(ts_data, "./data/ts_data_raw.csv")
st_write(sf_data, "./data/sf_data_raw.gpkg", delete_dsn = TRUE)

# Prepare model data
model_data <- select(ts_data, id, id_mine, mine, country_code, year, raw_water, total_water, production, average_production, ore_grade, byproduct_group, mine_type, ore_body_group, process_route, et0_annual) |>
  mutate_if(is.character, as.factor) |>
  dplyr::group_by(id_mine) |>
  dplyr::mutate(average_production = mean(production, na.rm = TRUE), production = ifelse(is.na(production), average_production, production)) |>
  dplyr::mutate(total_water = ifelse(total_water < raw_water, raw_water, total_water)) |>
  dplyr::ungroup() |>
  dplyr::mutate(outlier_flag = ifelse(((raw_water / production) < 0.01 | (raw_water / production) > 40) | 
                                      ((total_water / production) < 0.01 | (total_water / production) > 40), 1, 0))

# Check water outliers and set to NA variables that are probably incorrect
outliers_ids <- dplyr::filter(model_data, id_mine %in% unique(as.character(dplyr::filter(model_data, outlier_flag == 1)$id_mine)))$id
model_data |>
  dplyr::filter(id %in% outliers_ids) |>
  select(id, id_mine, mine, country_code, year, raw_water, total_water, production, average_production) |>
  print(n = 40)
  
model_data <- model_data |>
  select(-outlier_flag)

model_data |>
  dplyr::mutate(
    raw_water_intensity = raw_water / production,
    total_water_intensity = total_water / production,
    ore_extracted = 100 * production / ore_grade,
    ratio_raw_water_ore = raw_water / ore_extracted,
    ratio_total_water_ore = total_water / ore_extracted) |>
    write_csv2("./data/ts_model_data.csv")

# Fill predictors gaps using knn
names(model_data)
pred_data <- recipe(raw_water + total_water ~ ., data = model_data) |>
  step_impute_knn(any_of(c("production", "ore_grade", "mine_type", "process_route")),
                  impute_with = imp_vars(any_of(c("id", "mine", "year", "et0_annual"))), neighbors = 1,) |>
  prep() |>
  bake(model_data) |>
  dplyr::group_by(id_mine) |>
  dplyr::mutate(average_production = mean(production, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    raw_water_intensity = raw_water / production,
    total_water_intensity = total_water / production,
    ore_extracted = 100 * production / ore_grade,
    ratio_raw_water_ore = raw_water / ore_extracted,
    ratio_total_water_ore = total_water / ore_extracted)

# aux <- model_data |>
#   select(id, id_mine, mine, country_code, year, production, average_production, ore_grade, mine_type, process_route) |>
#   filter(if_any(everything(), is.na))

# pred_data |>
#   dplyr::filter(id_mine %in% unique(aux$id_mine)) |>
#   select(id, id_mine, mine, country_code, year, raw_water, total_water, production, average_production, ore_grade, mine_type, process_route) |>
#   View()

write_csv2(pred_data, "./data/ts_pred_data.csv")

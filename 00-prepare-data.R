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
model_data_dir <- file.path("model_data", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(model_data_dir, recursive = TRUE)
message("Model data will be saved to: ", model_data_dir)

# Global copper mines - this dataset is not provided due to copyright restrictions
raw_data <- read_excel('./data/copper_data_20260302.xlsx') |> 
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
  download.file("https://storage.googleapis.com/earthstat/WaterDepletion_WaterGap3.zip", destfile = "data/WaterDepletion_WaterGap3.zip")
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

# Global Aridity Index and Potential Evapotranspiration (ET0) Database: Version 3
# Paper: https://www.nature.com/articles/s41597-022-01493-1
if(!file.exists("./data/ai_et0/Global-AI_ET0_v3_annual/ai_v3_yr.tif")){
  print("Download Global Aridity Index and Potential Evapotranspiration (ET0) Database: Version 3")
  options(timeout = 1000)
  download.file("https://figshare.com/ndownloader/articles/7504448/versions/5", destfile = "data/ai_et0.zip", mode = "wb", headers = c("User-Agent" = "Mozilla/5.0"))
  options(timeout = 60)
  unzip("data/ai_et0.zip", files = "Global-AI_ET0_annual_v3.zip", exdir = "data/ai_et0")
  unzip("data/ai_et0/Global-AI_ET0_annual_v3.zip", exdir = "data/ai_et0")
  unlink(c("data/ai_et0.zip", "data/ai_et0/Global-AI_ET0_annual_v3.zip"))
}
ai_annual <- rast("./data/ai_et0/Global-AI_ET0_v3_annual/ai_v3_yr.tif")
et0_annual <- rast("./data/ai_et0/Global-AI_ET0_v3_annual/et0_v3_yr.tif")

# pre-process copper data
sf_data <- select(
  raw_data, id_mine, Longitude, Latitude, mine, snl_id, country, country_code, region, cumulative_production, average_production, byproduct_group = `by-prod-group\ 2`, 
  mine_type = mine_type_combined, ore_body_group = `Ore Body Group Origin`,
  process_route = `Process route`, ore_grade = `Ore Grade_combined`) |> 
  dplyr::filter(!(is.na(Longitude)|is.na(Latitude))) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

# add extended layers
sf_data$water_depletion <- extract(water_depletion, sf_data)[,2]
sf_data$freshwater_availability <- extract(freshwater_availability, sf_data)[,2]
sf_data$ai_annual <- extract(ai_annual, sf_data)[,2] * 0.0001 # correct scale
sf_data$et0_annual <- extract(et0_annual, sf_data)[,2] * 0.0001 # correct scale
sf_use_s2(FALSE)
sf_data <- st_join(sf_data, geological_unit)
sf_data <- st_join(sf_data, aware)
sf_data <- st_join(sf_data, aqueduct)
sf_use_s2(TRUE)

# ---- TerraClimate climate predictors (year-specific, per mine) ----
# Variables: ppt (mm, annual total), aet (mm, annual total — actual ET)
# Source: TerraClimate ~4 km monthly (Abatzoglou et al. 2018, Sci Data)
# Strategy: download one NetCDF file per variable per year (~40 MB each, 10 files
# total), cached under data/terraclimate/. terra::extract on 500 points is
# instantaneous. Files are permanent — subsequent runs skip all downloads.

terraclim_cache <- "data/terraclimate_mines.csv"

if (!file.exists(terraclim_cache)) {
  nc_dir   <- "data/terraclimate"
  dir.create(nc_dir, showWarnings = FALSE)
  base_url <- "https://climate.northwestknowledge.net/TERRACLIMATE-DATA"
  years    <- 2015:2019
  vars     <- c("ppt", "aet")

  climate_list <- lapply(years, function(yr) {
    vals <- lapply(vars, function(v) {
      nc_file <- file.path(nc_dir, sprintf("TerraClimate_%s_%d.nc", v, yr))
      if (!file.exists(nc_file)) {
        url <- sprintf("%s/TerraClimate_%s_%d.nc", base_url, v, yr)
        message(sprintf("Downloading %s ...", basename(url)))
        options(timeout = 600)
        download.file(url, destfile = nc_file, mode = "wb", quiet = TRUE)
        options(timeout = 60)
      }
      r    <- terra::rast(nc_file)           # 12 monthly layers
      extr <- terra::extract(r, sf_data, ID = FALSE)
      rowSums(extr, na.rm = TRUE)            # annual total (mm)
    })
    names(vals) <- vars
    tibble(
      id_mine       = sf_data$id_mine,
      year          = yr,
      annual_ppt_mm = vals[["ppt"]],
      annual_aet_mm = vals[["aet"]]
    )
  })

  climate_df <- bind_rows(climate_list)
  write_csv(climate_df, terraclim_cache)
  message("TerraClimate data cached to: ", terraclim_cache)
} else {
  climate_df <- read_csv(terraclim_cache, show_col_types = FALSE)
}

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
         total_water = ifelse(total_water == 0, NA, total_water), # replace 0 with NA since they are actually missing data not zero
         raw_water = ifelse(raw_water == 0, NA, raw_water) # replace 0 with NA since they are actually missing data not zero
  ) |> 
  left_join(st_drop_geometry(sf_data)) |> 
  dplyr::mutate(
         process_route = ifelse(process_route%in%c("Other", "other", "0"), NA, process_route), # replace other with NA since the data is actually missing
         # Merge "pyro" and "pyro/hydro" into a single "pyro_based" category.
         # Both routes involve pyrometallurgical smelting; combined they have n=91
         # (vs. n=6 and n=70 separately), which is sufficient to estimate a route
         # effect robustly within the small training set.
         process_route = case_when(
           process_route %in% c("pyro", "pyro/hydro") ~ "pyro_based",
           TRUE ~ process_route  # preserves concentrator, hydro, and NA
         ),
         process_route = as.character(process_route),
         byproduct_group = as.character(byproduct_group),
         byproduct_group = ifelse(byproduct_group=="CuCu", "yes", "no"),
         mine_type = tolower(as.character(mine_type)),
         ore_body_group = tolower(as.character(ore_body_group)),
         mine_type = ifelse(mine_type=="open pit", "pit", mine_type) # simplify pit category
         ) |> 
  rename(byproduct_production = byproduct_group) |>
  group_by(id_mine) |>
  mutate(average_production = ifelse(average_production==0|is.na(average_production), mean(production, na.rm = TRUE), average_production)) |>
  ungroup() |>
  left_join(climate_df, by = c("id_mine", "year")) |>
  dplyr::mutate(id = row_number()) |> # add an unique id for each observation
  relocate(id, .before = id_mine)

# Check data availability
png(filename = file.path(model_data_dir, "data_availability.png"),
    width = 300, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
print(
  ts_data |>
    select(`Total Water` = total_water, `New Water`= raw_water, ore_body_group, process_route, mine_type, byproduct_production, production_available = production, ore_grade_available = ore_grade) |>
    mutate(production_available = as.character(!is.na(production_available)), ore_grade_available = as.character(!is.na(ore_grade_available))) |>
    pivot_longer(cols = -c(`Total Water`, `New Water`)) |>
    pivot_longer(cols = c(`Total Water`, `New Water`), names_to = "water_use", values_to = "volume", values_drop_na = TRUE) |>
    ggplot(aes(x = value, y = after_stat(count), fill = water_use)) +
    facet_wrap(~name, scales = "free") +
    geom_bar(stat = 'count', position = "dodge", width = 0.7) +
    geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1, position = position_dodge(width = 0.7)) +
    scale_fill_grey(start = 0.8, end = 0.4) +
    xlab("") +
    ylab("Count") +
    theme_bw()
)
dev.off()

png(filename = file.path(model_data_dir, "data_strata_boxplot.png"),
    width = 300, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
print(
  ts_data |>
    select(total_water, raw_water, ore_body_group, process_route, mine_type, byproduct_production) |>
    pivot_longer(cols = -c(total_water, raw_water)) |>
    pivot_longer(cols = c(total_water, raw_water), names_to = "water_use", values_to = "volume", values_drop_na = TRUE) |>
    ggplot(aes(x = value, y = log10(volume), colour = water_use)) +
    facet_wrap(~name, scales = "free") +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE) +
    scale_colour_grey(start = 0.8, end = 0.2) +
    theme_bw()
)
dev.off()

png(filename = file.path(model_data_dir, "data_distribution.png"),
    width = 300, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
print(
  transmute(ts_data, production, ore_grade, raw_water, total_water,
    water_diff = total_water - raw_water, freshwater_availability) |>
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
    drop_na() |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, alpha = 0.7) +
    facet_wrap(~ variable, scales = "free") +
    theme_bw() +
    labs(title = "Distribution of Variables", x = "Value", y = "Frequency")
)
dev.off()

write_csv2(ts_data, file.path(model_data_dir, "ts_data_raw.csv"))
st_write(sf_data, file.path(model_data_dir, "sf_data_raw.gpkg"), delete_dsn = TRUE)

# Prepare model data
model_data <- select(ts_data, id, id_mine, mine, country_code, year, raw_water, total_water, production, average_production, ore_grade, byproduct_production, mine_type, ore_body_group, process_route, annual_ppt_mm, annual_aet_mm) |>
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
  select(id, id_mine, mine, country_code, year, raw_water, total_water, production, average_production, byproduct_production) |>
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
    write_csv2(file.path(model_data_dir, "ts_model_data.csv"))

# ---------------------------------------------------------------------------
# Fill predictor gaps for prediction data
# ---------------------------------------------------------------------------
# Strategy:
#   production     : within-mine mean already applied above (line ~231).
#                    The 5 remaining NAs (1 mine, all years missing) cannot be
#                    recovered and are excluded downstream by filter(production > 0).
#   ore_grade      : continuous, 2.9% missing (15 mines). Imputed with median
#                    stratified by ore_body_group, falling back to country_code
#                    median, then global median. Small gap; stratified median is
#                    transparent, non-parametric, and robust to skew.
#   process_route,
#   ore_body_group : categorical, 18-20% missing. NA is NOT imputed here.
#                    Imputing from weak features (geography, mine type) would
#                    inject noise and conflict with step_unknown() in the model
#                    recipes, which correctly treats "unknown" as an informative
#                    category that the model can learn from.
#   mine_type      : 0% missing — no action needed.
# ---------------------------------------------------------------------------

# Record missingness before imputation
imp_vars_check <- c("production", "ore_grade", "process_route", "ore_body_group")
imp_before <- sapply(imp_vars_check, function(v) sum(is.na(model_data[[v]])))

pred_data <- model_data |>
  # ore_grade: stratified median imputation
  dplyr::group_by(ore_body_group) |>
  dplyr::mutate(ore_grade = ifelse(is.na(ore_grade),
                                   median(ore_grade, na.rm = TRUE),
                                   ore_grade)) |>
  dplyr::ungroup() |>
  dplyr::group_by(country_code) |>
  dplyr::mutate(ore_grade = ifelse(is.na(ore_grade),
                                   median(ore_grade, na.rm = TRUE),
                                   ore_grade)) |>
  dplyr::ungroup() |>
  dplyr::mutate(ore_grade = ifelse(is.na(ore_grade),
                                   median(ore_grade, na.rm = TRUE),
                                   ore_grade)) |>
  # Recalculate average_production and derived intensity fields
  dplyr::group_by(id_mine) |>
  dplyr::mutate(average_production = ifelse(
    average_production == 0 | is.na(average_production),
    mean(production, na.rm = TRUE), average_production)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    raw_water_intensity   = raw_water   / production,
    total_water_intensity = total_water / production,
    ore_extracted         = 100 * production / ore_grade,
    ratio_raw_water_ore   = raw_water   / ore_extracted,
    ratio_total_water_ore = total_water / ore_extracted)

# Imputation statistics
imp_after <- sapply(imp_vars_check, function(v) sum(is.na(pred_data[[v]])))
imp_stats <- tibble(
  variable         = imp_vars_check,
  n_missing_before = as.integer(imp_before),
  n_imputed        = as.integer(imp_before - imp_after),
  n_still_missing  = as.integer(imp_after),
  method           = c(
    "within-mine mean (applied above; residual NAs excluded downstream)",
    "stratified median: ore_body_group -> country_code -> global",
    "none: NA retained as 'unknown' category (step_unknown in model recipe)",
    "none: NA retained as 'unknown' category (step_unknown in model recipe)"
  )
)
message("Predictor imputation summary:")
print(imp_stats)
write_csv(imp_stats, file.path(model_data_dir, "imputation_stats.csv"))

write_csv2(pred_data, file.path(model_data_dir, "ts_pred_data.csv"))

# ---
# Script: 03-water-use-analysis.R
# Purpose: Geospatial and temporal analysis of predicted copper-mine water use.
#
# This script reads the comprehensive final_predictions.csv produced by
# 02-predict-water-use.R (which includes ML predictions, uncertainty columns,
# and Bayesian slope estimates) and performs:
#
# 1. Summary statistics: water-use intensity benchmarks (m3/t ore, m3/t Cu).
# 2. Spatial mapping: aggregate predicted water use to a 100-km grid and
#    generate global maps for each year (2015-2019).
# 3. Quadrant analysis: classify mines by the sign of their raw water use slope
#    and local freshwater availability trend; identify "stress-zone" mines
#    (Q4: increasing water use + declining freshwater availability).
# 4. Data products: export site-level water use and slope/quadrant tables.
#
# All outputs are saved to the same versioned results directory as the
# trained models (auto-detected as the most recent results/YYYYMMDD_HHMMSS/).
# ---


source("utils.R")

# ---------------------------------------------------------------------------
# Auto-detect latest versioned directories
# ---------------------------------------------------------------------------
ts_pattern     <- "\\d{8}_\\d{6}$"
results_dir    <- tail(sort(Filter(function(x) grepl(ts_pattern, x),
                                   list.dirs("./results",    recursive = FALSE))), 1)
model_data_dir <- tail(sort(Filter(function(x) grepl(ts_pattern, x),
                                   list.dirs("./model_data", recursive = FALSE))), 1)
message("Results dir : ", results_dir)
message("Model data  : ", model_data_dir)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
# final_predictions.csv contains all predictions, uncertainty, spatial
# attributes, and Bayesian slope estimates from 02-predict-water-use.R.
water_data <- read_csv2(file.path(results_dir, "final_predictions.csv"),
                        show_col_types = FALSE) |>
  # Use predicted values as the primary water-use estimate; convert ML -> Mm3.
  # rw_filled / tw_filled apply observed values where available.
  dplyr::mutate(
    raw_water   = rw_pred * 1e-3,    # ML -> Mm3  (predictions, for map consistency)
    total_water = tw_pred * 1e-3     # ML -> Mm3
  )

sf_data <- st_read(file.path(model_data_dir, "sf_data_raw.gpkg"), quiet = TRUE)

# Mine-level slope table: one row per mine, from Bayesian estimation in script 02
trend_by_mine <- water_data |>
  select(id_mine, rw_median_slope, rw_ci_lower, rw_ci_upper,
         rw_prob_gt0, rw_prob_lt0, ci_width, rw_slope_uncertainty) |>
  distinct(id_mine, .keep_all = TRUE)

################################################################################
####
####  1. SUMMARY STATISTICS
####
################################################################################

# Raw water intensity benchmark: literature suggests 0.45–0.60 m3 per tonne ore.
# Here we compare aggregate predicted raw water against that range.
intensity_check <- water_data |>
  filter(production > 0, ore_grade > 0) |>
  transmute(
    ore_extracted_t  = 100 * production / ore_grade,   # tonnes of ore
    tw_lw_Mm3        = ore_extracted_t * 0.45 * 1e-6,  # lower bound
    tw_up_Mm3        = ore_extracted_t * 0.60 * 1e-6,  # upper bound
    raw_water_Mm3    = raw_water
  ) |>
  drop_na() |>
  summarise(across(everything(), sum))

message("Raw water intensity benchmark (aggregate Mm3):")
print(intensity_check)

# Raw water intensity per tonne of ore (m3/t) and per tonne of copper (m3/t)
rw_int_ore <- water_data |>
  filter(production > 0, ore_grade > 0) |>
  transmute(rw_int_m3_per_t_ore = raw_water / (100 * production / ore_grade) * 1e6) |>
  drop_na()

rw_int_cu <- water_data |>
  filter(production > 0) |>
  transmute(rw_int_m3_per_t_cu = raw_water / production * 1e6) |>
  drop_na()

message("Raw water intensity (m3 / tonne ore):")
print(summary(rw_int_ore))
message("Raw water intensity (m3 / tonne Cu):")
print(summary(rw_int_cu))

write_csv(
  bind_cols(intensity_check,
            tibble(rw_int_m3_per_t_ore_median = median(rw_int_ore$rw_int_m3_per_t_ore, na.rm = TRUE),
                   rw_int_m3_per_t_cu_median  = median(rw_int_cu$rw_int_m3_per_t_cu,   na.rm = TRUE))),
  file.path(results_dir, "water_intensity_summary.csv")
)

################################################################################
####
####  2. SPATIAL MAPS
####
################################################################################

# Base world map in Goode homolosine projection (equal-area, minimal distortion)
bg_map <- plot_goode_homolosine_world_map(
  ocean_color            = NA,
  land_color             = "gray95",
  family                 = "Sans",
  grid_color             = "grey75",
  grid_size              = 0.1,
  country_borders_color  = "grey75",
  country_borders_size   = 0.1
) +
  ggplot2::coord_sf(crs = "+proj=igh", expand = FALSE) +
  theme(
    plot.title          = element_text(hjust = 0.1, vjust = -15),
    plot.title.position = "plot",
    legend.spacing.x    = unit(1.0, "cm"),
    legend.position     = "bottom",
    legend.direction    = "horizontal",
    legend.justification = "center",
    legend.box.spacing  = unit(0.0, "cm"),
    legend.key.size     = unit(0.3, "cm"),
    legend.key.width    = unit(345 / 15, "mm"),
    plot.margin         = unit(c(0, 0, 0, 0), "cm")
  )

# Aggregate site-level predictions to a 100-km grid for mapping.
# Each grid cell shows the summed water use of all mines within it.
map_data_wide <- left_join(
    select(sf_data, id_mine),
    select(water_data, id_mine, year, total_water, raw_water),
    by = "id_mine"
  ) |>
  make_water_use_grid(cell_size = units::set_units(100, km))

map_data_long <- map_data_wide |>
  pivot_longer(
    cols      = starts_with("total_water") | starts_with("raw_water"),
    names_to  = c(".value", "year"),
    names_sep = "\\."
  )

# Derive a shared log1p scale with rounded, human-readable break labels.
round_pretty <- function(x) {
  x <- round(expm1(pretty(log1p(x))))
  ifelse(x >= 10, round(x / 10) * 10, x) |> unique()
}
rw_labels <- round_pretty(map_data_long$raw_water)
tw_labels <- round_pretty(map_data_long$total_water)

# Helper: build one annual map panel
make_year_map <- function(df_year, y, fill_var, labels, title_expr) {
  bg_map +
    geom_sf(data     = df_year,
            aes(fill = log1p(.data[[fill_var]])),
            color = NA, lwd = 0, size = 0) +
    scale_fill_viridis(
      option    = "turbo",
      begin     = 0, end = 1, direction = 1,
      breaks    = log1p(labels),
      labels    = labels,
      limits    = range(log1p(labels)),
      name      = expression("Million " * m^3)
    ) +
    labs(title = bquote(.(y)))
}

# ---- Raw water maps (2015–2019) ----
rw_maps <- lapply(2015:2019, function(y)
  make_year_map(filter(map_data_long, year == y), y, "raw_water", rw_labels))

# Suppress legend on non-bottom panels to avoid duplication in stacked layout
rw_maps[[1]] <- rw_maps[[1]] + theme(legend.position = "none")
rw_maps[[3]] <- rw_maps[[3]] + theme(legend.position = "none")

png(file.path(results_dir, "rw_maps_2015-2016.png"),
    width = 250, height = 250, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(rw_maps[[1]] / rw_maps[[2]])
dev.off()

png(file.path(results_dir, "rw_maps_2017-2018.png"),
    width = 250, height = 250, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(rw_maps[[3]] / rw_maps[[4]])
dev.off()

png(file.path(results_dir, "rw_maps_2019.png"),
    width = 250, height = 150, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(rw_maps[[5]])
dev.off()

# ---- Total water maps (2015–2019) ----
tw_maps <- lapply(2015:2019, function(y)
  make_year_map(filter(map_data_long, year == y), y, "total_water", tw_labels))

tw_maps[[1]] <- tw_maps[[1]] + theme(legend.position = "none")
tw_maps[[3]] <- tw_maps[[3]] + theme(legend.position = "none")

png(file.path(results_dir, "tw_maps_2015-2016.png"),
    width = 250, height = 250, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(tw_maps[[1]] / tw_maps[[2]])
dev.off()

png(file.path(results_dir, "tw_maps_2017-2018.png"),
    width = 250, height = 250, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(tw_maps[[3]] / tw_maps[[4]])
dev.off()

png(file.path(results_dir, "tw_maps_2019.png"),
    width = 250, height = 150, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(tw_maps[[5]])
dev.off()

################################################################################
####
####  3. QUADRANT ANALYSIS
####
################################################################################
# Classify each mine by the combination of:
#   - Raw water use trend (slope > 0: increasing; <= 0: decreasing/stable)
#     from Bayesian slope estimates computed in 02-predict-water-use.R.
#   - Local freshwater availability trend (GRACE satellite, 2002-2016)
#     (>= 0: stable/improving; < 0: declining)
#
# Quadrant definitions:
#   Q1: increasing water use + improving freshwater  (pressure, but buffered)
#   Q2: decreasing water use + improving freshwater  (positive trajectory)
#   Q3: decreasing water use + declining freshwater  (adaptation underway)
#   Q4: increasing water use + declining freshwater  (STRESS ZONE)

pred_mine_data <- water_data |>
  # Slope columns already present in water_data (from final_predictions.csv);
  # no join needed. Filter to mines with both a Bayesian slope and
  # freshwater availability data for quadrant classification.
  filter(!is.na(rw_median_slope), !is.na(freshwater_availability)) |>
  mutate(
    quadrant = case_when(
      rw_median_slope >  0 & freshwater_availability >= 0 ~ "Q1",
      rw_median_slope <= 0 & freshwater_availability >= 0 ~ "Q2",
      rw_median_slope <= 0 & freshwater_availability <  0 ~ "Q3",
      rw_median_slope >  0 & freshwater_availability <  0 ~ "Q4"
    ),
    quadrant    = factor(quadrant, levels = c("Q1", "Q2", "Q3", "Q4")),
    stress_zone = quadrant == "Q4"
  )

# Summarise to one row per mine (average across years)
df_avg_stats <- pred_mine_data |>
  group_by(id_mine) |>
  summarise(
    rw_median_slope      = unique(rw_median_slope),
    rw_ci_lower          = unique(rw_ci_lower),
    rw_ci_upper          = unique(rw_ci_upper),
    rw_ci_width          = unique(ci_width),
    rw_prob_gt0          = unique(rw_prob_gt0),
    rw_prob_lt0          = unique(rw_prob_lt0),
    rw_slope_uncertainty = unique(rw_slope_uncertainty),
    rw_water             = mean(raw_water,   na.rm = TRUE),
    tw_water             = mean(total_water, na.rm = TRUE),
    cum_rw               = sum(raw_water,    na.rm = TRUE),
    cum_tw               = sum(total_water,  na.rm = TRUE),
    freshwater_availability = unique(freshwater_availability),
    avg_production       = mean(production,  na.rm = TRUE) * 1e-6,  # t -> Mt
    cum_production       = sum(production,   na.rm = TRUE) * 1e-6,
    log_avg_production   = log1p(mean(production, na.rm = TRUE) * 1e-6),
    stress_zone          = unique(stress_zone),
    quadrant             = unique(quadrant),
    lon                  = unique(lon),
    lat                  = unique(lat),
    .groups = "drop"
  ) |>
  drop_na()

# ---- Quadrant summary table ----
# The sign of freshwater and water-use trends is directly encoded in the
# quadrant label, so we read it from there rather than re-deriving from
# continuous values (which can have rounding edge cases).
quadrant_summary <- df_avg_stats |>
  mutate(
    fw_trend = ifelse(quadrant %in% c("Q1", "Q2"), "+", "-"),
    rw_trend = ifelse(quadrant %in% c("Q1", "Q4"), "+", "-")
  ) |>
  group_by(quadrant, fw_trend, rw_trend) |>
  summarise(
    n_mines        = n(),
    cum_production = sum(cum_production, na.rm = TRUE),
    cum_rw         = sum(rw_water,       na.rm = TRUE),
    cum_tw         = sum(tw_water,       na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(quadrant) |>
  mutate(across(c(n_mines, cum_production, cum_rw, cum_tw),
                list(share = \(x) round(x / sum(x) * 100, 1))))

message("Quadrant summary:")
print(kable(quadrant_summary, format = "markdown", digits = 1,
      col.names = c("Quadrant", "FW trend", "RW trend",
                    "Mines", "Prod (Mt)", "RW (Mm3)", "TW (Mm3)",
                    "Mines (%)", "Prod (%)", "RW (%)", "TW (%)")))

write_csv(quadrant_summary, file.path(results_dir, "quadrant_summary.csv"))

# ---- Quadrant map ----
# Reconstruct sf object from lon/lat (originally in EPSG:3857)
df_avg_stats_sf <- df_avg_stats |>
  st_as_sf(coords = c("lon", "lat"), crs = 3857) |>
  st_transform("+proj=igh +ellps=WGS84 +units=m +no_defs")

png(file.path(results_dir, "rw_quadrant_map.png"),
    width = 250, height = 150, units = "mm", pointsize = 12, res = 300,
    bg = "white")
print(
  bg_map +
    geom_sf(data  = filter(df_avg_stats_sf, !is.na(quadrant)),
            aes(color = quadrant), size = 0.9) +
    scale_color_brewer(palette = "Set1",
                       labels  = c("Q1" = "Q1: \u2191RW, \u2191FW",
                                   "Q2" = "Q2: \u2193RW, \u2191FW",
                                   "Q3" = "Q3: \u2193RW, \u2193FW",
                                   "Q4" = "Q4: \u2191RW, \u2193FW (stress)")) +
    labs(color = "Quadrant (RW=raw water, FW=freshwater)") +
    guides(color = guide_legend(override.aes = list(size = 3)))
)
dev.off()

################################################################################
####
####  4. DATA PRODUCTS  (publication-ready tables)
####
################################################################################

# ---- Site-level water use table (2015–2019) ----
# Values in m3 (converted from ML: 1 ML = 1000 m3)
read_csv2(file.path(results_dir, "final_predictions.csv"),
          show_col_types = FALSE) |>
  transmute(
    mine_name           = mine,
    country_name        = country,
    region_name         = region,
    year,
    new_water_m3_ref    = raw_water   * 1e3,   # ML -> m3
    new_water_m3_pred   = rw_pred     * 1e3,
    new_water_m3_sd     = rw_pred_sd  * 1e3,   # SD of prediction (m3)
    total_water_m3_ref  = total_water * 1e3,
    total_water_m3_pred = tw_pred     * 1e3,
    total_water_m3_sd   = tw_pred_sd  * 1e3,   # SD of prediction (m3)
    new_water_pi_lower  = rw_pi_lower * 1e3,
    new_water_pi_upper  = rw_pi_upper * 1e3,
    total_water_pi_lower = tw_pi_lower * 1e3,
    total_water_pi_upper = tw_pi_upper * 1e3,
    tw_uncertainty_source
  ) |>
  arrange(region_name, country_name, mine_name, year) |>
  write_csv(file.path(results_dir,
                      "copper_mine_site_level_water_use_2015-2019.csv"))

# ---- Slope + quadrant table ----
water_data |>
  select(mine, id_mine, aqueduct_bws_label, aware_annual_non_agri) |>
  group_by(id_mine) |>
  reframe(
    mine                  = unique(mine),
    aqueduct_bws_label    = unique(aqueduct_bws_label),
    aware_annual_non_agri = unique(aware_annual_non_agri)
  ) |>
  left_join(df_avg_stats, by = "id_mine") |>
  select(
    mine_name                     = mine,
    new_water_slope_median        = rw_median_slope,
    new_water_slope_lower         = rw_ci_lower,
    new_water_slope_upper         = rw_ci_upper,
    new_water_slope_ci_width      = rw_ci_width,
    prob_increasing               = rw_prob_gt0,
    prob_decreasing               = rw_prob_lt0,
    slope_uncertainty             = rw_slope_uncertainty,
    freshwater_availability_trend = freshwater_availability,
    avg_production,
    quadrant,
    aware_annual_non_agri,
    aqueduct_bws_label
  ) |>
  write_csv(file.path(results_dir,
                      "copper_mine_site_level_new_water_slope_2015-2019.csv"))

message("\nDone. All results saved to: ", results_dir)

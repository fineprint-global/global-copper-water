# ---
# Script: 03-water-use-analysis.R
# Purpose: This script performs the final geospatial and slope analysis of
#          predicted water use data, focusing on the relationship between
#          water use and freshwater availability. It generates the final
#          visualizations and data products for the project.
#
# Key Steps:
# 1. Geospatial Visualization: Aggregates the predicted water use data to a
#    spatial grid and generates a series of global maps to visualize water use
#    hotspots by year.
# 2. Water Use Slope Analysis: Fits a Bayesian mixed model using `brms` to estimate
#    the long-term trends (2015-2019) in raw water consumption for each mine.
#    The model uses default, weakly informative priors and is fitted with
#    4 Markov Chain Monte Carlo (MCMC) chains, running for 6000 iterations
#    with a 2000 iteration warmup period.
# 3. Quadrant Plot Analysis: Classifies each mine into a quadrant based on its
#    water use trend (increasing/decreasing) and the local freshwater availability
#    trend. This identifies mines in "stress zones" that have increasing water
#    use in areas with declining freshwater. 
# 4. Output: Saves the final analysis results as CSV files, which include
#    the trends, quadrant classifications, and other key summary statistics for
#    each mining site.
# ---


source("utils.R")

# Load data
water_data <- read_csv2("./results/final_predictions.csv") |>
    dplyr::mutate(total_water = tw_pred * 1e-3, raw_water = rw_pred * 1e-3) # Get pred and convert from ML to Mm3

sf_data <- st_read("./data/sf_data_raw.gpkg")

# check raw water against coefficients 0.45 to 0.6 m3 per tonne of ore
drop_na(transmute(filter(water_data, production > 0, ore_grade > 0), tw_lw = 100 * production / ore_grade * 0.45 * 1e-6, tw_up = 100 * production / ore_grade * 0.6 * 1e-6, raw_water)) |> reframe(across(everything(), sum))

# raw water intensity per tonne of ore
drop_na(transmute(filter(water_data, production > 0, ore_grade > 0), raw_water_int = raw_water / (100 * production / ore_grade) * 1e6)) |> summary() # m3/tonne

# raw water intensity per tonne of copper 
drop_na(transmute(filter(water_data, production > 0), raw_water_int = raw_water / production * 1e6)) |> summary() # m3/tonne


bg_map = plot_goode_homolosine_world_map(ocean_color = NA, land_color = "gray95", family = "Sans",
                                      grid_color = "grey75", grid_size = 0.1,
                                      country_borders_color = "grey75", country_borders_size = 0.1) +
      ggplot2::coord_sf(crs = "+proj=igh", expand = FALSE) +
      theme(
        plot.title = element_text(hjust = 0.1, vjust = -15), # Center and adjust vertical position
        plot.title.position = "plot",  # Position title relative to the entire plot
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.spacing = unit(0.0, "cm"),
        legend.key.size = unit(0.3, "cm"),
        legend.key.width = unit(345/15, "mm"),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      ) 

# water use to grid
map_data_wide <- left_join(select(sf_data, id_mine), select(water_data, id_mine, year, total_water, raw_water)) |>
    make_water_use_grid(cell_size = units::set_units(100, km))

map_data_long <- map_data_wide |>
    pivot_longer(cols = starts_with("total_water") | starts_with("raw_water"),
    names_to = c(".value", "year"),
    names_sep = "\\.")

# get single scale
rw_pretty_labels <- map_data_long |>
    reframe(raw_water = round(expm1(pretty(log1p(raw_water))))) |>
    dplyr::mutate(raw_water = ifelse(raw_water >= 10 & raw_water < 50, round(raw_water / 10) * 10,
                       ifelse(raw_water >= 50, round(raw_water / 10) * 10, raw_water))) |>
                       unlist() |>
                       as.numeric()

tw_pretty_labels <- map_data_long |>
    reframe(total_water = round(expm1(pretty(log1p(total_water))))) |>
    dplyr::mutate(total_water = ifelse(total_water >= 10 & total_water < 50, round(total_water / 10) * 10,
                       ifelse(total_water >= 50, round(total_water / 10) * 10, total_water))) |>
                       unlist() |>
                       as.numeric()

### Make Raw water maps
rw_maps <- lapply(2015:2019, function(y){
    bg_map +
    geom_sf(data = dplyr::filter(map_data_long, year == y), aes(fill = log1p(raw_water)), color = NA, lwd = 0, size = 0) +
    scale_fill_viridis(
        option = "turbo",
        begin = 0,
        end = 1,
        direction = 1,
        breaks = log1p(rw_pretty_labels),
        labels = rw_pretty_labels,
        limits = range(log1p(rw_pretty_labels)),
        name = expression("Million" * phantom(" ") * m^3)
        ) + 
        labs(title = bquote(.(y)))
})

rw_maps[[1]] <- rw_maps[[1]] + theme(legend.position = "none")
rw_maps[[3]] <- rw_maps[[3]] + theme(legend.position = "none") 
png(filename = "./results/rw_maps_2015-2016_50x50km.png", width = 250, height = 250, units = "mm", pointsize = 12, res = 300, bg = "white")
rw_maps[[1]] / rw_maps[[2]]
dev.off()
png(filename = "./results/rw_maps_2017-2018_50x50km.png", width = 250, height = 250, units = "mm", pointsize = 12, res = 300, bg = "white")
rw_maps[[3]] / rw_maps[[4]]
dev.off()
png(filename = "./results/rw_maps_2019_50x50km.png", width = 250, height = 150, units = "mm", pointsize = 12, res = 300, bg = "white")
rw_maps[[5]]
dev.off()


### Make Total water maps
tw_maps <- lapply(2015:2019, function(y){
    bg_map +
    geom_sf(data = dplyr::filter(map_data_long, year == y), aes(fill = log1p(total_water)), color = NA, lwd = 0, size = 0) +
    scale_fill_viridis(
        option = "turbo",
        begin = 0,
        end = 1,
        direction = 1,
        breaks = log1p(tw_pretty_labels),
        labels = tw_pretty_labels,
        limits = range(log1p(tw_pretty_labels)),
        name = expression("Million" * phantom(" ") * m^3)
        ) + 
        labs(title = bquote(.(y)))
})

tw_maps[[1]] <- tw_maps[[1]] + theme(legend.position = "none")
tw_maps[[3]] <- tw_maps[[3]] + theme(legend.position = "none") 
png(filename = "./results/tw_maps_2015-2016_50x50km.png", width = 250, height = 250, units = "mm", pointsize = 12, res = 300, bg = "white")
tw_maps[[1]] / tw_maps[[2]]
dev.off()
png(filename = "./results/tw_maps_2017-2018_50x50km.png", width = 250, height = 250, units = "mm", pointsize = 12, res = 300, bg = "white")
tw_maps[[3]] / tw_maps[[4]]
dev.off()
png(filename = "./results/tw_maps_2019_50x50km.png", width = 250, height = 150, units = "mm", pointsize = 12, res = 300, bg = "white")
tw_maps[[5]]
dev.off()


######## Make quadrant plot for fresh water availability

### Calculate local water use trends
mine_data <- left_join(sf_data, water_data, by = join_by(id_mine)) |>
    st_transform(crs = 3857) |>
    select(id_mine, year, raw_water, total_water, production, freshwater_availability = freshwater_availability.x) |>
    drop_na(raw_water, total_water) |>
    mutate(
      lon = st_coordinates(geom)[, 1],
      lat = st_coordinates(geom)[, 2]
    ) |>
    drop_na() |>
    group_by(id_mine) |>
    mutate(n_years = n()) |>
    ungroup() |>
    filter(n_years > 2) |>
    mutate(raw_water_log = log(raw_water), year_c = year - 2017, year_i = as.integer(year_c), id_mine_fac = factor(id_mine))

# Check for spatial autocorrelation using Moran's I
coords <- st_drop_geometry(mine_data) |>
  dplyr::transmute(
    lon = jitter(lon, amount = 1e-5),
    lat = jitter(lat, amount = 1e-5)) # jitter to avoid error because of repeated coordinates of the temporal observations
nb <- knn2nb(knearneigh(coords))
listw <- nb2listw(nb)

moran.test(mine_data$raw_water, listw)
moran.test(mine_data$raw_water_log, listw)
moran.test(mine_data$total_water, listw)
moran.test(mine_data$production, listw)

shapiro.test(mine_data$raw_water)
shapiro.test(mine_data$raw_water_log)

# Fit Bayesian mixed model
library(brms)
if(file.exists("./results/fit_bayes_trends.rds")){
  fit_bayes <- read_rds("./results/fit_bayes_trends.rds")
} else {
  fit_bayes <- brm(
    bf(raw_water_log ~ year_c + (year_c | id_mine), autocor = cor_ar(~ year_c | id_mine, p = 1)),
    data = mine_data,
    family = gaussian(),
    chains = 4,
    cores = 4,
    iter = 6000,        # More iterations
    warmup = 2000,      # Longer warmup
    seed = 123,
    control = list(
      adapt_delta = 0.99,  # More conservative stepsize (helps with convergence)
      max_treedepth = 15   # Allow deeper exploration of posterior
    ))
  write_rds(fit_bayes, "./results/fit_bayes_trends.rds")
}

summary(fit_bayes)
bayesplot::mcmc_rhat(rhat(fit_bayes))
plot(fit_bayes)
pp_check(fit_bayes, type = "dens_overlay")
#pp_check(fit_bayes, type = "stat_grouped", group = "id_mine", stat = "mean")

# 1.  Extract one draw per posterior iteration for
#     – the global (population-level) slope b_year_c
#     – each mine-specific deviation r_id_mine[ ,year_c]
library(tidybayes)
draws <- fit_bayes |>
  spread_draws(b_year_c, r_id_mine[id_mine, year_c]) |>
  mutate(slope = b_year_c + r_id_mine)   # mine-specific slope

draws |>
  filter(id_mine %in% c(1:5)) |>
  ggplot(aes(x = slope)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  facet_wrap(~ id_mine)

# 2.  Summarise per mine
trend_by_mine <- draws |>
  group_by(id_mine) |>
  summarise(
    rw_median_slope = median(slope),
    rw_ci_lower     = quantile(slope, 0.025),
    rw_ci_upper     = quantile(slope, 0.975),
    rw_prob_gt0     = mean(slope > 0),
    rw_prob_lt0     = mean(slope < 0),
    ci_width = rw_ci_upper - rw_ci_lower,
    rw_slope_uncertainty = 1 - abs(mean(slope > 0) - 0.5) * 2,  # 0 = certain, 1 = maximally uncertain,
    .groups = "drop"
  )

trend_by_mine |>
  ggplot(aes(x = rw_prob_gt0)) +
  geom_density(fill = "skyblue", alpha = 0.6)


# Extract random effects from the nlme model
pred_mine_data <- left_join(mine_data, trend_by_mine) |>
  # quadrants on water trends
  mutate(
  quadrant = case_when(
    rw_median_slope >  0 & freshwater_availability >= 0 ~ "Q1",
    rw_median_slope <= 0 & freshwater_availability >= 0 ~ "Q2",
    rw_median_slope <= 0 & freshwater_availability <  0 ~ "Q3",
    rw_median_slope >  0 & freshwater_availability <  0 ~ "Q4"),
  quadrant = factor(quadrant, c("Q1", "Q2", "Q3", "Q4"), c("Q1", "Q2", "Q3", "Q4")),
  stress_zone = quadrant == "Q4")

# Quadrant statistics
df_avg_stats <- pred_mine_data |>
  group_by(id_mine) |>
  summarise(
    rw_median_slope = unique(rw_median_slope),
    rw_ci_lower     = unique(rw_ci_lower),
    rw_ci_upper     = unique(rw_ci_upper),
    rw_prob_gt0     = unique(rw_prob_gt0),
    rw_prob_lt0     = unique(rw_prob_lt0),
    rw_slope_uncertainty = unique(rw_slope_uncertainty),
    rw_water = mean(raw_water, na.rm = TRUE),
    tw_water = mean(total_water, na.rm = TRUE),
    cum_rw = sum(raw_water, na.rm = TRUE),
    cum_tw = sum(total_water, na.rm = TRUE),
    # rw_residuals = mean(rw_residuals, na.rm = TRUE),
    freshwater_availability = unique(freshwater_availability),
    freshwater_dir = factor(freshwater_availability<0, c(TRUE, FALSE), c('Negative', 'Positive')),
    avg_production = mean(production, na.rm = TRUE) * 1e-6, # converted from t to Mt to avoid scale issues in the model
    cum_production = sum(production, na.rm = TRUE) * 1e-6, # converted from t to Mt to avoid scale issues in the model
    log_avg_production = log1p(avg_production),
    stress_zone = unique(stress_zone),
    quadrant = unique(quadrant),
    lon = unique(lon),
    lat = unique(lat)
  ) |>
  ungroup() |>
  drop_na()

st_drop_geometry(df_avg_stats) |> 
  as_tibble() |>
  group_by(quadrant) |> 
  summarise(
    freshwater = ifelse(unique(freshwater_availability<=0),'-','+'),
    rw_median_slope = ifelse(unique(rw_median_slope<=0),'-','+'),
    n_mines = n(),
    cum_production = sum(cum_production, na.rm = TRUE),
    cum_rw = sum(rw_water, na.rm = TRUE),
    cum_tw = sum(tw_water, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(quadrant) |>
  mutate(across(c(n_mines, cum_production, cum_rw, cum_tw), ~ .x / sum(.x)*100, .names = '{.col}_share')) |>
  kable(format = "markdown", digits = 1, col.names = c("Quadrant", "FW trend", "RW trend", "Mines", "Prod (Mt)", "RW (Mm3)", "TW (Mm3)", "Mines (%)", "Prod (%)", "RW (%)", "TW (%)"))

ggplot(trend_by_mine, aes(x = id_mine, y = rw_median_slope)) +
  geom_point() +
  theme_minimal() +
  geom_errorbar(aes(ymin = rw_ci_lower, ymax = rw_ci_upper, colour = rw_slope_uncertainty), width = 0.2) +
  scale_color_viridis() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Yearly trend raw water use", x = "Mine ID", colour = "Confidence of trend direction")


png(filename = "./results/rw_quadrant_map.png", width = 250, height = 150, units = "mm", pointsize = 12, res = 300, bg = "white")
bg_map + 
    geom_sf(data = dplyr::filter(df_avg_stats, !is.na(quadrant)),
            aes(color = quadrant), size = 0.7) + 
            scale_color_brewer(palette = "Set1") +
            labs(color = "Quadrant") +
            guides(color = guide_legend(override.aes = list(size = 4))) 
dev.off()


st_write(df_avg_stats, dsn = "./results/trend_data.csv", delete_dsn = TRUE)



# Create data release
transmute(read_csv2("./results/final_predictions.csv"),
  mine_name = mine,
  country_name = country,
  region_name = region, year, 
  # converted to m3
  new_water_m3_ref = raw_water * 1e3,
  new_water_m3_pred = rw_pred * 1e3,
  total_water_m3_ref = total_water * 1e3,
  total_water_m3_pred = tw_pred * 1e3
  ) |>
  arrange(desc(region_name), country_name, mine_name, year) |>
  write_csv("./results/copper_mine_site_level_water_use_2015-2019.csv")

select(water_data, mine, id_mine, aqueduct_bws_label, aware_annual_non_agri) |>
  group_by(id_mine) |>
  reframe(
    mine = unique(mine),
    aqueduct_bws_label = unique(aqueduct_bws_label),
    aware_annual_non_agri = unique(aware_annual_non_agri)
  ) |>
  left_join(df_avg_stats) |>
  select(
    mine_name = mine,
    new_water_slope_median = rw_median_slope,
    new_water_slope_lower = rw_ci_lower,
    new_water_slope_upper = rw_ci_upper,
    freshwater_availability_trend = freshwater_availability,
    avg_production = avg_production,
    quadrant,
    aware_annual_non_agri,
    aqueduct_bws_label) |>
    write_csv("./results/copper_mine_site_level_new_water_slope_2015-2019.csv")

source("utils.R")

# Load data
water_data <- read_csv2("./data/final_predictions.csv") |>
    dplyr::mutate(total_water = total_water * 1e-3, raw_water = raw_water * 1e-3) # convert from ML to Mm3

# check raw water against coefficients 0.45 to 0.6 m3 per tonne of ore
drop_na(transmute(water_data, tw_lw = 100 * production / ore_grade * 0.45 * 1e-6, tw_up = 100 * production / ore_grade * 0.6 * 1e-6, raw_water)) |> summarise(across(everything(), sum))

# raw water intensity per tonne of ore
drop_na(transmute(water_data, raw_water_int = raw_water / (100 * production / ore_grade) * 1e6)) |> summary() # m3/tonne

# raw water intensity per tonne of copper 
drop_na(transmute(water_data, raw_water_int = raw_water / production * 1e6)) |> summary() # m3/tonne

sf_data <- st_read("./data/sf_data_raw.gpkg")

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

### Calculate local water use trend using GWR

gwr_data <- left_join(sf_data, water_data, by = join_by(id_mine)) |>
    select(id_mine, year, raw_water, total_water)

# Check for spatial autocorrelation using Moran's I
coords <- st_coordinates(gwr_data) |>
  as_tibble() |>
  dplyr::mutate(
    X = jitter(X, amount = 1e-5),
    Y = jitter(Y, amount = 1e-5)) # jitter to avoid error because of repeated coordinates of the temporal observations
nb <- knn2nb(knearneigh(coords))
listw <- nb2listw(nb)
moran.test(gwr_data$raw_water, listw)
moran.test(gwr_data$total_water, listw)

rw_bw <- gwr.sel(raw_water ~ year, data = as(gwr_data, "Spatial"), gweight = gwr.Gauss, verbose = TRUE)
tw_bw <- gwr.sel(raw_water ~ year, data = as(gwr_data, "Spatial"), gweight = gwr.Gauss, verbose = TRUE)

rw_gwr_model <- gwr(raw_water ~ year,
                 data = as(gwr_data, "Spatial"),
                 bandwidth = rw_bw,
                 gweight = gwr.Gauss,
                 hatmatrix = TRUE)

tw_gwr_model <- gwr(total_water ~ year,
                 data = as(gwr_data, "Spatial"),
                 bandwidth = tw_bw,
                 gweight = gwr.Gauss,
                 hatmatrix = TRUE)

# Compare to ols
rw_ols_model <- lm(raw_water ~ year, data = as.data.frame(gwr_data))
summary(rw_ols_model)
AIC(rw_ols_model)
rw_gwr_model

tw_ols_model <- lm(total_water ~ year, data = as.data.frame(gwr_data))
summary(tw_ols_model)
AIC(tw_ols_model)
tw_gwr_model

# Extract local coefficients and their standard errors
as.data.frame(rw_gwr_model$SDF) |>
  mutate(
    t_value = gwr_data$raw_water / rw_gwr_model$SDF$pred.se,  # Adjust column name for standard errors
    p_value = 2 * pt(abs(t_value), df = nrow(gwr_data) - 2, lower.tail = FALSE)
  ) |>
  dplyr::select(p_value) |>
  summary()

as.data.frame(tw_gwr_model$SDF) |>
  mutate(
    t_value = gwr_data$total_water / tw_gwr_model$SDF$pred.se,  # Adjust column name for standard errors
    p_value = 2 * pt(abs(t_value), df = nrow(gwr_data) - 2, lower.tail = FALSE)
  ) |>
  dplyr::select(p_value) |>
  summary()


##### Prepare data for trend plots
rw_gwr_res <- as(rw_gwr_model$SDF, "sf") |>
  dplyr::mutate(id_mine = gwr_data$id_mine) |>
  dplyr::select(id_mine, sum.w, `(Intercept)`, year, `(Intercept)_se`, year_se, localR2, `(Intercept)_se_EDF`, year_se_EDF, id_mine) |>
  st_drop_geometry() |>
  as_tibble() |>
  dplyr::group_by(id_mine) |>
  dplyr::summarise(across(everything(), unique)) |>
  dplyr::right_join(sf_data)

tw_gwr_res <- as(tw_gwr_model$SDF, "sf") |>
  dplyr::mutate(id_mine = gwr_data$id_mine) |>
  dplyr::select(id_mine, sum.w, `(Intercept)`, year, `(Intercept)_se`, year_se, localR2, `(Intercept)_se_EDF`, year_se_EDF, id_mine) |>
  st_drop_geometry() |>
  as_tibble() |>
  dplyr::group_by(id_mine) |>
  dplyr::summarise(across(everything(), unique)) |>
  dplyr::right_join(sf_data)  

trend_data <- water_data |>
  select(id_mine, year, raw_water, total_water, freshwater_availability, average_production, production) |>
  group_by(id_mine) |>
  reframe(
    #raw_water_trend = coef(lm(raw_water ~ year))[2],
    #total_water_trend = coef(lm(total_water ~ year))[2],
    freshwater_availability = unique(freshwater_availability),
    average_production = mean(average_production, na.rm = TRUE) * 1e-6, # from t to Mt
    cum_production = sum(production, na.rm = TRUE) * 1e-6 # from t to Mt
  ) |>
  dplyr::mutate(
    raw_water_trend = rw_gwr_res$year,
    total_water_trend = tw_gwr_res$year) |> # trend from GWR
  drop_na() |>
  dplyr::mutate(quadrant = case_when(
    raw_water_trend > 0 & freshwater_availability > 0 ~ "Q1",
    raw_water_trend <= 0 & freshwater_availability > 0 ~ "Q2",
    raw_water_trend <= 0 & freshwater_availability < 0 ~ "Q3",
    raw_water_trend > 0 & freshwater_availability < 0 ~ "Q4"
  )) |>
  dplyr::mutate(quadrant = factor(quadrant, c("Q4", "Q1", "Q2", "Q3")))

# Prepare quadrant annotations
quadrant_summaries <- trend_data |>
  group_by(quadrant) |>
  reframe(
    total_ave_production = sum(average_production),
    total_production = sum(cum_production),
  ) |>
  dplyr::mutate(
    total_percentage = 100 * total_production / sum(total_production),
    total_ave_percentage = 100 * total_ave_production / sum(total_ave_production),
    label_total = paste(quadrant, ": ", round(total_production, 1), "Mt (", round(total_percentage, 1), "%)", sep = ""),
    label_total_ave = paste(quadrant, ": ", round(total_ave_production, 1), "Mt (", round(total_ave_percentage, 1), "%)", sep = ""))

# Set positions for quadrant annotations
annotation_positions <- data.frame(
  quadrant = c("Q1", "Q2", "Q3", "Q4"),
  x = c(6.5, -6.5, -6.5, 6.5),
  y = c(1, 1, -1, -1)
) |>
  left_join(quadrant_summaries, by = "quadrant")

png(filename = "./results/rw_quadrant_plot.png", width = 250, height = 200, units = "mm", pointsize = 12, res = 300, bg = "white")
ggplot(trend_data, aes(x = raw_water_trend, y = freshwater_availability)) +
  geom_point(aes(size = average_production, color = quadrant), alpha = 0.5, show.legend = c(color = FALSE, size = TRUE)) +
  scale_size_continuous(range = c(3, 15)) +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_text(data = annotation_positions, aes(x = x, y = y, label = label_total_ave), size = 5, fontface = "bold") +
  theme_minimal() +
  labs(
    x = expression("Raw water use trend (Million" * phantom(" ") * m^3 * phantom(" ") * year^-1 * ")"),
    y = expression("Freshwater availability trend (" * cm * phantom(" ") * year^-1 * ")"),
    size = "Average production (Mt)"
  ) +
  guides(size = guide_legend(position = "inside")) +
  theme(
    legend.direction = "vertical",
    legend.position.inside = c(0.85, 0.2),
    legend.background = element_rect(fill = "white", color = "black") # Optional: Add background and border for better visibility
  )
dev.off()

png(filename = "./results/rw_quadrant_map.png", width = 250, height = 150, units = "mm", pointsize = 12, res = 300, bg = "white")
bg_map + 
    geom_sf(data = dplyr::filter(left_join(sf_data, trend_data, by = join_by(id_mine)), !is.na(quadrant)),
            aes(color = quadrant), size = 0.7) + 
            scale_color_brewer(palette = "Set1") +
            labs(color = "Quadrant") +
            guides(color = guide_legend(override.aes = list(size = 4))) 
 dev.off()


 
left_join(sf_data, trend_data, by = join_by(id_mine)) |>
    st_write(dsn = "./results/trend_data.csv", delete_dsn = TRUE)


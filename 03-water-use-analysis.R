source("utils.R")

# Load data
water_data <- read_csv2("./data/final_predictions.csv")
sf_data <- st_read("./data/sf_data_raw.gpkg")

sf_data



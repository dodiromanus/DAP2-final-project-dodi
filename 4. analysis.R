# PART 7: POWERPLANT ANALYSIS

# Read the Excel file
powerplants_data <- read_excel(powerplants_path) |>
  select(
    `Plant name`, `Capacity (MW)`, "Status", `Start year`, `Retired year`,
    `Combustion technology`, `Coal type`, "Longitude", "Latitude"
  ) |>
  filter(Status != "cancelled")

# Replace NA in Retired year with 2024
powerplants_data <- powerplants_data |>
  mutate(
    `Retired year` = ifelse(is.na(`Retired year`), 2024, `Retired year`),
    Duration_operation = `Retired year` - `Start year`,  # Calculate duration of operation
    Active_energy = `Capacity (MW)` * Duration_operation  # Calculate active energy
  )

# Convert powerplant data to an sf object
powerplants_sf <- st_as_sf(powerplants_data, coords = c("Longitude", "Latitude"), crs = st_crs(germany_shapefile))

# Spatially join powerplants to regions
powerplants_with_regions <- st_join(powerplants_sf, germany_shapefile, join = st_intersects)

# Aggregate powerplant data to regions
powerplants_summary <- powerplants_with_regions |>
  group_by(Region) |>
  summarise(
    active_energy = sum(Active_energy, na.rm = TRUE),                  # Sum of active energy
    num_powerplants = n(),                                           # Total number of powerplants
    active_powerplants = sum(Status == "operating", na.rm = TRUE),   # Count of active powerplants
    retired_powerplants = sum(Status == "retired", na.rm = TRUE),    # Count of retired powerplants
    avg_duration_operation = mean(Duration_operation, na.rm = TRUE), # Average duration of operation
    combustion_tech_distribution = paste(unique(`Combustion technology`), collapse = ", "), # Combustion tech distribution
    coal_type_distribution = paste(unique(`Coal type`), collapse = ", "),                   # Coal type distribution
    .groups = "drop"
  )

# Merge aggregated powerplant data with AQ_region
AQ_region_extended <- AQ_region |>
  left_join(powerplants_summary, by = "Region")

# Step 1: Prepare the data
model_data <- AQ_region_extended |>
  # Extract the month and define the season
  mutate(
    Month = as.numeric(format(Date, "%m")),
    season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    )
  ) |>
  # Include lagged air quality variables as predictors
  mutate(
    lag_PM10 = lag(PM10_mean_region),
    lag_NO2 = lag(NO2_mean_region),
    lag_O3 = lag(O3_mean_region)
  ) |>
  # Remove rows with NA values (caused by lagged variables or missing data)
  drop_na()

# Step 2: Define the regression model for PM10
pm10_model <- lm(
  PM10_mean_region ~ active_energy + num_powerplants + active_powerplants + retired_powerplants +
    avg_duration_operation + season,
  data = model_data
)

# Step 3: Define the regression model for NO2
no2_model <- lm(
  NO2_mean_region ~ active_energy + num_powerplants + active_powerplants + retired_powerplants +
    avg_duration_operation + season,
  data = model_data
)

# Step 4: Define the regression model for O3
o3_model <- lm(
  O3_mean_region ~ active_energy + num_powerplants + active_powerplants + retired_powerplants +
    avg_duration_operation + season,
  data = model_data
)

# Step 5: Summarize the regression results
summary(pm10_model)
summary(no2_model)
summary(o3_model)

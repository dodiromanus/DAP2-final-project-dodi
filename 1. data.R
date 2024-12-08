library(readxl)
library(arrow)
library(dplyr)
library(httr)


# Define the main path where the R files, summarized files 
# (Station, Air quality, shp, poweplant data, and scrapped reddit data) are located


main_path <- "C:/Users/roman/OneDrive/Documents/GitHub/DAP2-final-project-dodi"


# Check if the summarized final dataset (filtered_air_quality_data.csv) 
# already exists in the "Data" folder within the main project path.
# If the file exists, load it and use it for the project.
# If the file does not exist, proceed to download and process the air quality data

filtered_data_path <- file.path(main_path, "Data", "filtered_air_quality_data.csv")
station_list_path <- file.path(main_path, "Data", "station_list.xlsx")
powerplants_path <- file.path(main_path, "Data", "Powerplants.xlsx")
shapefile_path <- file.path(main_path, "Data", "gadm41_DEU_2.shp")

# Read the station list Excel file
station_list <- read_excel(station_list_path)
station_ids <- station_list$`Station ID`



#Part 0 : Initial checking

# Check if the summarized dataset exists
if (file.exists(filtered_data_path)) {
  # Load the existing dataset
  filtered_air_quality_data <- read.csv(filtered_data_path)
  message("Loaded existing dataset: filtered_air_quality_data.csv")
}else {

# If you want to run Part 1 and Part 2 code (downloading the air quality and parque them)
# please delete the filtered_air_quality_data.csv in the main folder


  
  

#Part 1 : Data preparation


  # Define the directory to save the raw air quality data
  save_directory <- file.path(main_path, "Data", "Air quality")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(save_directory)) {
    dir.create(save_directory, recursive = TRUE)
  }
  
  # Loop through station IDs and download data
  for (station_id in station_ids) {
    # Construct the download URL for each station ID
    url <- paste0(
      "https://www.umweltbundesamt.de/api/air_data/v3/airquality/csv?",
      "date_from=2020-01-01&time_from=1&",
      "date_to=2023-01-01&time_to=1&",
      "station=", station_id, "&lang=en"
    )
    
    # Define the destination file path
    file_name <- paste0("airquality_station_", station_id, ".csv")
    file_path <- file.path(save_directory, file_name)
    
    # Download the file
    download.file(url, destfile = file_path, mode = "wb", quiet = TRUE)
    message("Downloaded: ", file_name)
  }
  
  
  

  
  # PART 2: LOADING AND PARQUET THE AIR QUALITY DATA
  
  # List all CSV files in the directory
  csv_files <- list.files(save_directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if there are any CSV files in the directory
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified directory.")
  }
  
  # Combine all CSVs into a single DataFrame, removing the last 2 rows from each file
  combined_data <- bind_rows(
    lapply(csv_files, function(file) {
      # Read the CSV file with semicolon delimiter
      data <- read_delim(file, delim = ";")
      # Remove the last two rows
      data <- data[1:(nrow(data) - 2), ]
      return(data)
    })
  )
  
  # Remove quotes from the 'Date' column and convert it to datetime format
  combined_data <- combined_data |>
    mutate(Date = as.POSIXct(gsub("'", "", Date), format = "%Y-%m-%d %H:%M"))
  
  # Write the complete dataset to a Parquet file
  parquet_file <- file.path(save_directory, "complete_air_quality_data.parquet")
  write_parquet(combined_data, parquet_file)
  message("Complete dataset successfully saved as a Parquet file at: ", parquet_file)
  
  # Load the Parquet file and apply time filtering
  filtered_air_quality_data <- open_dataset(parquet_file) 
  filtered_air_quality_data <- filtered_air_quality_data |>
    filter(
      format(Date, "%H:%M") %in% c("08:00", "12:00", "16:00") &  # Filter specific times
        Date >= as.POSIXct("2022-01-01 00:00:00")  # Filter for dates starting January 1, 2022
    ) |>
    collect()  # Collect the data into a DataFrame
  
  # Rename and simplify the column names
  filtered_air_quality_data <- filtered_air_quality_data |>
    select(`Station code`:`Nitrogen dioxide (NO₂) One hour average in µg/m³`) |>
    rename(
      PM10 = `Particulate matter (PM₁₀) Daily average (hourly floating) in µg/m³`,
      O3 = `Ozone (O₃) One hour average in µg/m³`,
      NO2 = `Nitrogen dioxide (NO₂) One hour average in µg/m³`
    )
}  






# PART 3: DATA PROCESSING AND PREPARATION

# Replace non-numeric values with NA
filtered_air_quality_data <- filtered_air_quality_data |>
  rename(`Station code` = "Station.code") |>
  mutate(
    PM10 = as.numeric(ifelse(PM10 %in% c("-", ""), NA, PM10)),
    NO2 = as.numeric(ifelse(NO2 %in% c("-", ""), NA, NO2)),
    O3 = as.numeric(ifelse(O3 %in% c("-", ""), NA, O3))
  )

# Remove the hour portion from the Date column
filtered_air_quality_data <- filtered_air_quality_data |>
  mutate(Date = as.Date(Date))

# Calculate mean for each parameter by station code and date
AQ_station <- filtered_air_quality_data |>
  group_by(`Station code`, Date) |>
  summarise(
    PM10_mean = round(mean(PM10, na.rm = TRUE),1),
    NO2_mean = round(mean(NO2, na.rm = TRUE),1),
    O3_mean = round(mean(O3, na.rm = TRUE),1),
    .groups = "drop"
  )

# Define AQ summary per station and per region
AQ_station <- AQ_station |> 
  left_join(station_list, by = "Station code") |>
  rename(Region = `Closest Region`)

# Calculate AQ_region by averaging parameters for each region
AQ_region <- AQ_station |>
  group_by(Region, Date) |>
  summarise(
    PM10_mean_region = mean(PM10_mean, na.rm = TRUE),  # Average PM10
    NO2_mean_region = mean(NO2_mean, na.rm = TRUE),   # Average NO2
    O3_mean_region = mean(O3_mean, na.rm = TRUE),     # Average O3
    .groups = "drop"                                  # Ungroup after summarizing
  )


# Calculate monthly average for each region
region_monthly_avg <- AQ_region |>
  mutate(Month = format(Date, "%Y-%m")) |>
  group_by(Region, Month) |>
  summarise(
    PM10_monthly_avg = mean(PM10_mean_region, na.rm = TRUE),
    NO2_monthly_avg = mean(NO2_mean_region, na.rm = TRUE),
    O3_monthly_avg = mean(O3_mean_region, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate monthly average for the whole of Germany
germany_monthly_avg <- AQ_region |>
  mutate(Month = format(Date, "%Y-%m")) |>
  group_by(Month) |>
  summarise(
    PM10_monthly_avg = mean(PM10_mean_region, na.rm = TRUE),
    NO2_monthly_avg = mean(NO2_mean_region, na.rm = TRUE),
    O3_monthly_avg = mean(O3_mean_region, na.rm = TRUE),
    .groups = "drop"
  )

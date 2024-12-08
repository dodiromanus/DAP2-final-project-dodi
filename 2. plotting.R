library(sf)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)



# PART 4: LOADING THE SHAPEFILE

# Load the specific shapefile and remove unnecessary column
germany_shapefile <- st_read(shapefile_path) |>
  select(NAME_1, NAME_2, geometry) |>
  rename(State = NAME_1, Region = NAME_2)

# Plot the shapefile
plot(germany_shapefile$geometry, main = "Germany Shapefile (Region level)")






# PART 5A: CREATE A FUNCTION TO VISUALIZE THE AIR QUALITY COMPARISON AFTER 3 MONTHS

# Define the visualization function with 3 months comparison
visualize_air_quality <- function(date_input, parameter, main_path = NULL) {
  
  # Ensure parameter is valid
  valid_parameters <- c("PM10", "NO2", "O3")
  if (!(parameter %in% valid_parameters)) {
    stop("Invalid parameter. Choose from 'PM10', 'NO2', or 'O3'.")
  }
  
  # Match the parameter to the corresponding column in AQ_region
  parameter_col <- paste0(parameter, "_mean_region")
  
  # Convert the input date to a Date object and calculate the next date
  date_input <- as.Date(date_input)
  date_next_period <- date_input + 90  # Roughly three months after
  
  # Filter AQ_region for the selected dates
  temp_data_current <- AQ_region |>
    filter(Date == date_input) |>
    select(Region, !!sym(parameter_col))
  
  temp_data_next <- AQ_region |>
    filter(Date == date_next_period) |>
    select(Region, !!sym(parameter_col))
  
  # Check if data exists for both dates
  if (nrow(temp_data_current) == 0 || nrow(temp_data_next) == 0) {
    stop("No data available for the selected date(s). Please choose a different date.")
  }
  
  # Combine data to determine shared scale
  combined_data <- bind_rows(temp_data_current, temp_data_next)
  scale_limits <- range(combined_data[[parameter_col]], na.rm = TRUE)
  
  # Join with germany_shapefile for both dates
  temp_shapefile_current <- germany_shapefile |>
    left_join(temp_data_current, by = "Region")
  temp_shapefile_next <- germany_shapefile |>
    left_join(temp_data_next, by = "Region")
  
  # Create the first plot (current date)
  plot_current <- ggplot(temp_shapefile_current) +
    geom_sf(aes(fill = !!sym(parameter_col)), color = "white", size = 0.1) +
    scale_fill_gradient(
      name = parameter,
      low = "green", high = "red", na.value = "grey50",
      limits = scale_limits  # Shared scale
    ) +
    theme_minimal() +
    labs(
      title = paste("Air Quality -", parameter, "on", date_input),
      subtitle = "Based on regional average air quality values",
      caption = "Data: Averaged Air Quality Measurements For All Stations"
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "none"  # Remove the legend from the first plot
    )
  
  # Create the second plot (next period)
  plot_next <- ggplot(temp_shapefile_next) +
    geom_sf(aes(fill = !!sym(parameter_col)), color = "white", size = 0.1) +
    scale_fill_gradient(
      name = parameter,
      low = "green", high = "red", na.value = "grey50",
      limits = scale_limits  # Shared scale
    ) +
    theme_minimal() +
    labs(
      title = paste("Air Quality -", parameter, "on", date_next_period),
      subtitle = "Based on regional average air quality values",
      caption = "Data: Averaged Air Quality Measurements For All Stations"
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "right"  # Keep the legend on the second plot
    )
  
  # Arrange the two plots side by side
  combined_plot <- grid.arrange(plot_current, plot_next, ncol = 2)
  
  # Save the plot as a PNG file in the main_path if provided
  if (!is.null(main_path)) {
    # Create "Images" folder if it doesn't exist
    images_folder <- file.path(main_path, "Images")
    if (!dir.exists(images_folder)) {
      dir.create(images_folder)
    }
    
    # Define the output filename
    output_filename <- file.path(
      images_folder,
      paste0("2.1 air_quality_comparison_", parameter, "_", date_input, ".png")
    )
    
    # Use a workaround to save gridExtra plots
    ggsave(
      filename = output_filename,
      plot = ggplot(),  # Placeholder plot
      device = "png",
      width = 16, height = 8, units = "in", dpi = 300
    )
    png(filename = output_filename, width = 16, height = 8, units = "in", res = 300)
    grid.draw(combined_plot)
    dev.off()
  }
  
  # Return the combined plot object
  invisible(combined_plot)
}

visualize_air_quality(
  date_input = "2022-08-01",
  parameter = "PM10",
  main_path = main_path
)







# PART 5B: CREATE A FUNCTION TO VISUALIZE REGION AIR QUALITY TRENDLINE FOR 10 MONTHS

# Define the visualization function with month-to-month comparison
visualize_air_quality_multiple <- function(date_input, parameter, main_path = NULL) {
  # Ensure parameter is valid
  valid_parameters <- c("PM10", "NO2", "O3")
  if (!(parameter %in% valid_parameters)) {
    stop("Invalid parameter. Choose from 'PM10', 'NO2', or 'O3'.")
  }
  
  # Match the parameter to the corresponding column in AQ_region
  parameter_col <- paste0(parameter, "_mean_region")
  
  # Convert the input date to a Date object
  date_input <- as.Date(date_input)
  
  # Initialize an empty list to store plots
  plot_list <- list()
  
  # Loop through 10 months to generate plots
  for (i in 0:9) {
    current_date <- date_input + i * 30  # Calculate the date for each month
    
    # Filter AQ_region for the selected date
    temp_data <- AQ_region |>
      filter(Date == current_date) |>
      select(Region, !!sym(parameter_col))
    
    # Join with germany_shapefile
    temp_shapefile <- germany_shapefile |>
      left_join(temp_data, by = "Region")
    
    # Create the plot for the current month
    plot <- ggplot(temp_shapefile) +
      geom_sf(aes(fill = !!sym(parameter_col)), color = "white", size = 0.1) +
      scale_fill_gradient(
        name = parameter,
        low = "green", high = "red", na.value = "grey50"
      ) +
      theme_minimal() +
      labs(
        title = paste("Air Quality -", parameter, "on", current_date),
        subtitle = "Based on regional average air quality values",
        caption = "Data: Averaged Air Quality Measurements For All Stations"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.position = ifelse(i == 9, "right", "none")  # Legend only for the last plot
      )
    
    # Add the plot to the list
    plot_list[[i + 1]] <- plot
  }
  
  # Arrange the plots in a grid
  combined_plot <- grid.arrange(grobs = plot_list, ncol = 5)
  
  # Save the plot as a PNG file in the main_path if provided
  if (!is.null(main_path)) {
    # Create "Images" folder if it doesn't exist
    images_folder <- file.path(main_path, "Images")
    if (!dir.exists(images_folder)) {
      dir.create(images_folder)
    }
    
    # Define the output filename
    output_filename <- file.path(
      images_folder,
      paste0("2.2 air_quality_multiple_", parameter, "_", date_input, ".png")
    )
    
    # Use a workaround to save gridExtra plots
    png(filename = output_filename, width = 20, height = 10, units = "in", res = 300)
    grid.draw(combined_plot)
    dev.off()
  }
  
  # Return the combined plot object
  invisible(combined_plot)
}

# Example usage:
visualize_air_quality_multiple(
  date_input = "2022-03-01",
  parameter = "PM10",
  main_path = main_path
)






# PART 5C: CREATE A FUNCTION TO VISUALIZE AIR QUALITY TRENDLINE IN A STATION

# Define the visualization function for a station
visualize_station_trends <- function(station_code_input, main_path = NULL) {
  # Ensure the Station code exists in AQ_station
  if (!(station_code_input %in% AQ_station$`Station code`)) {
    stop("Station code not found in the dataset.")
  }
  
  # Filter the data for the specified Station code
  station_data <- AQ_station |>
    filter(`Station code` == station_code_input) |>
    select(Date, PM10_mean, NO2_mean) |>
    pivot_longer(
      cols = c(PM10_mean, NO2_mean),
      names_to = "Parameter",
      values_to = "Value"
    ) |>
    mutate(Parameter = case_when(
      Parameter == "PM10_mean" ~ "PM10",
      Parameter == "NO2_mean" ~ "NO2"
    ))
  
  # Create the plot
  station_plot <- ggplot(station_data, aes(x = Date, y = Value, color = Parameter, group = Parameter)) +
    geom_line(size = 0.7) +
    geom_point(size = 1) +
    labs(
      title = paste("Air Quality Trends for Station Code:", station_code_input),
      x = "Date",
      y = "Average Value",
      color = "Parameter",
      caption = "Data: Averaged Air Quality Measurements For Selected Station"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("PM10" = "blue", "NO2" = "green"))
  
  # Display the plot
  print(station_plot)
  
  # Save the plot as a PNG file in the main_path if provided
  if (!is.null(main_path)) {
    # Create "Images" folder if it doesn't exist
    images_folder <- file.path(main_path, "Images")
    if (!dir.exists(images_folder)) {
      dir.create(images_folder)
    }
    
    # Define the output filename
    output_filename <- file.path(
      images_folder,
      paste0("2.3 air_quality_station_", station_code_input,".png")
    )
    
    ggsave(
      filename = output_filename,
      plot = station_plot,
      device = "png",
      width = 12, height = 6, units = "in", dpi = 300
    )
  }
}


# Example usage
visualize_station_trends(
  station_code_input = "DERP010",
  main_path = main_path
)

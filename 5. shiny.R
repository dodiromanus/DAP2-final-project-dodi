library(broom)
library(shiny)



# PART 8: SHINY

# Define UI
ui <- fluidPage(
  titlePanel("Air Quality Map with Powerplants"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "parameter",
        label = "Select Air Quality Parameter:",
        choices = c("PM10", "NO2", "O3"),
        selected = "PM10"
      ),
      sliderInput(
        inputId = "date",
        label = "Select Date:",
        min = as.Date(min(AQ_region$Date)),
        max = as.Date(max(AQ_region$Date)),
        value = as.Date(min(AQ_region$Date)),
        timeFormat = "%Y-%m-%d",
        step = 1,
        animate = animationOptions(interval = 1500, loop = TRUE)
      ),
      checkboxGroupInput(
        inputId = "powerplant_status",
        label = "Toggle Powerplant Status:",
        choices = c("operating", "retired", "mothballed"),
        selected = c("operating", "retired", "mothballed")
      )
    ),
    mainPanel(
      plotOutput(outputId = "air_quality_map", height = "600px")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$air_quality_map <- renderPlot({
    # Filter AQ_region for the selected date and parameter
    parameter_col <- paste0(input$parameter, "_mean_region")
    temp_data <- AQ_region |>
      filter(Date == input$date) |>
      select(Region, !!sym(parameter_col)) |>
      rename(ParameterValue = !!sym(parameter_col))
    
    # Join with germany_shapefile
    temp_shapefile <- germany_shapefile |>
      left_join(temp_data, by = "Region")
    
    # Filter powerplants based on selected statuses
    powerplants_filtered <- powerplants_data |>
      filter(Status %in% input$powerplant_status)
    
    # Convert to sf object if not already
    powerplants_sf <- st_as_sf(powerplants_filtered, coords = c("Longitude", "Latitude"), crs = st_crs(germany_shapefile))
    
    # Create the map
    ggplot() +
      geom_sf(data = temp_shapefile, aes(fill = ParameterValue), color = "white", size = 0.1) +
      geom_sf(data = powerplants_sf, aes(size = `Capacity (MW)`, color = Status), alpha = 0.7) +
      scale_fill_gradient(
        name = input$parameter,
        low = "green", high = "red", na.value = "grey50"
      ) +
      scale_size_continuous(name = "Capacity (MW)", range = c(2, 10)) +
      scale_color_manual(
        name = "Powerplant Status",
        values = c("operating" = "blue", "retired" = "purple", "mothballed" = "white")
      ) +
      theme_minimal() +
      labs(
        title = paste("Air Quality Map for", input$parameter),
        subtitle = paste("Date:", input$date),
        caption = "Data: Averaged Air Quality Measurements and Powerplants"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "right"
      )
  })
}

# Run the App
shinyApp(ui = ui, server = server)

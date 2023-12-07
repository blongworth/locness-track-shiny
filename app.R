#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)
library(dygraphs)
library(xts)

# Data locations
loc01_underway <- "data/LOC-01_Combined_Nav_TSG_Fluo_HydroFIA.txt"
loc01_drifter <- "data/Drifter_data_combined.csv"
loc01_ctd <- "data/CTD_downcast_upcast.csv"

# Load and prepare sample ship track data
ship_data <- read_csv(loc01_underway,
                      na = c("", "NA", "NaN"),
                      col_types = "cnninnnnnncni") |> 
  mutate(datetime = as.POSIXct(DateTime_UTC, 
                                   format = "%d-%b-%Y %H:%M:%S", 
                                   tz = "UTC"),
         sal = ifelse(Salinity_PSU < 29, NA, Salinity_PSU)) |> 
  select(datetime,
         lat = Latitude, 
         lon = Longitude,
         dye = Dye_ppb,
         temp = Temp1_C,
         sal) |> 
  filter(datetime > as.POSIXct("2023-09-02 12:00:00", tz = "UTC"),
         datetime < as.POSIXct("2023-09-04 01:30:00", tz = "UTC"))

map_plot <- function(data, point_var, palette = "magma", n_quantiles = 20) {
  # Create color palette
  pal <- colorQuantile(palette, data[[point_var]], n = n_quantiles)
  
  # Create Leaflet map
  leaflet(data = data) |> 
    addTiles() |> 
    addCircleMarkers(
      radius = 2,
      stroke = FALSE,
      fillOpacity = 0.8,
      color = ~pal(data[[point_var]])) #|> 
    #leaflet::addLegend(
    #  position = "bottomright",
    #  pal = pal,
    #  values = data[[point_var]],
    #  title = "Color legend",
    #  opacity = 0.8
    #)
}

# Define function to create time series plot
ts_plot <- function(data, ts_var) {
  loc_ts <- as.xts(data[[ts_var]], data[["datetime"]])
  g <- dygraph(
    data = loc_ts,
    ylab = ts_var,
    main = paste(ts_var, "vs Time"),
    
  ) |> 
    dyRangeSelector()
  if (ts_var == "dye") {
    g |> 
      dyOptions(logscale = TRUE)
  } else {
    g
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Ship Track and Temperature Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("temp_col", "Temperature Variable:", choices = names(ship_data)[4:6]),
      actionButton("update_plot", "Update Plot")
    ),
    mainPanel(
      leafletOutput("map_plot"),
      dygraphOutput("time_series_plot")
    )
  )
)

# server <- function(input, output) {
#   # Variable to store selected datetime range
#   selected_datetime_range <- reactive(range(ship_data$datetime))
#   
#   # Update map and time series plot based on range selector
#   observe({
#     # Get selected datetime range from dygraph
#     values <- reactiveValues()  
#     observeEvent(input$time_series_plot_date_window,{
#       value1 <- input$time_series_plot_date_window[[1]]
#       value2 <- input$time_series_plot_date_window[[2]]
#       values$v1 <- value1
#       values$v2 <- value2
#     })
#     # Filter data based on selected range
#     filtered_data <- ship_data %>%
#       filter(datetime >= values$v1,
#              datetime <= values$v2)
#     
#     # Update map with markers within the range
#     output$map_plot <- renderLeaflet({
#       map_plot(filtered_data, input$temp_col, point_size = 5) |>
#         addCircleMarkers(
#           data = filtered_data,
#           lng = ~lon,
#           lat = ~lat,
#           radius = 10,
#           color = "red",
#           fillOpacity = 0.8
#         )
#     })
#   })
#   
#   # Update time series plot with pointer
#   output$time_series_plot <- renderDygraph({
#            ts_plot(ship_data, selected_temp_col)
#          })
#   
#   # Initial plot render
#   output$map_plot <- renderLeaflet({
#     map_plot(ship_data, "temp")
#   })
#   
#   output$time_series_plot <- renderDygraph({
#          ts_plot(ship_data, "temp")
#        })
# }

# Define server 
server <- function(input, output) {
  # Update map and time series plot on input change
  observeEvent(input$update_plot, {
    selected_temp_col <- input$temp_col
    
    output$map_plot <- renderLeaflet({
      map_plot(ship_data, selected_temp_col)
    })
    
    output$time_series_plot <- renderDygraph({
      ts_plot(ship_data, selected_temp_col)
    })
  })
  
  # Initial plot render
  output$map_plot <- renderLeaflet({
    map_plot(ship_data, "temp")
  })
  
  output$time_series_plot <- renderDygraph({
    ts_plot(ship_data, "temp")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
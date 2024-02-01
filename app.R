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
library(tidyr)
library(lubridate)
library(plotly)
library(readr)
library(dygraphs)
library(xts)

# Data locations
loc01_underway <- "data/LOC-01_Underway_continuous.txt"
loc01_drifter <- "data/Drifter_data_combined.csv"
loc01_ctd <- "data/CTD_downcast_upcast.csv"

# Load and prepare sample ship track data
ship_data <- read_csv(loc01_underway,
                      na = c("", "NA", "NaN"),
                      col_types = "cnninnnnnncninnni") |> 
  mutate(datetime = as.POSIXct(DateTime_UTC, 
                                   format = "%d-%b-%Y %H:%M:%S", 
                                   tz = "UTC"),
         sal = ifelse(Salinity_PSU < 29, NA, Salinity_PSU)) |> 
  select(datetime,
         lat = Latitude, 
         lon = Longitude,
         dye = Dye_ppb,
         temp = Temp1_C,
         sal,
         TA = Corrected_TA_umol_kg_,
         fCO2sw = fCO2_SW_SST_uatm,
         fCO2atm = fCO2_ATM_interpolated_uatm,
         dfCO2 = dfCO2_uatm) |> 
  filter(datetime > as.POSIXct("2023-09-02 12:00:00", tz = "UTC"),
         datetime < as.POSIXct("2023-09-04 01:30:00", tz = "UTC"))

# resample dataset
resample_ship <- function(data, interval_seconds) {
  data %>%
    mutate(datetime = floor_date(datetime, 
                                          unit = paste0(interval_seconds, 
                                                        " seconds"))) %>%  
    group_by(datetime) %>%
    summarise(across(everything(), mean, .names = "{.col}"))
}


map_plot <- function(data, point_var, palette = "magma", n_quantiles = 20) {
  pal <- colorQuantile(palette, data[[point_var]], n = n_quantiles)
  data <- drop_na(data, {{point_var}})
  # Create Leaflet map
  leaflet(data = data, options = leafletOptions(zoomControl = FALSE)) |> 
    addTiles() |> 
    addCircleMarkers(
      radius = 2,
      stroke = FALSE,
      fillOpacity = 0.8,
      #color = ~pal(data[[point_var]])
      color = "darkgrey"
    )
}

map_add <- function(mapid, data, point_var, palette = "magma", n_quantiles = 20) {
  pal <- colorQuantile(palette, ship_data[[point_var]], n = n_quantiles)
  #pal <- colorQuantile(palette, data[[point_var]], n = n_quantiles)
  data <- drop_na(data, {{point_var}})
  # Create Leaflet map
  leafletProxy(mapid, data = data) |> 
    clearGroup("quantity") |> 
    #clearMarkers() |> 
    addCircleMarkers(
      group = "quantity",
      radius = 2,
      stroke = FALSE,
      fillOpacity = 0.8,
      color = ~pal(data[[point_var]]))
}

ts_plot <- function(data, ts_var, 
                    date_range = as.POSIXct(c("2023-09-02 12:00:00", 
                                              "2023-09-04 01:30:00"), 
                                              tz = "UTC")) {
  data <- drop_na(data, {{ts_var}})
  loc_ts <- as.xts(data[[ts_var]], data[["datetime"]])
  g <- dygraph(
    data = loc_ts,
    ylab = ts_var,
    #main = paste(ts_var, "vs Time")
    ) |> 
    dyRangeSelector(dateWindow = date_range)
  if (ts_var == "dye") {
    g |> 
      dyOptions(logscale = TRUE) |> 
      dyAxis("y", label = "Rhodamine Conc. (ppb)", valueRange = c(1, 250))
  } else {
    g
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Ship Track Timeseries"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var_col", 
                  "Variable to plot", 
                  choices = names(ship_data)[4:10]),
      sliderInput("interval", "Averaging (s)", min = 2, max = 60, value = 30),
      checkboxInput(inputId = "show_moving",
                    label = strong("Show moving window"),
                    value = FALSE),
      sliderInput("time",
                  label = "Time elapsed",
                  min = min(ship_data$datetime),
                  max = max(ship_data$datetime),
                  value = min(ship_data$datetime), step = 60 * 10,
                  animate =
                    animationOptions(interval = 200, loop = TRUE)),
      #sliderInput("timestep", "Timestep (s)", min = 60, max = 60 * 30, value = 60 * 10),
      sliderInput("ts_window", "Time window (m)", min = 1 * 60, max = 2249.9, value = 3 * 60),
    ),
    mainPanel(
      leafletOutput("mapplot", height = "60vh"),
      dygraphOutput("tsplot", height = "30vh"),
      textOutput("click")
    )
  )
)

# Define server 
server <- function(input, output) {
  # Update map and time series plot on input change
    
    filtered_ship <- reactive({
      if (input$var_col %in% names(ship_data[4:6])) {
        resample_ship(ship_data, input$interval)
      } else {
        ship_data
      }
    })
  
    output$mapplot <- renderLeaflet({
      map_plot(filtered_ship(), input$var_col)
    })
    
    output$tsplot <- renderDygraph({
      ts_plot(filtered_ship(), 
              input$var_col, 
              c(input$time, input$time + input$ts_window * 60))
    })
    
    output$click <- renderText({
      if (is.null(input$tsplot_click$x)) {
      } else {
        paste("Clicked point:", format(lubridate::ymd_hms(input$tsplot_click$x, tz = Sys.timezone())))
      }
    })
    
    observeEvent(input$tsplot_click$x, {
      if(!is.null(input$tsplot_click$x)){
        selected_time <- lubridate::ymd_hms(input$tsplot_click$x, tz = Sys.timezone())
        # get the lat lon of the time selected
        selected_point <- filtered_ship() |> 
          filter(datetime == selected_time)
        
        # avoid redraw
        leafletProxy( "mapplot", data = filtered_ship() ) |> 
          removeMarker(layerId = 'clicked_point') |> 
          addMarkers(data = selected_point,
                     layerId = 'clicked_point')
          
      }
    })
    # React to Dygraph range selection
    observeEvent(input$tsplot_date_window, {
      date_range  <- lubridate::ymd_hms(input$tsplot_date_window, tz = Sys.timezone())
      filtered_data  <- filtered_ship() |> 
          filter(datetime >= date_range[1],
                 datetime <= date_range[2])
      
      map_add("mapplot", filtered_data, input$var_col)
    })
    
    # React to time selector
    observeEvent(input$time, {
      date_range  <- c(input$time, input$time + input$ts_window * 60)
      filtered_data  <- filtered_ship() |> 
          filter(datetime >= date_range[1],
                 datetime <= date_range[2])
      
      map_add("mapplot", filtered_data, input$var_col)
      
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
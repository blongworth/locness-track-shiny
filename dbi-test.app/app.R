# app.R
library(shiny)
library(RSQLite)
library(dplyr)
library(leaflet)
library(dygraphs)
library(DBI)
library(here)


DB_FILE <- here("../locness-fluorologger/data.db")

read_data <- function(db_file, time_range) {
  con <- dbConnect(RSQLite::SQLite(), db_file)
  query <- sprintf("SELECT * FROM data WHERE timestamp BETWEEN '%s' AND '%s'",
                   as.integer(time_range[1]),
                   as.integer(time_range[2]))
  df <- dbGetQuery(con, query)
  dbDisconnect(con)
  df
}

update_map <- function() {
  current_data <- data()
  #pal <- colorNumeric(palette = "viridis", domain = current_data$concentration)
  pal <- colorQuantile(palette = "magma",
                       domain = current_data$concentration,
                       n = 20)
  leafletProxy("map") %>%
    clearMarkers() %>%
    addCircleMarkers(~longitude, ~latitude,
                     color = ~pal(concentration),
                     radius = 1,
                     fillOpacity = 0.7,
                     popup = ~paste("Value:", concentration)) %>%
    leaflet::addLegend("bottomright", pal = pal, values = ~concentration,
              title = "Value",
              opacity = 1)
}

# Define UI
ui <- fluidPage(
  titlePanel("Mapping Data from SQLite Database"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("time", "Select Time Range:",
                  min = as.POSIXct("2023-09-01 00:00:00"),
                  max = Sys.time(),
                  value = c(as.POSIXct("2023-01-01 00:00:00"), Sys.time()),
                  timeFormat = "%Y-%m-%d %H:%M:%S",
                  step = 3600),
      checkboxInput("current", "Autoupdate", value = TRUE),
      textOutput("npoints"),
      textOutput("mean")
    ),
    mainPanel(
        leafletOutput("map", height = "70vh"),
        dygraphOutput("tsplot", height = "20vh")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to fetch data based on time range
  data <- reactive({
    invalidateLater(5000)
    if (input$current) {
      time_range <- c(input$time[1], Sys.time())
    } else {
      time_range <- input$time
    }
    new_data <- read_data(DB_FILE, time_range)
    if (!identical(new_data, data())) {
      data(new_data)
    }
  })
  
  # Render Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -70.9, lat = 41.1, zoom = 2) # set initial view
  })
  
  # Update when data changes
  observeEvent(data(), update_map())
  
  #Render timeseries
  output$tsplot <- renderDygraph({
    df <- data() %>%
      mutate(timestamp = as.POSIXct(timestamp)) %>% 
      select(timestamp, concentration)
    dygraph(df) %>% 
      dyRangeSelector() %>% 
      dyOptions(logscale = TRUE) |> 
      dyAxis("y", label = "Rhodamine Conc. (ppb)", valueRange = c(0.001, 500))
  })
  
  output$npoints <- renderText(nrow(data()))
  output$mean <- renderText(mean(data()$voltage, na.rm = TRUE))
}

# Run the application 
shinyApp(ui = ui, server = server)

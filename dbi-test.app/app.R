# app.R
library(shiny)
library(RSQLite)
library(dplyr)
library(leaflet)
library(dygraphs)
library(DBI)
library(here)


DB_FILE <- here("../locness-fluorologger/data.db")

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
    con <- dbConnect(RSQLite::SQLite(), DB_FILE)
    query <- sprintf("SELECT * FROM data WHERE timestamp BETWEEN '%s' AND '%s'",
                     as.integer(input$time[1]),
                     #as.integer(input$time[2])) #need current time
                     as.integer(Sys.time()))
    df <- dbGetQuery(con, query)
    dbDisconnect(con)
    df
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    df <- data()
    #pal <- colorNumeric(palette = "viridis", domain = df$concentration)
    pal <- colorQuantile(palette = "magma", domain = df$concentration, n = 20)
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude,
                       color = ~pal(concentration),
                       radius = 1,
                       fillOpacity = 0.7,
                       popup = ~paste("Value:", concentration)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~concentration,
                title = "Value",
                opacity = 1)
  })
  
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

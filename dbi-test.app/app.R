# app.R
library(shiny)
library(RSQLite)
library(dplyr)
library(leaflet)
library(DBI)
library(here)

DB_FILE <- here("data/data.db")

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
                  step = 3600)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to fetch data based on time range
  data <- reactive({
    con <- dbConnect(RSQLite::SQLite(), DB_FILE)
    query <- sprintf("SELECT * FROM locations WHERE time BETWEEN '%s' AND '%s'",
                     format(input$time[1], "%Y-%m-%d %H:%M:%S"),
                     format(input$time[2], "%Y-%m-%d %H:%M:%S"))
    df <- dbGetQuery(con, query)
    dbDisconnect(con)
    df
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    df <- data()
    #pal <- colorNumeric(palette = "viridis", domain = df$value)
    pal <- colorQuantile(palette = "magma", domain = df$value, n = 20)
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude,
                       color = ~pal(value),
                       radius = 1,
                       fillOpacity = 0.7,
                       popup = ~paste("Value:", value)) %>%
      addLegend("bottomright", pal = pal, values = ~value,
                title = "Value",
                opacity = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

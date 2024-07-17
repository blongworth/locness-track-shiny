# app.R
library(shiny)
library(RSQLite)
library(dplyr)
library(tidyr)
library(leaflet)
library(dygraphs)
library(DBI)
library(here)
library(bslib)

#DB_FILE <- here("../locness-fluorologger/data.db")
DB_FILE <- here("data.db")

# row counters for updates
row_count <<- 0L
previous_row_count <<- 0L
display_data <- reactiveVal(NULL)

con <- dbConnect(RSQLite::SQLite(), DB_FILE)

db_data_chunk <- reactivePoll(
  intervalMillis = 2000L, # check for a db update every second
  session = NULL,
  checkFunc = function() {
    print(paste("Running checkFunc:", Sys.time()))
    if(DBI::dbIsValid(con) && dbExistsTable(con, "data")){
      row_count <<- dbGetQuery(con, "select count(*) from data")[[1]]
    } else {
      0L
    }
  },
  valueFunc = function() {
    if(DBI::dbIsValid(con) && dbExistsTable(con, "data")){
      print(paste("Running valueFunc: Updating display_data | Current row count:", 
                  row_count))
      df <- dbGetQuery(con, sprintf("select * from data LIMIT %s OFFSET %s", 
                                    row_count-previous_row_count, 
                                    previous_row_count))
      previous_row_count <<- row_count
      df
    } else {
      NULL
    }
  }
)

# TODO: Catch no data error
get_data <- function(db_file, time_range, last_read_time) {
  con <- dbConnect(RSQLite::SQLite(), DB_FILE)
  query <- sprintf("SELECT * FROM data
                   WHERE timestamp BETWEEN '%s' AND '%s'",
                   time_range[1],
                   time_range[2])
  df <- dbGetQuery(con, query) |> 
    mutate(new = timestamp > last_read_time)
  dbDisconnect(con)
  df
}
    
map_plot <- function() {
  leaflet(options = leafletOptions(maxZoom = 25)) |> 
    # add ocean basemap
    addProviderTiles(providers$Esri.OceanBasemap) |>
    setView(-70.65, 41.5285, zoom = 15)
}

map_add <- function(mapid, data, point_var, 
                    clear_points = FALSE,
                    palette = "magma", n_quantiles = 20,
                    new_legend = TRUE) {
  pal <- colorQuantile(palette, data[[point_var]], n = n_quantiles)
  #pal <- colorQuantile(palette, data[[point_var]], n = n_quantiles)
  data <- drop_na(data, {{point_var}})
  ship_lat <- data$latitude[nrow(data)]
  ship_lon <- data$longitude[nrow(data)]
  m <- leafletProxy(mapid, data = data) |> 
    clearGroup("ship")
  if (clear_points) {
    m <- m |> 
    clearGroup("quantity") 
  }
  if (new_legend) {
    m <- m |> 
      removeControl("legend") |> 
      leaflet::addLegend(layerId = "legend",
                         pal = pal, 
                         values = ~data[[point_var]], 
                         #title = point_var,
                         title = ifelse(point_var == "concentration", 
                                        "Rhodamine (ppb)",
                                        point_var),
                         opacity = .8,
                         labFormat = function(type, cuts, p) {
                           n = length(cuts)
                           cuts <- round(cuts, 1)
                           paste0(cuts[-n], " &ndash; ", cuts[-1])
                           #cuts[length(cuts)] <- NA
                         })
  }
  
  m |> 
    addCircleMarkers(
      lng= ~longitude,
      lat= ~latitude,
      group = "quantity",
      radius = 2,
      stroke = FALSE,
      fillOpacity = 0.8,
      color = ~pal(data[[point_var]])) |> 
    addMarkers(group = "ship",
               lng = ship_lon,
               lat = ship_lat,
               )
}

cur_time <- Sys.time()
# Define UI
ui <- page_sidebar(
  title = "LOCNESS Underway Mapper",
 # nav_spacer(), # push nav items to the right
 # nav_panel("Page 1", "Dashboard content"),
 # nav_item(
 #   input_dark_mode(id = "dark_mode", mode = "light")
 # ),
#)
  sidebar = sidebar(
    sliderInput("time", "Select Time Range:",
                min = as.POSIXct("2024-01-01 00:00:00"),
                max = Sys.time(),
                value = c(as.POSIXct("2024-07-01 00:00:00"), cur_time),
                timeFormat = "%Y-%m-%d %H:%M:%S",
                step = 3600),
    input_dark_mode(id = "dark_mode", mode = "light"),
    textOutput("npoints"),
    textOutput("mean"),
    textOutput("time"),
    textOutput("lasttime")
  ),
  card(
    leafletOutput("map", height = "70vh"),
    dygraphOutput("tsplot", height = "20vh")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(db_data_chunk(), {
    if(is.null(display_data())){
      display_data(db_data_chunk())
    } else {
      display_data(rbind(display_data(), db_data_chunk()))
    }
  })
  # check ?dataTableProxy() and ?replaceData() to avoid re-rendering the table
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      showNotification("Welcome to the dark side!")
    }
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    map_plot()
  })
  
  # Add points Leaflet map
  observe({
    map_add("map", db_data_chunk(), "concentration", clear_points = FALSE)
  })
  
  # Plot subset
  
  #Render timeseries
  output$tsplot <- renderDygraph({
    df <- display_data() %>%
      mutate(timestamp = as.POSIXct(timestamp)) %>% 
      select(timestamp, concentration)
    dygraph(df) %>% 
      dyRangeSelector() %>% 
      dyOptions(logscale = TRUE) |> 
      dyAxis("y", label = "Rhodamine Conc. (ppb)", valueRange = c(0.001, 500))
  })
  
  output$npoints <- renderText(nrow(display_data()))
  output$mean <- renderText(mean(display_data()$voltage, na.rm = TRUE))
  output$time <- renderText(row_count)
  output$lasttime <- renderText(previous_row_count)
}

# Run the application 
shinyApp(ui = ui, server = server)

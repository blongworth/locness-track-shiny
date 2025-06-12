# app.R
library(shiny)
library(RSQLite)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DBI)
library(here)
library(bslib)

DB_FILE <- "C:/Users/CSL 2/Documents/LOCNESS_data/underway.db"
#DB_FILE <- here("data.db")

# row counters for updates
row_count <<- 0L
previous_row_count <<- 0L
display_data <- reactiveVal(NULL)

con <- dbConnect(RSQLite::SQLite(), DB_FILE)

db_data_chunk <- reactivePoll(
  intervalMillis = 10000L, # check for a db update every second
  session = NULL,
  checkFunc = function() {
    #print(paste("Running checkFunc:", Sys.time()))
    if(DBI::dbIsValid(con) && dbExistsTable(con, "data")){
      row_count <<- dbGetQuery(con, "select count(*) from data")[[1]]
    } else {
      0L
    }
  },
  valueFunc = function() {
    if(DBI::dbIsValid(con) && dbExistsTable(con, "data")){
      #print(paste("Running valueFunc: Updating display_data | Current row count:", 
      #            row_count))
      df <- dbGetQuery(con, sprintf("select * from data LIMIT %s OFFSET %s", 
                                    row_count-previous_row_count, 
                                    previous_row_count))
      previous_row_count <<- row_count
      df |> 
        mutate(new = TRUE,
               latitude = na_if(as.numeric(latitude), 0),
               longitude = na_if(as.numeric(longitude), 0))
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
                    plot_new = FALSE,
                    palette = "magma", n_quantiles = 20,
                    new_legend = TRUE) {
  pal <- colorQuantile(palette, data[[point_var]], n = n_quantiles)
  data <- drop_na(data, c(latitude, longitude, {{point_var}})) |>
    filter(latitude != 0, longitude != 0) 
  # Return early if no data after filtering
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }
  ship_lat <- data$latitude[nrow(data)]
  ship_lon <- data$longitude[nrow(data)]
  if (plot_new) {
    data <- data |> 
      filter(new == TRUE)
    # Return early if no new data to plot
    if (nrow(data) == 0) {
      return(invisible(NULL))
    }
  }
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
    selectInput("resample", "Resample interval:",
                choices = c("1 min" = "1 min", "5 min" = "5 min", "10 min" = "10 min"),
                selected = "10 min"),
    input_dark_mode(id = "dark_mode", mode = "light"),
    actionButton("redraw",
                 "Redraw plot"),
    textOutput("npoints"),
    textOutput("mean"),
    #textOutput("time"),
    textOutput("lasttime")
  ),
  card(
    leafletOutput("map", height = "70vh"),
    plotlyOutput("tsplot", height = "20vh")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(db_data_chunk(), {
    if(is.null(display_data())){
      db_data_chunk() |> 
        display_data()
    } else {
      display_data() |> 
        mutate(new = FALSE) |> 
        rbind(db_data_chunk()) |> 
        display_data()
      #display_data(rbind(display_data(), db_data_chunk()))
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
  
  # Redraw leaflet map
  observeEvent(input$redraw,
               {map_add("map", display_data(), "concentration",
                        clear_points = TRUE,
                        plot_new = FALSE,
                        new_legend = TRUE)
               }
  )
  
  # Add points Leaflet map
  observe({
    map_add("map", display_data(), "concentration",
            clear_points = FALSE, plot_new = TRUE,
            new_legend = FALSE)
  })
  
  # Plot subset
  
  #Store plotly layout state in a reactive value
  plotly_layout <- reactiveVal(list(xaxis = NULL, yaxis = list(type = 'log', range = c(log10(0.001), log10(500)))))

  #Render timeseries with plotly
  output$tsplot <- renderPlotly({
    req(display_data())
    df <- display_data() %>%
      mutate(timestamp = as.POSIXct(timestamp)) %>%
      select(timestamp, concentration)
    # Resample by user-selected interval
    interval <- input$resample
    df <- df %>%
      mutate(time_bin = cut(timestamp, breaks = interval)) %>%
      group_by(time_bin) %>%
      summarize(
        timestamp = first(timestamp),
        concentration = mean(concentration, na.rm = TRUE),
        .groups = 'drop'
      )
    # Use stored layout if available
    layout_opts <- plotly_layout()
    plt <- plot_ly(df, x = ~timestamp, y = ~concentration, type = 'scatter', mode = 'lines+markers', name = 'Rhodamine', source = 'tsplot') %>%
      event_register('plotly_relayout')
    # Always set yaxis log and range, but preserve xaxis range if available
    yaxis_opts <- layout_opts$yaxis
    if (is.null(yaxis_opts)) {
      yaxis_opts <- list(title = 'Rhodamine Conc. (ppb)', type = 'log', range = c(log10(0.001), log10(500)))
    } else {
      yaxis_opts$title <- 'Rhodamine Conc. (ppb)'
      yaxis_opts$type <- 'log'
      yaxis_opts$range <- c(log10(0.001), log10(500))
    }
    if (!is.null(layout_opts$xaxis) && !is.null(layout_opts$xaxis$range)) {
      plt <- layout(plt, xaxis = layout_opts$xaxis, yaxis = yaxis_opts)
    } else {
      plt <- layout(plt, yaxis = yaxis_opts)
    }
    plt
  })
  
  # Listen for relayout events and store the current layout
  observeEvent(event_data("plotly_relayout", source = "tsplot"), {
    layout_evt <- event_data("plotly_relayout", source = "tsplot")
    cur_layout <- plotly_layout() %||% list()
    # Only update axis ranges if present in event
    if (!is.null(layout_evt[["xaxis.range[0]"]]) && !is.null(layout_evt[["xaxis.range[1]"]])) {
      cur_layout$xaxis <- list(range = c(layout_evt[["xaxis.range[0]"]], layout_evt[["xaxis.range[1]"]]))
    }
    if (!is.null(layout_evt[["yaxis.range[0]"]]) && !is.null(layout_evt[["yaxis.range[1]"]])) {
      cur_layout$yaxis <- list(range = c(layout_evt[["yaxis.range[0]"]], layout_evt[["yaxis.range[1]"]]), type = 'log', title = 'Rhodamine Conc. (ppb)')
    }
    plotly_layout(cur_layout)
  })
  
  output$npoints <- renderText(nrow(display_data()))
  output$mean <- renderText(mean(display_data()$concentration, na.rm = TRUE))
  #output$time <- renderText(row_count)
  output$lasttime <- renderText(previous_row_count)
}

# Run the application 
shinyApp(ui = ui, 
         server = server, 
         onStart = function() {
  onStop(function() {
    dbDisconnect(con)
  })
})

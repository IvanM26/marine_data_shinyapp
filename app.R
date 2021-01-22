# Load packages
library(shiny)
library(shinydashboard)
# library(shiny.semantic)
library(tidyverse)
library(geosphere)
library(leaflet)
library(lubridate)

# Read data
data <- readRDS("data/ships.RDS")
ship_data <- readRDS("data/ship_data.RDS")
dict_shiptype <- readRDS("data/dict_shiptype.RDS")

ui <- dashboardPage(
  dashboardHeader(title = "Marine Data App"),
  dashboardSidebar(
    # Input to select ship type
    selectInput(inputId = "ship_type", label = "Select Vessel Type",
      choices = setNames(dict_shiptype$SHIPTYPE, dict_shiptype$ship_type)
    ),
    # Input to select ship name
    # choices = NULL because it'll be updated in server based on chosen type
    selectInput(inputId = "ship_name", label = "Select Vessel",
      choices = NULL
    )
  ),
  dashboardBody(
    
    fluidRow(
      column(10,
             h3("Longest distance between two consecutive observations"),
             leafletOutput("map")),
      column(2,
             # Note with the distance sailed
             h3("Distance Sailed"),
             textOutput("distance"),
             # Extra computation: time that took sailing that distance
             h3("Time Elapsed"),
             textOutput("time_sailing"))
    ),
    
    h2("Vessel Details"),
    
    # Info boxes
    infoBoxOutput("ship_length"),
    infoBoxOutput("ship_width"),
    infoBoxOutput("ship_weight")
  )
)

server <- function(input, output, session) {
  
  # Update available ship names based on selected type
  ship_data_filt <- reactive({
    req(input$ship_type)
    ship_data %>%
      filter(SHIPTYPE == input$ship_type)
  })

  observeEvent(ship_data_filt(), {
    choices <- setNames(ship_data_filt()$SHIP_ID_MOD, ship_data_filt()$SHIPNAME)
    updateSelectInput(session, "ship_name", choices = choices)
  })
  
  # Keep data rows that correspond to selected ship
  # (using the unique ID I created)
  data_filt <- reactive({
    req(input$ship_name)
    data %>% 
      filter(SHIP_ID_MOD == input$ship_name) %>% 
      # I make sure that the data is ordered by datetime
      arrange(DATETIME)
  })
  
  # Value that will determine which rows to use in the map
  long_dist_row <- reactiveVal(0)
  
  
  long_dist <- eventReactive(data_filt(), {
    # Compute all distances between consecutive rows
    distances <- compute_distances(data_filt())
    
    # Get the max distance value
    max_dist <- max(distances)
    
    # Get the row that produces the max distance with its consecutive row
    # (As I previously arranged by DATETIME, if there are two or more rows that
    # have this maximum value, I just have to pick the last one)
    max_dist_row <- max(which(distances == max_dist))
    
    # Update the row number that generates the max distance with its consecutive
    long_dist_row(max_dist_row)
    
    # Return the max distance for this function
    max_dist
  })
  
  # Data that is going to be used in the map
  data_map <- reactive({
    req(data_filt())
    data_filt() %>% 
      slice(long_dist_row(), long_dist_row() + 1)
  })
  
  # Used to create legend
  html_legend <- "<img src='beggining.png'>Beggining<br/>
<img src='end.png'>End"
  
  # Create map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>% 
      addAwesomeMarkers(lng = data_map()$LON,
                 lat = data_map()$LAT,
                 icon = awesomeIcons(icon = c("asterisk", "flag"),
                                     library = "glyphicon",
                                     markerColor = ifelse(data_map()$is_parked == 0, "green", "red")),
                 label = c(data_map()$DATETIME[[1]],
                           data_map()$DATETIME[[2]]),
                 labelOptions = labelOptions(direction = "top"),


                 ) %>% 
      addLegend(position = "topright",
                colors = c("red", "green"),
                labels = c("Parked", "In Movement"),
                title = "Vessel's State") %>%
      addControl(html = html_legend, position = "bottomright")
  })
  
  # Output max distance in meters
  output$distance <- renderText({
    paste0(round(long_dist(), 2), " meters")
  })
  
  # Compute time sailing
  time_sailing <- reactive({
    interval(data_map()$DATETIME[[1]], data_map()$DATETIME[[2]]) %>% 
      as.period(unit = "days")
    
  })
  
  # Output time sailing
  output$time_sailing <- renderText({
      paste(time_sailing())
  })
  
  
  # infoBoxes output
  output$ship_length <- renderInfoBox({
    infoBox(
      "Length", prettyNum(data_map()$LENGTH[[1]], big.mark = ","), 
      "meters", icon = icon("ruler"), color = "blue"
    )
  })
  
  output$ship_width <- renderInfoBox({
    infoBox(
      "Width", prettyNum(data_map()$WIDTH[[1]], big.mark = ","), 
      "meters", icon = icon("arrows-alt-h"), color = "blue"
    )
  })
  
  output$ship_weight <- renderInfoBox({
    infoBox(
      "Deadweight", prettyNum(data_map()$DWT[[1]], big.mark = ","), 
      "tones", icon = icon("weight-hanging"), color = "blue"
    )
  })
  
  # Some informal checks I used along the way
  # observe(message(paste0("long_dist() = ", long_dist())))
  # observe(message(paste0("long_dist_row() = ", long_dist_row())))
  # observe(message(paste0("nrow(data_filt()) = ", nrow(data_filt()))))
  
}

shinyApp(ui, server)
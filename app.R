library(shiny)
# library(shiny.semantic)
library(tidyverse)
library(geosphere)
library(leaflet)

# Read data
data <- readRDS("data/ships.RDS")
ship_data <- readRDS("data/ship_data.RDS")
dict_shiptype <- readRDS("data/dict_shiptype.RDS")

ui <- fluidPage(
  selectInput(
    inputId = "ship_type",
    label = "Select Ship Type",
    choices = setNames(dict_shiptype$SHIPTYPE, 
                       dict_shiptype$ship_type),
  ),
  
  selectInput(
    inputId = "ship_name",
    label = "Select Ship Name",
    choices = NULL
  ),
  
  dataTableOutput("data_filtered"),
  
  leafletOutput("map")
)

server <- function(input, output, session) {
  
  # Update available ship names based on selected type
  ship_data_filt <- reactive({
    req(input$ship_type)
    ship_data %>%
      filter(SHIPTYPE == input$ship_type)
  })

  observeEvent(ship_data_filt(), {
    choices <- setNames(ship_data_filt()$SHIP_ID_MOD,
                        ship_data_filt()$SHIPNAME)
    updateSelectInput(session, "ship_name", choices = choices)
  })
  
  data_filt <- reactive({
    req(input$ship_name)
    data %>% 
      filter(SHIP_ID_MOD == input$ship_name) %>% 
      # I make sure that the data is ordered by datetime
      arrange(DATETIME)
  })
  
  long_dist_row <- reactiveVal(0)
  
  # observe(message(paste0("nrow(data_filt()) = ", nrow(data_filt()))))
  
  long_dist <- eventReactive(data_filt(), {
    distances <- compute_distances(data_filt())
    
    max_dist <- max(distances)
    max_dist_row <- max(which(distances == max_dist))
    
    long_dist_row(max_dist_row)
    max_dist
  })
  
  data_map <- reactive({
    data_filt() %>% 
      slice(long_dist_row(), long_dist_row() + 1)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      addMarkers(lng = data_map()$LON,
                 lat = data_map()$LAT,
                 popup = c("Beggining", paste0("End: ", round(long_dist(), 2), " mts sailed")))
  })
  
  
  output$data_filtered <- renderDataTable(data_map())
  
  # observe(message(paste0("long_dist() = ", long_dist())))
  # observe(message(paste0("long_dist_row() = ", long_dist_row())))
  
}

shinyApp(ui, server)
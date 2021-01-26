# Read data
data <- readRDS("data/ships.RDS")
ship_data <- readRDS("data/ship_data.RDS")
dict_shiptype <- readRDS("data/dict_shiptype.RDS")

# Module UI
module_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Input to select ship type
    selectInput(inputId = ns("ship_type"), label = "Select Vessel Type",
                choices = setNames(dict_shiptype$SHIPTYPE, dict_shiptype$ship_type)
    ),
    # Input to select ship name
    # choices = NULL because it'll be updated in server based on chosen type
    selectInput(inputId = ns("ship_name"), label = "Select Vessel",
                choices = NULL
    )
  )
}

# Module server
module_server <- function(id) {
  moduleServer( id, function(input, output, session) {

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

    # return the reactive
    return(
      reactive({
        data_filt()
      })
    )
  }
  )
}

# # Application
# library(shiny)
# library(tidyverse)
#
# app_ui <- function() {
#   fluidPage(
#     module_ui("dropdowns"),
#     dataTableOutput("table")
#   )
# }
# 
# app_server <- function(input, output, session) {
#   module_server("dropdowns")
#   data_filt <- module_server("dropdowns")
#   output$table <- renderDataTable({
#     data_filt()
#   })
# }
# 
# shinyApp(app_ui, app_server)
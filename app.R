

####  LIBRARIES  #### 

# Data wrangling
library(dplyr) 
library(data.table) 
library(geosphere)
# Shiny
library(shiny)
library(shinycssloaders) 
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
# Visualization
library(leaflet)
library(plotly)


####  Data  ####

# Importing data, Reading and defining variables using data.table
start_time <- Sys.time()
df <- 
  setDT(
    readRDS("ships.rds")
  )[, .(ship_type = as.factor(ship_type), 
        SHIPNAME = SHIPNAME %>% 
          stringr::str_replace(". ", "") %>% 
          stringr::str_to_title() %>%
          as.factor(),
        LAT = as.numeric(LAT),
        LON = as.numeric(LON),
        DATETIME = DATETIME %>% 
          stringr::str_replace_all("T", " ") %>% 
          stringr::str_replace_all("Z", "") %>% 
          lubridate::as_datetime())]

# Importing time
end_time <- Sys.time()
print(paste("Importing time:", round(end_time - start_time,1)))


####  Objects  ####

ship_types <- unique(df$ship_type)


####  Modules  ####

## shipName
shipName_ui <- function(id) {
  ns <- NS(id)
  column(
    width = 6,
    uiOutput(ns("shipName"))
  )
}
shipName_server <- function(id, names) {
  moduleServer(id, function(input, output, session) {
    output$shipName <- renderUI({
      selectInput(
        inputId = session$ns('ShipNameInput'), 
        label = "Select Ship name",
        choices = names(),
        selected = names()[1]
      )
    })
    return(reactive(input$ShipNameInput))
  })
}


####  UI  #### 

ui <- fluidPage(
  
  ## Background color
  setBackgroundColor(color = "#EDEDED"),
  
  ## ShinyDashboard
  useShinydashboard(),
  
  ## CSS
  tags$link(rel = "stylesheet", 
            type = "text/css", 
            href = "css_tuning.css"),
  
  br(),
  
  box(
    width = 12,
    solidHeader = FALSE,
    status = "primary",
    
    ####  Header  #### 
    fluidRow(
      column(
        width = 6,
        tags$img(src = 'ShinyLogo.png', width = 350)
      ),
      column(
        width = 6,
        offset = 0,
        br(),
        br(),
        column(
          width = 6,
          offset = 0,
          selectInput(
            inputId = 'ShipTypeInput', 
            label = "Select Ship Type",
            choices = ship_types,
            selected = ship_types[1]
          )
        ),
        ####  Inputs  #### 
        shipName_ui("shipName_id")
      )
    ),
    
    br(),
    
    fluidRow(
      ####  InfoBox  #### 
      infoBox(
        width = 3,
        title = "Ships Types",
        value = paste(
          ship_types %>% 
            length()
        ),
        subtitle = '',
        icon = icon("ship"),
        color = "aqua",
        fill = FALSE
      ),
      infoBoxOutput(
        width = 3,
        "valuebox_names"
      ),
      infoBoxOutput(
        width = 3,
        "valuebox_movements"
      ),
      infoBoxOutput(
        width = 3,
        "valuebox_distance"
      )
    ),
    
    br(),
    
    fluidRow(
      box(
        width = 5, 
        solidHeader = TRUE,
        title = "FREQUENCY MOVEMENTS",
        status = "primary",
        tabsetPanel(
          ####  Top 20  #### 
          tabPanel(
            "Top 20 ",
            withSpinner(
              plotlyOutput("plotlyFreq_top", height="410px"),
              type = 6,
              color = "#0D9AE0DA",
              size = 0.7
            )
          ),
          ####  Bottom 20  #### 
          tabPanel(
            "Bottom 20",
            withSpinner(
              plotlyOutput("plotlyFreq_bottom", height = "410px")  ,
              type = 6,
              color = "#0D9AE0DA",
              size = 0.7
            )
          )
        )
      ),
      ####  Map  ####
      box(
        width = 7, 
        solidHeader = TRUE,
        title = "LONGEST MOVEMENT",
        status = "primary",
        withSpinner(
          leafletOutput("map_trip", height="450px"),
          type = 6,
          color = "#0D9AE0DA",
          size = 0.7
        )
      )
    ),
    footer = HTML(
      "<h6 style='text-align: justify; line-height:1.2;'>
       <span style='color: #21B5CC;'>
       <b>Developer</b></span>: davidignacioam@gmail.com"
    )
  )
  
)


####  SERVER  #### 

server <- function(input, output, session) {
  
  ####  Name Value  #####
  shipNames <- reactive({
    unique(df[ship_type == input$ShipTypeInput][, .(SHIPNAME)]) #----> First Input Filter
  })
  ShipName <- shipName_server("shipName_id", shipNames) #----> Second Input Filter
  
  ####  DF Freq  #####
  df_mov <- reactive({
    
    # Data Table syntax
    df_f <- df[ship_type == input$ShipTypeInput] #----> First Input Filter
    df_f[, .(ship_type, 
             SHIPNAME)][, .(.N), 
                        by = .(ship_type,
                               SHIPNAME)][, .(Movements = N, 
                                              Type = SHIPNAME)]
    
  })
  
  ####  Top  #####
  output$plotlyFreq_top <- renderPlotly({
    
    # Dplyr syntax
    df_slice <- 
      as.data.frame(df_mov()) %>%
      arrange(desc(Movements)) %>% 
      slice(1:20) %>%
      mutate(Type = Type %>% as.character() %>% as.factor()) 
    
    # Visualization
    plot_ly()  %>%
      add_trace(
        orientation = 'h',
        type = 'scatter',
        mode = 'markers',
        y=df_slice$Type,
        x=df_slice$Movements,
        opacity = 0.9,
        marker = list(
          size = 10,
          color = '#0070B5'
        ),
        text = paste(
          "<b>Type: </b>", df_slice$Type,
          "<br><b>Movements: </b>", df_slice$Movements
        ),
        hoverinfo = 'text'
      ) %>%
      add_trace(
        orientation = 'h',
        type = 'bar',
        y=df_slice$Type,
        x=df_slice$Movements,
        width = ifelse(length(df_slice$Type) < 20, 0.1,
                       ifelse(length(df_slice$Type) < 10, 0.05, 
                              0.3)),
        opacity = 0.7,
        marker = list(
          color = '#00C0EF'
        ),
        text = paste(
          "<b>Type: </b>", df_slice$Type,
          "<br><b>Movements: </b>", df_slice$Movements
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        yaxis = list(
          autotick = FALSE,
          ticks = "outside",
          tick0 = 0,
          dtick = 0.25,
          ticklen = 5,
          tickwidth = 5,
          tickcolor = '#00C0EF',
          tickfont = list(size = 11)
        ),
        showlegend = FALSE
      ) %>% 
      config( 
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "zoomIn2d", 
                                   "zoomOut2d", "lasso2d", 
                                   "toggleSpikelines"), 
        toImageButtonOptions = list(
          format = "jpeg",
          filename = 
            paste(
              "Top 20 Vessel movements' frequency of ", 
              input$ShipTypeInput,
              sep = ""
            ),
          scale = 2
        )
      ) 
    
  })
  
  ####  Bottom  #####
  output$plotlyFreq_bottom <- renderPlotly({
    
    # Dplyr syntax
    df_slice <- 
      as.data.frame(df_mov()) %>%
      arrange(Movements) %>% 
      slice(1:20) %>%
      mutate(Type = Type %>% as.character() %>% as.factor()) 
    
    # Visualization
    plot_ly()  %>%
      add_trace(
        orientation = 'h',
        type = 'scatter',
        mode = 'markers',
        y=df_slice$Type,
        x=df_slice$Movements,
        opacity = 0.9,
        marker = list(
          size = 10,
          color = '#0070B5'
        ),
        text = paste(
          "<b>Type: </b>", df_slice$Type,
          "<br><b>Movements: </b>", df_slice$Movements
        ),
        hoverinfo = 'text'
      ) %>%
      add_trace(
        orientation = 'h',
        type = 'bar',
        y=df_slice$Type,
        x=df_slice$Movements,
        width = ifelse(length(df_slice$Type) < 20, 0.1,
                       ifelse(length(df_slice$Type) < 10, 0.05, 
                              0.3)),
        opacity = 0.7,
        marker = list(
          color = '#00C0EF'
        ),
        text = "",
        hoverinfo = 'text'
      ) %>%
      layout(
        yaxis = list(
          autotick = FALSE,
          ticks = "outside",
          tick0 = 0,
          dtick = 0.25,
          ticklen = 5,
          tickwidth = 5,
          tickcolor = '#00C0EF',
          tickfont = list(size = 11)
        ),
        showlegend = FALSE
      ) %>% 
      config( 
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "zoomIn2d", 
                                   "zoomOut2d", "lasso2d", 
                                   "toggleSpikelines"), 
        # Image Download 
        toImageButtonOptions = list(
          format = "jpeg",
          filename = 
            paste(
              "Bottom 20 Vessel movements' frequency of ", 
              input$ShipTypeInput,
              sep = ""
            ),
          scale = 2
        )
      ) 
    
  })
  
  ####  DF filtered  #####
  df_fil <- reactive({
    df[ship_type == input$ShipTypeInput & SHIPNAME %in% ShipName()]
  })
  
  ####  DF distance  #####
  df_dt <- reactive({
    
    # Data Table syntax
    start_time <- Sys.time()
    df_f <- df_fil()
    df_mut <- 
      # Calculating Lag by Group
      df_f[, .(SHIPNAME,
               DATETIME,
               LON,
               LAT)][, .(previous_LON = lag(LON), 
                         previous_LAT = lag(LAT),
                         LON,
                         LAT,
                         DATETIME), 
                     by = SHIPNAME]
    df_dt <- 
      # Distance and Time
      df_mut[, .(previous_LON, previous_LAT, #----> Previous location
                 LON, LAT, #----> Current location
                 distance = diag(distm(cbind(previous_LON, previous_LAT), 
                                       cbind(LON, LAT), 
                                       fun = distHaversine)), #----> Haversine method for distance
                 time = difftime(DATETIME, 
                                 lag(DATETIME), 
                                 units = "secs"))]
    
    # Loading time by inputs
    end_time <- Sys.time()
    print(
      paste("Loading time of inputs <", input$ShipTypeInput, 
            "> and <", ShipName(), ">: ",
            round(end_time - start_time,1), " seconds.",
            sep = ""),
    )
    
    # Dplyr syntax
    df_dt %>%
      as.data.frame() %>% 
      slice_max(distance) %>% #----> Main option: selecting the biggest distance
      slice_max(time) #----> Just in case: selecting the most recent 
    
  })
  
  ####  Map  #####
  output$map_trip <- renderLeaflet({
    
    # Visualization
    leaflet() %>%
      addTiles()  %>% 
      # Distance line
      addPolylines(lng = c(df_dt() %>% pull(previous_LON),
                           df_dt() %>% pull(LON)),
                   lat = c(df_dt() %>% pull(previous_LAT),
                           df_dt() %>% pull(LAT)),
                   color = "#00C0EF") %>%  
      # Begining marker
      addMarkers(lng = df_dt() %>% pull(previous_LON),
                 lat = df_dt() %>% pull(previous_LAT),
                 popup = "Begining of the trip",
                 label = "Begining of the trip",
                 labelOptions = labelOptions(
                   noHide = TRUE, 
                   direction = "bottom",
                   style = list(
                     "font-family" = "serif",
                     "font-style" = "initial",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                     "font-size" = "14px"
                   ))) %>%
      # End marker
      addMarkers(lng = df_dt() %>% pull(LON),
                 lat = df_dt() %>% pull(LAT),
                 popup = "End of the trip",
                 label = "End of the trip",
                 labelOptions = labelOptions(
                   noHide = T, 
                   direction = "bottom",
                   style = list(
                     "font-family" = "serif",
                     "font-style" = "initial",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                     "font-size" = "14px"
                   ))) %>%
      # Adding mini map
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 toggleDisplay = TRUE)
    
  })
  
  ####  InfoBox  #####
  output$valuebox_names <- renderInfoBox({
    infoBox(
      title = "Total names",
      value = paste(
        shipNames() %>% 
          nrow() 
      ),
      subtitle = paste("ship", input$ShipTypeInput),
      icon = icon("signature"),
      color = "aqua",
      fill = FALSE
    )
  })
  output$valuebox_movements <- renderInfoBox({
    infoBox(
      title = "Movements",
      value = paste(
        df_fil() %>%
          as.data.frame() %>%
          nrow()
      ),
      subtitle = paste("vessel", ShipName()), 
      icon = icon("map-marked-alt"),
      color = "aqua",
      fill = FALSE
    )
  })
  output$valuebox_distance <- renderInfoBox({
    infoBox(
      title = "Distance",
      value = paste(
        df_dt() %>% 
          pull(distance) %>% 
          round(0),
        "m"
      ),
      subtitle = 'longest movement',
      icon = icon("digital-tachograph"),
      color = "aqua",
      fill = FALSE
    )
  })
  
}


####  INTERFACE  #### 

shinyApp(ui = ui, server = server)



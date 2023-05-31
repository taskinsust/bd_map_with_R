library(shiny)
library(sf)
library(leaflet)
library(DT)

# Set the base path
setwd("G:\\Project Source\\Data Analysis\\BDMap")

# Define UI
ui <- fluidPage(
  
  #titlePanel("Bangladesh Map with SComm POP"),
  
  navbarPage(
    title = "Summit Communication Ltd",
    theme = "bootstrap",
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 fileInput("geojson", "Upload GeoJSON File", accept = ".geojson"),
                 fileInput("csv", "Upload CSV File", accept = ".csv"),
                 actionButton("drawMap", "Draw Map"),
                 actionButton("showListView", "Show List View"),
                 
               ),
               mainPanel(
                 div(id = "map", leafletOutput("map", height = "100vh")),
                 br(),
                 div(id = "csvData", dataTableOutput("csvData"))
               )
             )
    ),
    tabPanel("About")
  )
)

# Define server
server <- function(input, output) {
  
  data <- reactiveValues(
    geojson = NULL,
    csv = NULL
  )
  
  observeEvent(input$geojson, {
    data$geojson <- st_read(input$geojson$datapath)
  })
  
  observeEvent(input$csv, {
    data$csv <- read.csv(input$csv$datapath)
  })
  
  observeEvent(input$drawMap, {
    if (!is.null(data$geojson) && !is.null(data$csv)) {
      # Convert CSV data frame to spatial object 
      #data$csv <- st_as_sf(data$csv, coords = c("lon", "lat"), crs = st_crs(data$geojson))
      
      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 0)) %>%
          addTiles() %>%
          fitBounds(lng1 = 88.025, lat1 = 20.590, lng2 = 92.680, lat2 = 26.635) %>%
          
          addPolygons(data = data$geojson, weight = 2, color = "black", fillOpacity = 0.2) %>%
          addCircleMarkers(data = data$csv, lng = ~lon, lat = ~lat, color = ~ifelse(isup == 0, "red", "green"), radius = ~ifelse(isup == 0, 3, 1), weight = ~ifelse(isup == 0, 5, 2), 
                           label = ~paste(site_name, region, sub_center, upazilla, DistrictName, division, sep = "\n"),
                           popup = ~paste(site_name, "<br>Region", region))
          #addCircleMarkers(data = data$csv, lng = ~lon, lat = ~lat, color = "green", radius = 2, weight = 1, label = ~paste(site_name, region, sub_center, upazilla, DistrictName, division, sep = "\n"))
        
      })
    }
  })
  
  observeEvent(input$showListView, {
    if (!is.null(data$csv)) {
      output$csvData <- renderDataTable({
        datatable(data$csv[, -which(names(data$csv) == "region")], options = list(pageLength = 10, columnDefs = list(list(visible = FALSE, targets = 1))))
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)

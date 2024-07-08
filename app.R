#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(shinyWidgets)

# Load required packages

# Set your working directory

# Read sitedata from CSV file
# Replace with specific download location 
sitedata <- read.csv("/Users/mikayladotson/Online_App/Summary Illegal Dumping Site Inventory (1).csv")

# Create spatial points data frame
coordinates <- st_as_sf(sitedata, coords = c("Longitude", "Latitude"), crs = 4326)

#transform from NAD83 to WGS84

coordinates <- st_transform(coordinates, crs = 4326)

print(coordinates)

# shape file read
#replace with specific folder location, make sure to include all aspects 
shape_data <- st_read("/Users/mikayladotson/Online_App/wards_jackson")

#check alling the ticks for shape data
shape_data <- st_transform(shape_data, crs = 4326)

# Define UI
ui <- fluidPage(
  titlePanel("Illegal Dumping Sites in Mississippi"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(inputId = "source", label = "Select Zip Code",
                  choices = c("Reset", unique(sitedata$Zip_Code)),
                  selected = NULL,
                  multiple = TRUE)
      # Additional controls can be added here if needed
    ),
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("site_info") # Alternatively, use a shinyWidget for a more styled output
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMapPane("polygonsPane", zIndex = 410) %>%
      addMapPane("markersPane", zIndex = 420) %>% #stacks markers on front of panes 
      addPolygons(data = shape_data, color = "#9B9B9B", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  options = pathOptions(pane="polygonsPane")) %>%
      addCircleMarkers(data = sitedata,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       popup = ~Site_ID,
                       layerId = ~Site_ID,
                       options = pathOptions("markersPane")) # ensures markers are in front
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    site_data <- sitedata%>% filter(Site_ID == click$id)
    
    if (nrow(site_data) > 0) {
      leafletProxy("map") %>%
        clearPopups() %>%
        addPopups(
          lng = site_data$Longitude,
          lat = site_data$Latitude,
          popup = paste(
            "<strong>Site Id:</strong>", site_data$Site_ID, "<br>",
            "<strong>Perimeter:</strong>", site_data$Perimeter_m, "<br>",
            "<strong>City:</strong>", site_data$City, "<br>",
            "<strong>Longitude:</strong>", site_data$Longitude, "<br>",
            "<strong>Latitude:</strong>", site_data$Latitude, "<br>",
            "<strong>Zip Code:</strong>", site_data$Zip_Code
            
          )
        )
    }
  })
  #updating for when a user selects a zip code it filters only for that one(s)
  observe({
    selected_zip <- input$source
    
    filtered_data <- if (is.null(selected_zip) || "Reset" %in% selected_zip) {
      sitedata
    } else {
      sitedata %>% filter(Zip_Code %in% selected_zip)
    }
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = filtered_data,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       popup = ~City,
                       layerId = ~Site_ID,
                       options = pathOptions(pane = "markersPane"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
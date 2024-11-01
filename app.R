#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(shinyWidgets)
library(googledrive)
library(DT)
library(markdown) # Ensure this is included to render Markdown

# Read sitedata from CSV file
sitedata <- read.csv("Summary Illegal Dumping Site Inventory (1).csv")

# Create spatial points data frame
coordinates <- st_as_sf(sitedata, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform from NAD83 to WGS84
coordinates <- st_transform(coordinates, crs = 4326)

print(coordinates)

# Shape file read
shape_data <- st_read("jackson_wards.geojson")

# Check alling the ticks for shape data
shape_data <- st_transform(shape_data, crs = 4326)

# Define UI
ui <- navbarPage("Illegal Dumping Sites in Mississippi", id="nav",
                 includeCSS("style.css"),
                 
                 tags$script(HTML("
                      $(document).on('click', '.leaflet-popup-content-wrapper', function(){
                            $(this.toggleClass('expanded');
                                  });
                          ")),
                 tabPanel("Map",icon = icon("map"),
                          div(class="outer",
                              actionButton("button", "User Guide"),
                              leafletOutput("map", width = "100%", height = "700px"),
                              absolutePanel(
                                id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                width = 330, height = "auto",
                                style = "background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);",
                                h2("Search Options"),
                                pickerInput(inputId = "source", label = "Select Zip Code",
                                            choices = c(unique(sitedata$Zip_Code)),
                                            selected = NULL,
                                            multiple = TRUE),
                                pickerInput(inputId = "City_source", label = "Select City",
                                            choices = c(unique(sitedata$City)),
                                            selected = NULL, 
                                            multiple = TRUE),
                                br(),
                                actionButton("view_data", "View Data Table")
                                
                              ),
                              verbatimTextOutput("site_info")
                          )
                 ),
                 
                 tabPanel("Data Table", icon = icon("table"),
                          DT::dataTableOutput("data_table")
                          ),
                 
                 tabPanel("Data Dictionary", icon = icon("book"),
                          htmlOutput("data_dictionary")
                 ),
                 navbarMenu("About", icon = icon("info-circle"),
                            tabPanel("About the Developer",
                                     htmlOutput("about_developer") 
                            ),
                            tabPanel("Mission", fluid = TRUE, 
                                     
                                     column(6,
                                            br(),
                                            h2("Why Have You Developed An Illegal Dumping Site Reporting Tool?"),
                                            h5(p("Community Noise Lab believes that information is a crucial first-step in eliminating the illegal dumping issue in Mississippi. By providing all with information about illegal dumping sites in your community and what is in them, you have the data you need to demand change from your elected officials.")
                                            )
                                     )
                                     
                            ),
                            tabPanel("Reporting a Site", fluid = TRUE,
                                     
                                     column(6,
                                            br(),
                                            h2("How to Report an Illegal Dumping Site"),
                                            h5(p("Our team, which consists of local high school and community college students, will go out and assess the illegal dumping site for a wide variety of environmental concerns.If you would like to report an illegal dumping site in your community, you can fill out our",
                                                 a("Illegal Dumping Site Reporting Tool.",
                                                   href="https://www.jotform.com/form/241495365663162"))
                                            )
                                            
                                            
                                     ))
                            
                 )
)                 
# Define server logic
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      addMapPane("polygonsPane", zIndex = 410) %>%
      addMapPane("markersPane", zIndex = 420) %>% # Stacks markers on front of panes 
      addPolygons(data = shape_data, color = "#9B9B9B", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  options = pathOptions(pane = "polygonsPane")) %>%
      addCircleMarkers(data = sitedata,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       popup = ~Site_ID,
                       layerId = ~Site_ID,
                       options = pathOptions("markersPane")) # Ensures markers are in front
  })

  observeEvent(input$view_data, {
    updateTabsetPanel(session, "nav", selected = "Data Table")
  })
  
  # Render the full CSV data in the Data Table tab
  output$data_table <- DT::renderDataTable({
    DT::datatable(sitedata, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    site_data <- sitedata %>% filter(Site_ID == click$id)
    
    if (nrow(site_data) > 0) {
      tryCatch({
        leafletProxy("map") %>%
          clearPopups() %>%
          addPopups(
            lng = site_data$Longitude,
            lat = site_data$Latitude,
            popup = paste(
              "<div style='width: 300px; max-height: 400px; overflow-y: auto; font-family: Arial, sans-serif;'>",
              "<h3 style='margin-top: 0;'>", "Site ", site_data$Site_ID, "</h3>",
              "<table style='width: 100%; border-collapse: collapse;'>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Perimeter</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Perimeter_m, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>City</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$City, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Longitude</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Longitude, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Latitude</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Latitude, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Zip Code</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Zip_Code, "</td></tr>",
              if (!is.na(site_data$Image_link) && site_data$Image_link != "") {
                paste("<tr><th colspan='2' style='padding: 5px; border-bottom: 1px solid #ddd;'><img src='", site_data$Image_link, "' width='100%' height='auto'></th></tr>")
              } else {
                "<tr><th colspan='2' style='padding: 5px; border-bottom: 1px solid #ddd;'>No image available.</th></tr>"
              },
              "<tr><th colspan='2' style='padding: 5px; border-bottom: 1px solid #ddd; text-align:center;'>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Key Containments</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Key_Cont, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Measured Total VOC Level</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Voc_level, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Measured PM 10 Level</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$PM_10, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Measured PM 2.5 Level</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$PM_2.5, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Measured Noise Level</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Noise, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Measured Air Quality Level</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Air, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Available in the Soil Sample Library</th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Soil_sample, "</td></tr>",
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Date Last Assessed </th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Updates, "</td></tr>",
              "</table>",
              "</div>"
            )
          )
      })
    }
  })
  
  # Define reactive expressions for filters
  filtered_data_by_zip <- reactive({
    selected_zip <- input$source
    
    if (is.null(selected_zip)) {
      sitedata
    } else {
      sitedata %>% filter(Zip_Code %in% selected_zip)
    }
  })
  
  filtered_data_by_city <- reactive({
    selected_city <- input$City_source
    
    if (is.null(selected_city)) {
      sitedata
    } else {
      sitedata %>% filter(City %in% selected_city)
    }
  })
  
  # Observe the filtered data and update the map
  observe({
    # Get the filtered data based on zip code
    data_to_plot <- filtered_data_by_zip()
    
    # If city filter is applied, filter further by city
    if (!is.null(input$City_source) && !"Reset" %in% input$City_source) {
      data_to_plot <- data_to_plot %>% filter(City %in% input$City_source)
    }
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = data_to_plot,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       popup = ~City,
                       layerId = ~Site_ID,
                       options = pathOptions(pane = "markersPane"))
  })
  
  
  # Render the about.Rmd file
  output$about_developer <- renderUI({
    includeMarkdown("About_the_developer.Rmd")
  })
  output$data_dictionary <- renderUI ({
    includeMarkdown("Data_Dictionary.Rmd")
  })
  
  # observEvent for modal upon loading 
  observe({
    showModal(modalDialog(
      title = "Welcome!",
      "Welcome to Community Noise Lab’s Illegal Dumping Site Database. For now, our database only includes illegal dumping sites across the state of Mississippi.
          If you are interested in knowing if there are any reported illegal dumping sites in your community, you can search our database by zip code and city.
For each reported illegal dumping site in our database, you will find a wealth of information, including photos, soil pollution reports, 
          water pollution reports, air quality levels, and noise pollution levels.",
      easyClose = TRUE, # Allows closing the modal by clicking outside
      footer = modalButton("Close") # Adds a close button
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

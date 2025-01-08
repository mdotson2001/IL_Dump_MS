library(readr)
library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)
library(DT)
library(markdown) # Ensure this is included to render Markdown

# Read sitedata from CSV file
sitedata <- readr::read_csv("www/Summary Illegal Dumping Site Inventory (1).csv")

# Create spatial points data frame
coordinates <- st_as_sf(sitedata, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 4326)

print(coordinates)

# Define UI
ui <- navbarPage(
    "Illegal Dumping Sites in Mississippi", id="nav",
        includeCSS("style.css"),
                 tabPanel("Map",icon = icon("map"),
                          div(
                            class="outer",
                              actionButton("button", "User Guide"),
                              leafletOutput("map", width = "100%", height = "700px"),
                              absolutePanel(
                                id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                width = 330, height = "auto",
                                style = "background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);",
                                h2("Search Options"),
                                pickerInput(inputId = "source", label = "Select Zip Code", choices = unique(sitedata$Zip_Code),
                                            selected = NULL,
                                            multiple = TRUE),
                                pickerInput(inputId = "City_source", label = "Select City",
                                            choices = unique(sitedata$City),
                                            selected = NULL, 
                                            multiple = TRUE)
    
                                
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
                            tabPanel("About the Developer", htmlOutput("about_developer")),
                            tabPanel("Mission", 
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
  
  reactive_data <- reactive({
    data <-sitedata
    if(!is.null(input$source)) data <- data %>% filter(Zip_Code %in% input$source)
    if(!is.null(input$source)) data <- data%>% filter(City %in% input$City_source)
    data
  })

  #Render Map
  output$map <- renderLeaflet({
    leaflet(coordinates) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      addCircleMarkers(data = sitedata,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       popup = ~Site_ID,
                       layerId = ~Site_ID, 
                       radius = 4, color = "00732", fillOpacity = 0.7)
  })

  
  ##Pop-UP Info###
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
              "<tr><th style='text-align:left; padding: 5px; border-bottom: 1px solid #ddd;'>Date Last Assessed </th>",
              "<td style='padding: 5px; border-bottom: 1px solid #ddd;'>", site_data$Updates, "</td></tr>",
              "</table>",
              "</div>"
            )
          )
      })
    }
  })
  
observe({
  filtered <- reactive_data()
  leafletProxy("map") %>%
    clearMarkers()%>%
    addCircleMarkers(
      data=filtered, 
      lng = ~Longitude, lat = ~Latitude,
      popup = ~City, layerId = ~Site_ID
    )
})

  # Render the Data Table and necessary actions for button in DT###################################
  output$data_table <-renderDataTable({
    datatable(sitedata, options = list(pageLength = 10, scrollX = TRUE))
                                     })

  # Render the about.Rmd file#################
  output$about_developer <- renderUI({ includeMarkdown("About_the_developer.Rmd") })
  output$data_dictionary <- renderUI ({ includeMarkdown("Data_Dictionary.Rmd") })
  
  
  # Welcome Modal#######
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

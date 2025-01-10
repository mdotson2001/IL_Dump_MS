
library(readr)
library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)
library(DT)
library(markdown) 

# Read sitedata from CSV file
sitedata <- readr::read_csv("www/Summary Illegal Dumping Site Inventory (1).csv")

# Create spatial points data frame
coordinates <- st_as_sf(sitedata, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 4326)


# Define UI
ui <- navbarPage(
    "Illegal Dumping Sites in Mississippi", id="nav",
    
  includeCSS("www/style.css"),
  tags$head(
    tags$script(HTML("
    $(document).on('click', '.go-to-marker', function() {
      var site_id = $(this).attr('id').replace('marker-', '');
      Shiny.setInputValue('go_to_marker', site_id);
    });
  "))
  ),
  tags$head(
    tags$script(HTML("
    $(document).on('click', '.go-to-table', function() {
      var site_id = $(this).attr('id').replace('go-to-table-', '');  // Extract Site_ID
      Shiny.setInputValue('popup_more_info', site_id);  // Send Site_ID to Shiny
    });
  "))
  ),
  tags$head(
    tags$style(HTML("
    .modal {
      display: block !important;
      visibility: visible !important;
    }
  "))
  ),
  
  tabPanel(
          "Map",
          icon = icon("map"),
          div(
              class="outer",
              actionButton("button", "User Guide", class = "action-button"),
              leafletOutput("map", width = "100%", height = "700px"),
              absolutePanel(
                    id = "controls", 
                    class = "panel panel-default", 
                    fixed = TRUE,
                    draggable = TRUE,
                    top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    h2("Search Options"),
                    pickerInput(
                        inputId = "source", 
                        label = "Select Zip Code", 
                        choices = unique(sitedata$Zip_Code),
                        selected = NULL,
                        multiple = TRUE),
                    pickerInput(
                        inputId = "City_source", 
                        label = "Select City",
                        choices = unique(sitedata$City),
                        selected = NULL, 
                        multiple = TRUE
                        )
                ),
            verbatimTextOutput("site_info")
        )
    ),
                 
      tabPanel(
        "Data Table",
        icon = icon("table"),
        DT::dataTableOutput("data_table")
      ),
  
      tabPanel(
        "Data Dictionary",
        icon = icon("book"),
        htmlOutput("data_dictionary")
      ),
  
      navbarMenu(
          "About",
          icon = icon("info-circle"),
          tabPanel(
              "About the Developer", 
              htmlOutput("about_developer")
          ),
          tabPanel(
              "Mission",
              column(
                6,
                br(),
                h2("Why Have You Developed An Illegal Dumping Site Reporting Tool?"),
                h5(
                  p(
                    "Community Noise Lab believes that information is a crucial first-step in eliminating the illegal dumping issue in Mississippi. By providing all with information about illegal dumping sites in your community and what is in them, you have the data you need to demand change from your elected officials.")
                    )
                  )
                                     
          ),
            tabPanel(
              "Reporting a Site", 
                  column(
                    6,
                    br(),
                    h2("How to Report an Illegal Dumping Site"),
                    h5(
                      p(
                        "Our team, which consists of local high school and community college students, will go out and assess the illegal dumping site for a wide variety of environmental concerns.If you would like to report an illegal dumping site in your community, you can fill out our",
                        a("Illegal Dumping Site Reporting Tool.",href="https://www.jotform.com/form/241495365663162")
                        )
                      )
                      )
              )
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
      addCircleMarkers(
        data = sitedata,
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~Site_ID,
        layerId = ~Site_ID, 
        radius = 4, 
        color = "#00732", 
        fillOpacity = 0.7
        )
  })

  
  ##Pop-UP Info###
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    site_data <- sitedata %>% filter(Site_ID == click$id)
    
    # Handle case where site_data is empty
    if (nrow(site_data) == 0) {
      # Display a default popup for unmatched markers
      leafletProxy("map") %>%
        clearPopups() %>%
        addPopups(
          lng = click$lng,
          lat = click$lat,
          popup = "No data available for this site."
        )
      return()
    }
    
    # Proceed with the normal popup logic
    leafletProxy("map") %>%
      clearPopups() %>%
      addPopups(
        lng = ifelse(!is.na(site_data$Longitude), site_data$Longitude, 0),
        lat = ifelse(!is.na(site_data$Latitude), site_data$Latitude, 0),
        popup = paste0(
          "<div class='scrollable-popup'>",
          "<div class= 'popup-title'> Site Id: ", ifelse(!is.na(site_data$Site_ID), site_data$Site_ID, "Unknown"), "</div><br>", 
          "<strong> Perimeter:</strong> ", ifelse(!is.na(site_data$Perimeter_m), site_data$Perimeter_m, "N/A"), "<br>", 
          "<strong> City:</strong> ", ifelse(!is.na(site_data$City), site_data$City, "Unknown"), "<br>", 
          "<strong> Coordinates:</strong> ", ifelse(!is.na(site_data$Longitude), site_data$Longitude, "N/A"), ", ", 
          ifelse(!is.na(site_data$Latitude), site_data$Latitude, "N/A"), "<br>",
          "<strong> Zip Code:</strong> ", ifelse(!is.na(site_data$Zip_Code), site_data$Zip_Code, "N/A"), "<br>",
          if (!is.na(site_data$Image_link) && site_data$Image_link != "") {
            paste0("<strong>Site Image:</strong><br><img src='", site_data$Image_link, "' width='200px' height='auto'><br>")
          } else {
            "<strong>Site Image:</strong> No image available.<br>"
          },
          "<strong>Key Contaminants:</strong> ", ifelse(!is.na(site_data$Key_Cont), sitedata$Key_Cont, "Not Available"), "<br>", 
          "<strong>VOC 10 Levels:</strong> ", ifelse(!is.na(site_data$Voc_level), sitedata$Voc_level, "Not Available"), "<br>",
          "<strong>PM 10 Levels:</strong> ", ifelse(!is.na(site_data$PM_10), sitedata$PM_10, "Not Available"), "<br>", 
          "<strong>PM 2/5 Levels:</strong> ", ifelse(!is.na(site_data$PM_2.5), sitedata$PM_2.5, "Not Available"), "<br>",
          "<strong>Air quality:</strong> ", ifelse(!is.na(site_data$Air), sitedata$Air, "Not Available"), "<br>", 
          "<strong>Average Noise Level:</strong> ", ifelse(!is.na(site_data$Noise), sitedata$Noise, "Not Available"), "<br>", 
          "<strong>Soil Sample Collected?:</strong> ", ifelse(!is.na(site_data$Soil_sample), sitedata$Soil_sample, "No"), "<br>", 
          "<strong> Updates:</strong> ", ifelse(!is.na(site_data$Updates), site_data$Updates, "No updates"), "<br>",
          "<button id='go-to-table-", site_data$Site_ID, "' class='btn btn-primary go-to-table'>More Info</button>",
          "</div>"
        )
      )
  })  
  
  observeEvent(input$popup_more_info, {
    req(input$popup_more_info)
    
    updateTabsetPanel(session, inputId = "nav", selected = "Data Table")
  })

  
# Data Table ####
output$data_table <- renderDataTable({
    datatable(
      sitedata %>%
        mutate(
          Action = paste0(
            '<button class="btn btn-primary go-to-marker" id="marker-', Site_ID, '">Go to Marker</button>'
          )
        ),
      escape = FALSE,  # Allow HTML in the table
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
## Goes to Marker from Data Table#
  observeEvent(input$go_to_marker, {
    site_id <- input$go_to_marker  # Get the clicked Site_ID
    site_data <- sitedata %>% filter(Site_ID == site_id)
    
    if (nrow(site_data) > 0) {
      updateTabsetPanel(session, inputId = "nav", selected = "Map")
      leafletProxy("map") %>%
        setView(lng = site_data$Longitude, lat = site_data$Latitude, zoom = 15) %>%
        clearPopups() %>%
        addPopups(
          lng = site_data$Longitude,
          lat = site_data$Latitude,
          popup = paste0(
            "<div class='scrollable-popup'>",
            "<strong> Site Id:</strong> ", ifelse(!is.na(site_data$Site_ID), site_data$Site_ID, "Unknown"), "<br>", 
            "<strong> Perimeter:</strong> ", ifelse(!is.na(site_data$Perimeter_m), site_data$Perimeter_m, "N/A"), "<br>", 
            "<strong> City:</strong> ", ifelse(!is.na(site_data$City), site_data$City, "Unknown"), "<br>", 
            "<strong> Coordinates:</strong> ", ifelse(!is.na(site_data$Longitude), site_data$Longitude, "N/A"), ", ", 
            ifelse(!is.na(site_data$Latitude), site_data$Latitude, "N/A"), "<br>",
            "<strong> Zip Code:</strong> ", ifelse(!is.na(site_data$Zip_Code), site_data$Zip_Code, "N/A"), "<br>",
            if (!is.na(site_data$Image_link) && site_data$Image_link != "") {
              paste0("<strong>Site Image:</strong><br><img src='", site_data$Image_link, "' width='200px' height='auto'><br>")
            } else {
              "<strong>Site Image:</strong> No image available.<br>"
            },
            "<strong>Key Contaminants:</strong> ", ifelse(!is.na(site_data$Key_Cont), sitedata$Key_Cont, "Not Available"), "<br>", 
            "<strong>VOC 10 Levels:</strong> ", ifelse(!is.na(site_data$Voc_level), sitedata$Voc_level, "Not Available"), "<br>",
            "<strong>PM 10 Levels:</strong> ", ifelse(!is.na(site_data$PM_10), sitedata$PM_10, "Not Available"), "<br>", 
            "<strong>PM 2/5 Levels:</strong> ", ifelse(!is.na(site_data$PM_2.5), sitedata$PM_2.5, "Not Available"), "<br>",
            "<strong>Air quality:</strong> ", ifelse(!is.na(site_data$Air), sitedata$Air, "Not Available"), "<br>", 
            "<strong>Average Noise Level:</strong> ", ifelse(!is.na(site_data$Noise), sitedata$Noise, "Not Available"), "<br>", 
            "<strong>Soil Sample Collected?:</strong> ", ifelse(!is.na(site_data$Soil_sample), sitedata$Soil_sample, "No"), "<br>", 
            "<strong> Updates:</strong> ", ifelse(!is.na(site_data$Updates), site_data$Updates, "No updates"), "<br>",
            "</div>"
          )
        )
    }
  })
  
# Filters data# 
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
 # output$data_table <-renderDataTable({
    #datatable(sitedata, options = list(pageLength = 10, scrollX = TRUE))
                                    # })

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
      easyClose = TRUE, 
      footer = modalButton("Close") 
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

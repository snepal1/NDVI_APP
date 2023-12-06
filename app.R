library(shiny)
library(rstac)
library(terra)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(ggplot2)
library(rasterVis)
library(raster)

# Define UI 
ui <- fluidPage(
  tags$style(type = "text/css", "
    body {
      background-color: black;
      color: #00B779;
    }
  "),
  fluidRow(
    column(12, leafletOutput("mymap")),
    column(6, numericInput("min_lat", "Minimum Latitude", 0)),
    column(6, numericInput("min_lon", "Minimum Longitude", 0)),
    column(6, numericInput("max_lat", "Maximum Latitude", 0)),
    column(6, numericInput("max_lon", "Maximum Longitude", 0)),
    column(12, verbatimTextOutput("drawnCoordinates"))
  ),
  fluidRow(
    column(6, plotOutput("naipPlot", height = "800px")),  # Set offset to center the plot
    column(6, plotOutput("histogramPlot", height = "800px")),
    column(6, plotOutput("gndviPlot", height = "800px")), 
    column(6, plotOutput("gndviHistogram", height = "800px"))  # New output for GNDVI histogram plot
  )
)

# Define server logic 
server <- function(input, output, session) {
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  
  # Reactive values to store data
  naip_ndvi_data <- reactiveVal(NULL)
  naip_gndvi_data <- reactiveVal(NULL)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -120.75, lat = 39.72, zoom = 10) %>%
      addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = NULL,
        polygonOptions = NULL,
        circleOptions = NULL,
        markerOptions = NULL,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      addLayersControl(overlayGroups = c('draw'), options = layersControlOptions(collapsed = FALSE))
  })
  
  output$drawnCoordinates <- renderPrint({
    # Extract bounding box coordinates from the drawn shape
    drawn_feature <- input$mymap_draw_new_feature
    if (!is.null(drawn_feature)) {
      bbox <- drawn_feature$geometry$coordinates
      bbox <- matrix(unlist(bbox[[1]]), ncol = 2, byrow = TRUE)
      
      # Get the minimum and maximum latitude and longitude
      min_lat <- min(bbox[, 2])
      min_lon <- min(bbox[, 1])
      max_lat <- max(bbox[, 2])
      max_lon <- max(bbox[, 1])
      
      # Update numeric input values
      updateNumericInput(session, "min_lat", value = min_lat)
      updateNumericInput(session, "min_lon", value = min_lon)
      updateNumericInput(session, "max_lat", value = max_lat)
      updateNumericInput(session, "max_lon", value = max_lon)
      
      cat("Coordinates:", toJSON(bbox), "\n")
      return(NULL)  # Prevents the rendering of NULL
    }
  })
  
  output$naipPlot <- renderPlot({
    # Fetch NAIP image URL
    it_obj <- s_obj %>%
      stac_search(collections = "naip", bbox = c(input$min_lon, input$min_lat, input$max_lon, input$max_lat)) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())
    
    if (!is.null(it_obj$features) && length(it_obj$features) > 0) {
      url <- it_obj$features[[1]]$assets$rendered_preview$href
      
      # Read the NAIP image using terra
      naip_data <- rast(url)
      
      naip_csf_br <- brick(naip_data)
      
      # Calculate NDVI and GNDVI
      naip_ndvi <- (naip_csf_br[[4]] - naip_csf_br[[1]]) / (naip_csf_br[[4]] + naip_csf_br[[1]])
      naip_gndvi <- (naip_csf_br[[2]] - naip_csf_br[[4]]) / (naip_csf_br[[2]] + naip_csf_br[[4]])
      
      # Set reactive values
      naip_ndvi_data(naip_ndvi)
      naip_gndvi_data(naip_gndvi)
      
      # Plot the NDVI with x and y-axes
      plot(naip_ndvi,
           main = "NDVI",
           axes = TRUE, box = FALSE,
           xlab = "Longitude", ylab = "Latitude")
      # Add CSS styling to set the background color
      tags$head(
        tags$style(
          HTML("
          #naipPlot {
            background-color: grey;
          }
        ")
        )
      )
    } else {
      print("No NAIP images found within the specified criteria.")
    }
  })
  
  output$histogramPlot <- renderPlot({
    # Get data from reactive value
    naip_ndvi <- naip_ndvi_data()
    
    if (!is.null(naip_ndvi)) {
      # Plot histogram of NDVI values
      hist(naip_ndvi,
           main = "NDVI: Distribution of pixels",
           col = "springgreen",
           xlab = "NDVI Index Value")
      # Add a vertical red line at cutoff value
      abline(v = 0.5, col = "red", lwd = 2)
      # Add text labels
      text(0.45, 1000, "Non-Trees", pos = 2, col = "black", font = 2)
      text(0.55,1000, "Trees", pos = 4, col = "black", font = 2)
    } else {
      print("No NDVI data available.")
    }
  })
  
  output$gndviPlot <- renderPlot({
    # Get data from reactive value
    naip_gndvi <- naip_gndvi_data()
    
    if (!is.null(naip_gndvi)) {
      # Plot GNDVI with x and y-axes
      plot(naip_gndvi,
           main = "NDWI",
           axes = TRUE, box = FALSE,
           xlab = "Longitude", ylab = "Latitude")
      # Add CSS styling to set the background color
      tags$head(
        tags$style(
          HTML("
          #gndviPlot {
            background-color: grey;
          }
        ")
        )
      )
    } else {
      print("No GNDVI data available.")
    }
  })
  
  output$gndviHistogram <- renderPlot({
    # Get data from reactive value
    naip_gndvi <- naip_gndvi_data()
    
    if (!is.null(naip_gndvi)) {
      # Plot histogram of GNDVI values
      hist(naip_gndvi,
           main = "NDWI: Distribution of pixels",
           col = "skyblue",
           xlab = "NDWI Index Value")
      # Add a vertical red line at cutoff value
      abline(v = -0.55, col = "red", lwd = 2)
      abline(v = -0.4, col = "red", lwd = 2)
      # Add text labels
      text(-0.8, 2000, "Vegetation", pos = 2, col = "black", font = 2)
      text(-0.5, 2000, "Water", pos = 4, col = "black", font = 2)
      text(-0.3, 2000, "Soil", pos = 4, col = "black", font = 2)
    } else {
      print("No GNDVI data available.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

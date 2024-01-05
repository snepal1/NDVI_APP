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
    .vertical-gap {
      margin-bottom:20px; /* Adjust the margin to control vertical spacing */
    }
    .center-plot {
      text-align: center;
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
  
  tags$div(class = "vertical-gap"),
  
  fluidRow(
    column(6, plotOutput("naipPlot", height = "800px")),
    column(6, plotOutput("histogramPlot", height = "800px"))),
  tags$div(class = "vertical-gap"),  
  
  fluidRow(
    column(6, plotOutput("gndviPlot", height = "800px")),
    column(6, plotOutput("gndviHistogram", height = "800px"))),
  tags$div(class = "vertical-gap"),
  
  fluidRow(
    column(6, plotOutput("RGBplot", height = "800px")),
    column(6, plotOutput("GCIplot", height = "800px")))
)
# Define server logic 
server <- function(input, output, session) {
  
  # Default values
  defaultMinLat <- 0
  defaultMinLon <- 0
  defaultMaxLat <- 0
  defaultMaxLon <- 0
  
 # Add a reactive value to store the parsed extent
  parsed_extent <- reactiveVal(NULL)
  
  


#url <- "https://posit.intelifore.com/ndvi_analysis_II/?extent=[[[-122.31065086929824,41.01774275999912],[-122.22844544285316,41.01774275999912],[-122.22844544285316,41.072316893372005],[-122.31065086929824,41.072316893372005],[-122.31065086929824,41.01774275999912]]]"


observe({
  queryParams <- parseQueryString(session$clientData$url_search)
  
  # Update numeric parameters according to the request string  
  if (!is.null(queryParams$extent)) {
    # Extracting the extent part from the URL
    extent_match <- regmatches(queryParams$extent, regexpr("\\[\\[.*\\]\\]", queryParams$extent))[[1]]
    
    # Converting the extent string to a numeric matrix
    extent <- jsonlite::fromJSON(gsub("[\\[\\]]", "", extent_match), simplifyMatrix = TRUE)
    
    # Convert the matrix to two columns (longitude and latitude)
    extent_matrix <- matrix(extent, ncol = 2, byrow = TRUE)
    
    if (length(extent_matrix) == 10) {
      parsed_extent(extent_matrix)
      
      # Update numeric input values
      updateNumericInput(session, "min_lat", value =  extent_matrix[4, 1])
      updateNumericInput(session, "min_lon", value =  extent_matrix[1, 1])
      updateNumericInput(session, "max_lat", value =  extent_matrix[5, 1]) 
      updateNumericInput(session, "max_lon", value =  extent_matrix[2, 1])
    }
  }
})

  
  # Add custom CSS using shinyjs
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  
  # Reactive values to store data
  naip_ndvi_data <- reactiveVal(NULL)
  naip_gndvi_data <- reactiveVal(NULL)
  naip_rgb_data<-reactiveVal(NULL)
  naip_gci_data <- reactiveVal(NULL)
  
  output$mymap <- renderLeaflet({
    if (!is.null(parsed_extent()) && length(parsed_extent()) == 4) {
      leaflet() %>%
        addTiles() %>%
        fitBounds(
          lng1 = parsed_extent()[1],
          lat1 = parsed_extent()[2],
          lng2 = parsed_extent()[3],
          lat2 = parsed_extent()[4]
        ) %>%
        addDrawToolbar(
          targetGroup = 'draw',
          polylineOptions = NULL,
          polygonOptions = NULL,
          circleOptions = NULL,
          markerOptions = NULL,
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        ) %>%
        addLayersControl(overlayGroups = c('draw'), options = layersControlOptions(collapsed = FALSE))
    } else {
      map <- leaflet() %>%
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
      
      # Check if a drawn feature is present
      drawn_feature <- input$mymap_draw_new_feature
      if (!is.null(drawn_feature)) {
        bbox <- drawn_feature$geometry$coordinates
        bbox <- matrix(unlist(bbox[[1]]), ncol = 2, byrow = TRUE)
        map <- map %>% fitBounds(lng1 = bbox[1, 1], lat1 = bbox[1, 2], lng2 = bbox[3, 1], lat2 = bbox[3, 2])
      }
      
      map
    }
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
  
  # Create a temporary output directory
  output_dir <- tempdir()
  dir.create(output_dir, showWarnings = FALSE)
  
  # Add a reactive value to store temporary output directory
  output_dir_path <- reactiveVal(output_dir)
  
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
      naip_gci <- (naip_csf_br[[4]]- naip_csf_br[[1]])/sqrt(naip_csf_br[[4]]^2+naip_csf_br[[1]]+0.5) 
      #naip_gci <- (naip_csf_br[[4]]- (naip_csf_br[[2]]-(1.7*(naip_csf_br[[3]]-naip_csf_br[[1]]))))/(naip_csf_br[[4]]+(naip_csf_br[[2]]-(1.7*(naip_csf_br[[3]]-naip_csf_br[[1]]))))
      
      # Set reactive values
      naip_ndvi_data(naip_ndvi)
      naip_gndvi_data(naip_gndvi)
      naip_rgb_data(naip_csf_br)
      naip_gci_data(naip_gci)
      # Define a color palette
      color_palette <- colorRampPalette(c("red", "yellow", "brown","cyan"))
      # Plot the NDVI with x and y-axes
      plot(naip_ndvi,
           main = "NDVI",
           axes = TRUE, box = FALSE,
           xlab = "Longitude", ylab = "Latitude",
           col=color_palette(100))
      
      # Set TIFF file path
      tiff_file <- file.path(output_dir_path(), "naip_ndvi.tiff")
      # Save NDVI as TIFF
      rast(naip_ndvi) |> writeRaster(tiff_file, overwrite = TRUE)
      
    } else {
      print("No NAIP images found within the specified criteria.")
      return(NULL)  # Prevent further rendering
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
  
  color_palette <- colorRampPalette(c("red", "yellow", "brown","cyan"))
  
  output$gndviPlot <- renderPlot({
    # Get data from reactive value
    naip_gndvi <- naip_gndvi_data()
    
    if (!is.null(naip_gndvi)) {
      # Plot GNDVI with x and y-axes
      plot(naip_gndvi,
           main = "NDWI",
           axes = TRUE, box = FALSE,
           xlab = "Longitude", ylab = "Latitude",
           col= color_palette(100))
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
      # Set TIFF file path
      tiff_file <- file.path(output_dir_path(), "naip_gndvi.tiff")
      # Save NDVI as TIFF
      rast(naip_gndvi) |> writeRaster(tiff_file, overwrite = TRUE)
      
    } else {
      print("No NAIP images found within the specified criteria.")
      return(NULL)  # Prevent further rendering
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
  
  output$RGBplot<- renderPlot({
    # Get data from reactive value
    naip_rgb <- naip_rgb_data()   
    
    if (!is.null(naip_rgb)) {
      # Plot histogram of NDVI values
      plotRGB(naip_rgb, r=1,g=2,b=3,
              axes=TRUE,
              main ="False color composite")
      
      # Set TIFF file path
      tiff_file <- file.path(output_dir_path(), "naip_rgb.tiff")
      # Save NDVI as TIFF
      rast(naip_rgb) |> writeRaster(tiff_file, overwrite = TRUE)
      
    } else {
      print("No NAIP images found within the specified criteria.")
      return(NULL)  # Prevent further rendering
    }
  })
  
  color_palette <- colorRampPalette(c("red", "yellow", "brown","cyan"))
  output$GCIplot <- renderPlot({
    # Get data from reactive value
    naip_gci <- naip_gci_data()
    
    if (!is.null(naip_gci)) {
      # Plot GCI with x and y-axes
      plot(naip_gci,
           main = "GCI",
           axes = TRUE, box = FALSE,
           xlab = "Longitude", ylab = "Latitude",
           col=color_palette(10))
      # Add CSS styling to set the background color
      tags$head(
        tags$style(
          HTML("
          #GCIPlot {
            background-color: grey;
          }
        ")
        )
      )
      # Set TIFF file path
      tiff_file <- file.path(output_dir_path(), "naip_gci.tiff")
      # Save NDVI as TIFF
      rast(naip_gci) |> writeRaster(tiff_file, overwrite = TRUE)
      
    } else {
      print("No NAIP images found within the specified criteria.")
      return(NULL)  # Prevent further rendering
    }
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)

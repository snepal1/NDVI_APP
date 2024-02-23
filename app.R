library(shiny)
library(rstac)
library(terra)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(ggplot2)
library(rasterVis)
library(raster)
library(httr)
library(sp)
library(rgdal)
library(rgeos)

# Define UI
ui <- fluidPage(
  tags$style(type = "text/css", "
    body {
      background-color:#2E2E2E;
  
      color: white;
      margin: 10; /* Remove default body margin */
    }
    .vertical-gap {
      margin-bottom: 20px; /* Adjust the margin to control vertical spacing */
    }
    .center-plot {
      text-align: center;
    }
    
    #sidebar-content {
      width:33%; /* Set width of sidebar content to 100% of the sidebar */
      top:6%;
      left:10px;
      right:10px;
      position: fixed; /* Set sidebar content to a fixed position */
      height: 90%; /* Set height to 100% to fill the entire height of the viewport */
      overflow-y: auto; /* Enable vertical scrolling for the sidebar content */
      #background-color:#2E2E2E; /* Add a background color for better visibility */
      /* Add margin at the top of the sidebar content to lower it */
    }

    #mainPanel {
      margin-left:0%; /* Adjusted margin for spacing between the sidebar and main panel */
      width: 90%; /* Set width of the main panel to 65% of the page */
      height:100%;
      #overflow-y: auto; /* Enable vertical scrolling for the main panel */
    }
    
    #paragraph {
      margin-left:0%; /* Adjusted margin for spacing between the sidebar and main panel */
      width: 100%; /* Set width of the main panel to 65% of the page */
      height:100%;
      #overflow-y: auto; /* Enable vertical scrolling for the main panel */
    }
    
    .well {
      border: none; /* Remove the border for wellPanel */
  
    }
    
  "),
  
  titlePanel("Image Analysis with NAIP"),
  sidebarLayout(
    sidebarPanel (
      div(id = "sidebar-content",
          
          wellPanel(div(id = "mymap", leafletOutput("mymap"))),
          
          wellPanel(
            h4("Type of Vegetation Indices"),
            style="background:black",
            selectInput("analysis_select", "Choose Analysis", 
                        choices = c("NDVI","GNDVI","RGB", "GCI"),
                        selected = NULL)),
          
          wellPanel(verbatimTextOutput("drawnCoordinates"),
                    style="background:black"),
          
          wellPanel(h4("Bounding Box"),
                    p('The field below provides with the co-ordinates for maximum and minimum latitude and longitude of the selcted bounding box.'),
                    style="background:black",
                    numericInput("min_lat", "Minimum Latitude", 0),
                    tags$style("#min_lat { background-color: grey; color: white; }"),
                    numericInput("min_lon", "Minimum Longitude", 0),
                    tags$style("#min_lon { background-color: grey; color: white; }"),
                    numericInput("max_lat", "Maximum Latitude", 0),
                    tags$style("#max_lat { background-color: grey; color: white; }"),
                    numericInput("max_lon", "Maximum Longitude", 0)),
          tags$style("#max_lon { background-color: grey; color: white; }"),      
          
          wellPanel(
            h4("Information About this Application"),  style="background:black",
            p('This is a Shiny app that will allow users to draw a polygon over the leaflet map on the top left panel. Based on the
                   drawn coordinates, the app will fetch the NAIP-4 channel image (0.5 m resolution) 
                   from Microsoft Planetary Computer STAC API and perform the vegetation indices analysis.')
          )
      )),
    
    mainPanel( 
      div(id = "mainPanel",
          #h1('Image Analysis Outputs'),
          fluidRow(
            column(6,
                   h2("Indices Output"),
                   plotOutput("selectedPlot", height = 700)
            ),
            
            column(6,
                   h2("Histograms Output"),
                   plotOutput("histogramPlot", height = 700)
            ))),
      
      tags$div(class = "vertical-gap"),
      
      div(id = "paragraph",
          column(12,  # Full-width column for the well panel
                 wellPanel(
                   h4("Description of the Outputs"),
                   style="background:black",
                   p('The outputs that user can visualize on the main panle are ;1) vegetation indices (raster) and 2) the histograms of the associated 
                vegetation and non-vegetation pixels.')
                 )
          )),
      
      tags$div(class = "vertical-gap")
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  output_dir_path <- reactiveVal(NULL)
  webhook_url <- reactiveVal(NULL)
  parsed_extent <- reactiveVal(NULL)
  
  observe({
    queryParams <- parseQueryString(session$clientData$url_search)
    if (!is.null(queryParams$extent)) {
      extent_match <- regmatches(queryParams$extent, regexpr("\\[\\[.*\\]\\]", queryParams$extent))[[1]]
      extent <- jsonlite::fromJSON(gsub("[\\[\\]]", "", extent_match), simplifyMatrix = TRUE)
      extent_matrix <- matrix(extent, ncol = 2, byrow = TRUE)
      if (length(extent_matrix) == 10) {
        parsed_extent(extent_matrix)
        updateNumericInput(session, "min_lat", value = extent_matrix[4, 1])
        updateNumericInput(session, "min_lon", value = extent_matrix[1, 1])
        updateNumericInput(session, "max_lat", value = extent_matrix[5, 1])
        updateNumericInput(session, "max_lon", value = extent_matrix[2, 1])
      }
    }
    if (!is.null(queryParams$output_dir_path)) {
      output_dir_path(queryParams[["output_dir_path"]])
      webhook_url(queryParams[["webhook_url"]])
    }
  })
  
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  
  naip_ndvi_data <- reactiveVal(NULL)
  naip_gndvi_data <- reactiveVal(NULL)
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
          targetGroup = "draw",
          polylineOptions = NULL,
          polygonOptions = NULL,
          circleOptions = NULL,
          markerOptions = NULL,
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        ) %>%
        addLayersControl(overlayGroups = c("draw"), options = layersControlOptions(collapsed = FALSE))
    } else {
      map <- leaflet() %>%
        addTiles() %>%
        setView(lng = -120.75, lat = 39.72, zoom = 10) %>%
        addDrawToolbar(
          targetGroup = "draw",
          polylineOptions = NULL,
          polygonOptions = NULL,
          circleOptions = NULL,
          markerOptions = NULL,
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        ) %>%
        addLayersControl(overlayGroups = c("draw"), options = layersControlOptions(collapsed = FALSE))
      
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
    drawn_feature <- input$mymap_draw_new_feature
    if (!is.null(drawn_feature)) {
      bbox <- drawn_feature$geometry$coordinates
      bbox <- matrix(unlist(bbox[[1]]), ncol = 2, byrow = TRUE)
      min_lat <- min(bbox[, 2])
      min_lon <- min(bbox[, 1])
      max_lat <- max(bbox[, 2])
      max_lon <- max(bbox[, 1])
      
      updateNumericInput(session, "min_lat", value = min_lat)
      updateNumericInput(session, "min_lon", value = min_lon)
      updateNumericInput(session, "max_lat", value = max_lat)
      updateNumericInput(session, "max_lon", value = max_lon)
      
      cat("Coordinates:", toJSON(bbox), "\n")
      return(NULL)
    }
  })
  
  output$selectedPlot <- renderPlot({
    analysis <- input$analysis_select
    
    it_obj <- s_obj %>%
      stac_search(collections = "naip", bbox = c(input$min_lon, input$min_lat, input$max_lon, input$max_lat)) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())
    
    if (!is.null(it_obj$features) && length(it_obj$features) > 0) {
      url <- it_obj$features[[1]]$assets$rendered_preview$href
      #url <- it_obj$features[[1]]$assets$image$href
      #naip_data <- stack(url)
      #naip_brick <- brick(naip_data)
      naip_data_cr <- rast(url)
      
      naip_data <- brick(naip_data_cr)
      
      
      #bb <- ext(naip_data)
      #M <- matrix(c(bb[1], bb[3], bb[2], bb[4]), nrow = 2, ncol = 2)
      #ext.region <- extent(M)
      #naip_csf_br <- crop(naip_brick, ext.region)
      
      NDVI_Calc<-function(b1,b2) {
        ndvi<-(b2-b1)/(b2+b1)
        return(ndvi)
      }
      
      GNDVI_Calc<-function(b1,b2) {
        gndvi<-(b1-b2)/(b1+b2)
        return(gndvi)
      }
      
      GCI_Calc<-function(b1,b2) {
        gci<-(b2-b1)/sqrt((b2^2)+b1+0.5)
        return(gci)
      }
      
      if (analysis == "NDVI") {
        naip_ndvi <- overlay(naip_data[[1]],naip_data[[4]],
                             fun=NDVI_Calc)
        naip_ndvi_data(naip_ndvi)
        plot(naip_ndvi, main = "NDVI", axes = TRUE, box = FALSE, xlab = "Longitude", ylab = "Latitude")
        
        output_dir = output_dir_path()
        webhook = webhook_url()
        print(output_dir)
        
        
        if (!is.null(output_dir) && !is.null(webhook)) {
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }
          
          tiff_file <- file.path(output_dir, "naip_ndvi.tiff")
          # Save NDVI as TIFF
          rast(naip_ndvi) |> writeRaster(tiff_file, overwrite = TRUE)
          
          #response <- POST(webhook, body = list(message = "File processed", file_path = tiff_file))
          #print(response)
        }
        
        
      } else if (analysis == "GNDVI") {
        naip_gndvi <- 
          overlay(naip_data[[2]],naip_data[[4]],
                  fun=GNDVI_Calc)
        naip_gndvi_data(naip_gndvi)
        #(naip_csf_br[[2]] - naip_csf_br[[4]]) / (naip_csf_br[[2]] + naip_csf_br[[4]])
        plot(naip_gndvi, main = "GNDVI", axes = TRUE, box = FALSE, xlab = "Longitude", ylab = "Latitude")
        
        output_dir = output_dir_path()
        webhook = webhook_url()
        print(output_dir)
        
        if (!is.null(output_dir) && !is.null(webhook)) {
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }
          
          tiff_file <- file.path(output_dir, "naip_gndvi.tiff")
          # Save NDVI as TIFF
          rast(naip_gndvi) |> writeRaster(tiff_file, overwrite = TRUE)
          
          # response <- POST(webhook, body = list(message = "File processed", file_path = tiff_file))
          # print(response)
        }
        
      } else if (analysis == "RGB") {
        plotRGB(naip_data, r = 1, g = 2, b = 3, axes = TRUE, main = "False color composite")
        
        output_dir = output_dir_path()
        webhook = webhook_url()
        print(output_dir)
        
        if (!is.null(output_dir) && !is.null(webhook)) {
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }
          
          tiff_file <- file.path(output_dir, "naip_rgb.tiff")
          # Save NDVI as TIFF
          rast(naip_rgb) |> writeRaster(tiff_file, overwrite = TRUE)
          
          # response <- POST(webhook, body = list(message = "File processed", file_path = tiff_file))
          # print(response)
        }
      } else if (analysis == "GCI") {
        naip_gci <- overlay(naip_data[[1]],naip_data[[4]],
                            fun=GCI_Calc)
        naip_gci_data(naip_gci)
        #(naip_csf_br[[4]] - naip_csf_br[[1]]) / sqrt(naip_csf_br[[4]]^2 + naip_csf_br[[1]] + 0.5)
        plot(naip_gci, main = "GCI", axes = TRUE, box = FALSE, xlab = "Longitude", ylab = "Latitude")
        output_dir = output_dir_path()
        webhook = webhook_url()
        print(output_dir)
        
        if (!is.null(output_dir) && !is.null(webhook)) {
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }
          
          tiff_file <- file.path(output_dir, "naip_gci.tiff")
          # Save NDVI as TIFF
          rast(naip_gci) |> writeRaster(tiff_file, overwrite = TRUE)
          
          response <- POST(webhook, body = list(message = "File processed"))
          print(response)
        }
        
      }
    } else {
      print("No NAIP images found within the specified criteria.")
      return(NULL)
    }
  })
  
  output$histogramPlot <- renderPlot({
    analysis <- input$analysis_select
    naip_ndvi <- naip_ndvi_data()
    naip_gndvi <- naip_gndvi_data()
    naip_gci <- naip_gci_data()
    
    if (!is.null(naip_ndvi)) {
      if (analysis == "NDVI") {
        hist(naip_ndvi, main = "NDVI: Distribution of pixels", col = "springgreen", xlab = "NDVI Index Value")
        abline(v = 0.5, col = "red", lwd = 2)
        text(0.45, 1000, "Non-Trees", pos = 2, col = "black", font = 2)
        text(0.55, 1000, "Trees", pos = 4, col = "black", font = 2)
      } else if (analysis == "GNDVI") {
        hist(naip_gndvi, main = "GNDVI: Distribution of pixels", col = "springgreen", xlab = "GNDVI Index Value")
        # Add a vertical red line at cutoff value
        abline(v = -0.55, col = "red", lwd = 2)
        abline(v = -0.4, col = "red", lwd = 2)
        # Add text labels
        text(-0.8, 2000, "Vegetation", pos = 2, col = "black", font = 2)
        text(-0.5, 2000, "Water", pos = 4, col = "black", font = 2)
        text(-0.3, 2000, "Soil", pos = 4, col = "black", font = 2)
      } else if (analysis == "RGB") {
        plotRGB(naip_data, r = 1, g = 2, b = 3, axes = TRUE, main = "False color composite")
        return(NULL)  # No histogram for RGB
      } else if (analysis == "GCI") {
        hist(naip_gci, main = "GCI: Distribution of pixels", col = "springgreen", xlab = "GCI Index Value")
        abline(v = 0, col = "red", lwd = 2)  # Add threshold line for GCI
        text(-0.1, 1000, "Non-Trees", pos = 2, col = "black", font = 2)
        text(0.1, 1000, "Trees", pos = 4, col = "black", font = 2)
      } else {
        hist(naip_ndvi, main = "Distribution of pixels", col = "springgreen", xlab = "Index Value")
      }
    } else {
      print("No", analysis, "data available.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
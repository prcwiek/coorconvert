# Simple coordinates converter for the area of Poland
# GNU General Public License v3.0

library(dplyr)
library(leaflet)
library(readr)
library(sf)
library(shiny)
library(shinyjs)
library(stringr)

coord_systems <- c("ETRS89 / Poland CS92 EPSG: 2180" = "2180", 
                   "ETRS89 / Poland CS2000 zone 5 EPSG: 2176" = "2176",
                   "ETRS89 / Poland CS2000 zone 6 EPSG: 2177" = "2177",
                   "ETRS89 / Poland CS2000 zone 7 EPSG: 2178" = "2178",
                   "ETRS89 / Poland CS2000 zone 8 EPSG: 2179" = "2179",
                   "Pulkovo 1942(58) / Poland 1965 zone I EPSG: 3120" = "3120",
                   "Pulkovo 1942(58) / Poland 1965 zone II EPSG: 2172" = "2172",
                   "Pulkovo 1942(58) / Poland 1965 zone III EPSG: 2173" = "2173",
                   "Pulkovo 1942(58) / Poland 1965 zone IV EPSG: 2174" = "2174",
                   "Pulkovo 1942(58) / Poland zone V EPSG: 2175" = "2175",
                   "WGS 84 / UTM zone 33N EPSG: 32633" = "32633",
                   "WGS 84 / UTM zone 34N EPSG: 32634" = "32634",
                   "WGS84 - World Geodetic System 1984 EPSG: 4326 Format: d" = "4326",
                   "WGS84 - World Geodetic System 1984 EPSG: 4326 Format: dms" = "4326")

convert_from_dms <- function(din) {
    dout_b <- c(1:nrow(din))
    dout_b <- str_detect(din$lon, pattern = "^\\d{6}.")
    din$lon[dout_b] <- as.numeric(str_sub(din$lon[dout_b], 1, 2)) + as.numeric(str_sub(din$lon[dout_b], 3, 4)) / 60 + as.numeric(str_sub(din$lon[dout_b], 5, -1)) / 3600
    din$lon[!dout_b] <- as.numeric(str_sub(din$lon[!dout_b], 1, 2)) + as.numeric(str_sub(din$lon[!dout_b], 3, 4)) / 60 + as.numeric(str_sub(din$lon[!dout_b], 5, -1)) / 3600
    din$lat = as.numeric(str_sub(din$lat, 1, 2)) + as.numeric(str_sub(din$lat, 3, 4)) / 60 + as.numeric(str_sub(din$lat, 5, -1)) / 3600
    din
}

ui <- tagList(
    
    shinyjs::useShinyjs(),
    
    navbarPage(theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
               title = "Coordinates Converter",
               tabPanel("Coordinates", sidebarLayout(
                            sidebarPanel(width = 3,
                                         fileInput("filec_in", label = h5("Choose csv file with input coordinates"),
                                                   accept = c("text/csv", "text/comma-separated-values",
                                                              "text/tab-separated-values", ".csv", ".tsv")),
                                         selectInput("input_coord", label = h5("Select input coordinates system"), 
                                                     choices = list("ETRS89 / Poland CS92 EPSG: 2180" = 2180, 
                                                                    "ETRS89 / Poland CS2000 zone 5 EPSG: 2176" = 2176,
                                                                    "ETRS89 / Poland CS2000 zone 6 EPSG: 2177" = 2177,
                                                                    "ETRS89 / Poland CS2000 zone 7 EPSG: 2178" = 2178,
                                                                    "ETRS89 / Poland CS2000 zone 8 EPSG: 2179" = 2179,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone I EPSG: 3120" = 3120,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone II EPSG: 2172" = 2172,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone III EPSG: 2173" = 2173,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone IV EPSG: 2174" = 2174,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone V EPSG: 2175" = 2175,
                                                                    "WGS 84 / UTM zone 33N EPSG: 32633" = 32633,
                                                                    "WGS 84 / UTM zone 34N EPSG: 32634" = 32634,
                                                                    "WGS84 - World Geodetic System 1984 EPSG: 4326 Format: d only" = "4326_d",
                                                                    "WGS84 - World Geodetic System 1984 EPSG: 4326 Format: dms" = "4326_dms",
                                                                    "Other" = "other"),
                                                     selected = 2180),
                                         numericInput("epsg_input", label = h5("or enter other EPSG code:"), value = 32633),
                                         actionButton("switch_cols", "Switch lat and lot columns in input"),
                                         selectInput("output_coord", label = h5("Select output coordinates system"), 
                                                     choices = list("ETRS89 / Poland CS92 EPSG: 2180" = 2180, 
                                                                    "ETRS89 / Poland CS2000 zone 5 EPSG: 2176" = 2176,
                                                                    "ETRS89 / Poland CS2000 zone 6 EPSG: 2177" = 2177,
                                                                    "ETRS89 / Poland CS2000 zone 7 EPSG: 2178" = 2178,
                                                                    "ETRS89 / Poland CS2000 zone 8 EPSG: 2179" = 2179,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone I EPSG: 3120" = 3120,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone II EPSG: 2172" = 2172,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone III EPSG: 2173" = 2173,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone IV EPSG: 2174" = 2174,
                                                                    "Pulkovo 1942(58) / Poland 1965 zone V EPSG: 2175" = 2175,
                                                                    "WGS 84 / UTM zone 33N EPSG: 32633" = 32633,
                                                                    "WGS 84 / UTM zone 34N EPSG: 32634" = 32634,
                                                                    "WGS84 - World Geodetic System 1984 EPSG: 4326" = 4326,
                                                                    "Other" = "other"),
                                                     selected = 32633),
                                         numericInput("epsg_output", label = h5("or enter other EPSG code:"), value = 32634),
                                         p(),br(),
                                         downloadButton("downloadconverted", "Download converted coordinates")
                                         
                            ),
                            mainPanel(width = 8,
                                      fluidRow(
                                          column(3, "Input coordinates", tableOutput("coord_in")),
                                          column(2),
                                          column(3, "Transformed coordinates", tableOutput(("coord_out")))
                                      ))
                )),
               tabPanel("Map", sidebarLayout(
                   sidebarPanel(width = 3,
                                radioButtons("radio_in_out", label = h5("Select coordinates to use"),
                                             choices = list("Input coordinates" = 1, "Transformed coordinates" = 2), 
                                             selected = 1),
                                radioButtons("icon_out", label = h5("Select icon to use in a kml file"),
                                             choiceNames = list(HTML('<img src="http://maps.google.com/mapfiles/ms/micons/pink-dot.png" width="20" height="20">'),
                                                                HTML('<img src="http://maps.google.com/mapfiles/ms/micons/ylw-pushpin.png" width="20" height="20">'),
                                                                HTML('<img src="http://maps.google.com/mapfiles/kml/pal4/icon25.png" width="20" height="20">')),
                                             choiceValues = list("pink_dot",
                                                                 "ylw_pushpin",
                                                                 "icon_25")),
                                p(),br(),
                                downloadButton("downloadkml", "Download kml file")),
                   mainPanel(width = 8, 
                             tags$style(type = "text/css", "#coordMap {height: calc(100vh - 80px) !important;}"),
                             leafletOutput("coordMap"))
               )),
               tabPanel("About", htmlOutput("about"))
    )
)

server <- function(input, output, session) {
    
    # Input coordinates
    dc_in <- reactiveValues(data = NULL)
    
    # Output coordinates, after transformation
    dc_out <- reactiveValues(data = NULL)


    # load coordinates --------------------------------------------------------
    observeEvent(input$filec_in,{
        
        if(is.null(input$filec_in))
            return(NULL)
        
        if(str_detect(input$filec_in$name, ".csv$")){
            dc_import <- read_csv(input$filec_in$datapath, show_col_types = FALSE)    
        }
        
        if(str_detect(input$filec_in$name, ".tsv$")){
            dc_import <- read_tsv(input$filec_in$datapath, show_col_types = FALSE)    
        }

        if (ncol(dc_import) > 3) {
            showModal(modalDialog(title = "File upload error",
                                  "File has more than 3 columns",
                                  easyClose = TRUE, footer = NULL))
           return(NULL)           
        }

        if (ncol(dc_import) == 3) {
            if (sum(str_detect(names(dc_import), pattern = ("(label|lat|lon)"))) != 3) {
                names(dc_import) <- c("label", "lon", "lat")
            }
        } else if (ncol(dc_import) == 2) {
            names(dc_import) <- c("lon", "lat")
            dc_import <- dc_import %>% 
                mutate(label = seq(1:nrow(dc_import))) %>% 
                select(label, lon, lat)
        }
        
        dc_in$data <- dc_import
        
        shinyjs::enable("downloadconverted")
        shinyjs::enable("switch_cols")
        
    })

    # show table with input coordinates ---------------------------------------
    output$coord_in <- renderTable(digits = 4, {
        dc_in$data        
    })

    # show table with transformed coordinates ---------------------------------
    output$coord_out <- renderTable(digits = 4,{
        
        if(is.null(dc_in$data))
            return(NULL)
        
        # input coordinates system
        if (input$input_coord == "other") {
            epsg_in <- input$epsg_input
        } else if(input$input_coord == "4326_d" | input$input_coord == "4326_dms") {
            epsg_in <- 4326
        } else {
            epsg_in <- as.numeric(input$input_coord)    
        }
        
        # output coordinates system
        if (input$output_coord == "other") {
            epsg_out <- input$epsg_output
        } else {
            epsg_out <- as.numeric(input$output_coord)            
        }
        
        # convert coordinates from format dms
        if(input$input_coord == "4326_dms") {
            dout <- st_as_sf(convert_from_dms(dc_in$data), coords = c("lon", "lat"), crs = epsg_in)
        } else {
            dout <- st_as_sf(dc_in$data, coords = c("lon", "lat"), crs = epsg_in)    
        }
        
        dout <- st_transform(dout, crs = epsg_out)
        
        n_rounding <- ifelse(input$input_coord == "4326_d" | input$input_coord == "4326_dms", 8, 4)
        
        dout <- as_tibble(round(st_coordinates(dout), n_rounding)) %>% 
            mutate(label = dout$label) %>% 
            select(label, X, Y)
        
        names(dout) <- c("label", "lon", "lat")
        
        dc_out$data <- dout
        
        dc_out$data
        
    })

    # show map ----------------------------------------------------------------
    output$coordMap <- renderLeaflet({
        if(is.null(dc_in$data))
            return(NULL)
        
        if(input$radio_in_out == 1) {
            # input coordinates system
            if(input$input_coord == "4326_d" | input$input_coord == "4326_dms") {
                epsg_in <- 4326
            } else {
                if (input$input_coord == "other") {
                    epsg_in <- input$epsg_input
                } else {
                    epsg_in <- as.numeric(input$input_coord)    
                }
            }
            if(input$input_coord == "4326_dms"){
                dplot <- st_as_sf(convert_from_dms(dc_in$data), coords = c("lon", "lat"), crs = epsg_in)
            } else {
                dplot <- st_as_sf(dc_in$data, coords = c("lon", "lat"), crs = epsg_in)    
            }
        } else {
            # output coordinates system
            if (input$output_coord == "other") {
                epsg_out <- input$epsg_output
            } else {
                epsg_out <- as.numeric(input$output_coord)            
            }
            dplot <- st_as_sf(dc_out$data, coords = c("lon", "lat"), crs = epsg_out)
        }
        
        dplot <- st_transform(dplot, crs = 4326)
        
        lon_center <- mean(st_coordinates(dplot)[,1])
        lat_center <- mean(st_coordinates(dplot)[,2])
        
        lon_dist <- abs(max(st_coordinates(dplot)[,1]) - min(st_coordinates(dplot)[,1]))
        lat_dist <- abs(max(st_coordinates(dplot)[,2]) - min(st_coordinates(dplot)[,2]))
        
        if (lon_dist == 0 | lat_dist == 0) {
            z_scale <- 8
        } else if (lon_dist > 1.0 | lat_dist > 1.0) {
            if (lon_dist > lat_dist) z_scale <- round(lon_dist * 7, 0) else z_scale <- round(lat_dist * 7, 0)
        } else if (lon_dist > 0.5 | lat_dist > 0.5) {
            if (lon_dist > lat_dist) z_scale <- round(lon_dist * 14, 0) else z_scale <- round(lat_dist * 14, 0)
            #if (1/lon_dist > 1/lat_dist) z_scale <- round(1 / lon_dist / 6, 0) else z_scale <- round(1 / lat_dist / 6, 0)
        } else if (lon_dist > 0.25 | lat_dist > 0.25) {
            if (lon_dist > lat_dist) z_scale <- round(lon_dist * 21, 0) else z_scale <- round(lat_dist * 21, 0)
            #if (1/lon_dist > 1/lat_dist) z_scale <- round(1 / lon_dist / 6, 0) else z_scale <- round(1 / lat_dist / 6, 0)
        } else if (lon_dist < 0.1 | lat_dist < 0.1) {
            z_scale <- 14
        } else {
            z_scale <- 6
        }
        
        leaflet() %>% 
            setView(lng = lon_center, lat = lat_center, zoom = z_scale) %>% 
            addCircles(data = dplot) %>% 
            addLabelOnlyMarkers(
                label = ~lapply(label, htmltools::htmlEscape),
                labelOptions = labelOptions(noHide = TRUE, direction = "top"),
                data = dplot
            ) %>% 
            addProviderTiles(providers$OpenStreetMap)  
    
    })

    # Download converted coordinates
    output$downloadconverted <- downloadHandler(
        filename = function() {
            fname <- names(coord_systems)[coord_systems == input$output_coord]
            fname <- gsub(" ", "_", fname)
            fname <- gsub("/", "", fname)
            fname <- gsub("__", "_", fname)
            paste0(gsub(".[a-zA-Z]+$", "", input$filec_in$name), "_", fname, ".csv")
        },
        content = function(file) {
            write_csv(dc_out$data, file)
        }
    )

    # Download kml files ------------------------------------------------------
    output$downloadkml <- downloadHandler(
        
        filename = function() {
            fname <- names(coord_systems)[coord_systems == input$output_coord]
            fname <- gsub(" ", "_", fname)
            fname <- gsub("/", "", fname)
            fname <- gsub("__", "_", fname)
            paste0(gsub(".[a-zA-Z]+$", "", input$filec_in$name), "_", fname, ".kml")
        },
        
        content = function(file) {
            if(is.null(dc_in$data))
                return(NULL)
            
            if(input$radio_in_out == 1) {
                # input coordinates system
                if(input$input_coord == "4326_d" | input$input_coord == "4326_dms") {
                    epsg_in <- 4326
                } else (
                    if (input$input_coord == "other") {
                        epsg_in <- input$epsg_input
                    } else {
                        epsg_in <- as.numeric(input$input_coord)    
                    }
                )
                if(input$input_coord == "4326_dms"){
                    dplot <- st_as_sf(convert_from_dms(dc_in$data), coords = c("lon", "lat"), crs = epsg_in)
                } else {
                    dplot <- st_as_sf(dc_in$data, coords = c("lon", "lat"), crs = epsg_in)    
                }
                kmldescription <- "Created from input coordinates"
            } else {
                # output coordinates system
                if (input$output_coord == "other") {
                    epsg_out <- input$epsg_output
                } else {
                    epsg_out <- as.numeric(input$output_coord)            
                }
                dplot <- st_as_sf(dc_out$data, coords = c("lon", "lat"), crs = epsg_out)
                kmldescription <- "Created from converted coordinates"
            }
            
            Points <- sf::as_Spatial(st_transform(dplot, crs = 4326))
            points_sf <- sf::st_as_sf(Points)
            
            # select icon
            if (input$icon_out == "pink_dot") {
              icon <- "http://maps.google.com/mapfiles/ms/micons/pink-dot.png"
            } else if (input$icon_out == "ylw_pushpin") {
              icon <- "http://maps.google.com/mapfiles/ms/micons/ylw-pushpin.png"
            } else {
              icon <- "http://maps.google.com/mapfiles/kml/pal4/icon25.png"
            }

            kmlname <- paste0(gsub(".[a-zA-Z]+$", "", input$filec_in$name), ".kml")
            kmlname <- gsub("-", "_", kmlname)
            
            st_write(obj = points_sf, dsn = kmlname, driver = "kml", append = FALSE)
            
            # add icon 
            # read kml as a text file
            tx <- readChar(kmlname, file.info(kmlname)$size)
            # check if <SimpleField> exists
            if (str_detect(tx, pattern = "</SimpleField>\n")) {
              # create icon_point style
              tx <- str_replace(tx,
                                pattern = "<Folder>",
                                replacement = paste0('\n<Style id="icon_point">\n\t\t<IconStyle>\n\t\t\t<scale>1.2</scale>\n\t\t\t<Icon>',
                                                    '<href>',icon,'</href>\n',
                                                     '\t\t\t</Icon>\n\t\t\t<hotSpot x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>',
                                                     '\n\t\t</IconStyle>\n\t\t<ListStyle></ListStyle>\n</Style>\n\n',
                                                     '<Folder>'))
              # add icon_point style to placemarks
              tx <- str_replace_all(tx,
                                pattern = "<Placemark>\n", 
                                replacement = paste0("<Placemark>\n",
                                                     '\t<styleUrl>#icon_point</styleUrl>\n'))
              # check if name exists
              if (str_detect(tx, pattern = "<Schema name=")) {
                tname <- str_sub(str_extract(str_extract(b, pattern = "^<Schema name[:graph:]+\\s"), pattern = "\"\\w+"),
                                 start = 2)
                tdocument <- str_extract(tx, "<Document\\s[:graph:]+\n") 
                write(tx, "test_doc.kml")
                tx <- str_replace(tx,
                                  pattern = tdocument,
                                  replacement = paste0(tdocument,
                                                       "\t<name>", tname, "</name>\n"))

              }
              
              # save modified kml file
              write(tx, kmlname)
            }
        }
    )
    
    observeEvent(input$switch_cols, {
        dc_in$data <- dc_in$data %>% 
            mutate(lon2 = lat,
                   lat2 = lon) %>% 
            select(label, lon2, lat2)
        names(dc_in$data) <- c("label", "lon", "lat")
    })
    
    output$about <- renderText({
        paste0('<h3>Simple coordinates converter for the area of Poland</h3>
        Applications allows to convert coordinates between different systems for Poland."<br>
        <p>Only csv and tsv input files are accepted with the columns names in the first row:<br>
        <ul>
        <li>label</li>
        <li>lon</li>
        <li>lat</li>
        </ul>
        where lon is longitude and lat is latitude.<br>
        Download is possible in .csv or .tsv format.
        <h4>Acceptable formats for geographical coordinates</h4>
        <ul>
        <li>Format d: 16.4241, 54.4472 </li>
        <li>Format dms: 162526.9, 542649.9</li>
        </ul>
        '
        )
    })
    

    # change select value -----------------------------------------------------
    observe({
      
      if (input$input_coord != "other") {
        if (input$input_coord == "4326_d" | input$input_coord == "4326_dms") {
          updateNumericInput(session, "epsg_input", value = 4326)  
        } else {
          updateNumericInput(session, "epsg_input", value = input$input_coord)
        }
        shinyjs::disable("epsg_input")
      } else {
        shinyjs::enable("epsg_input")
      }
      
      if (input$output_coord != "other") {
        updateNumericInput(session, "epsg_output", value = input$output_coord)
        shinyjs::disable("epsg_output")
      } else {
        shinyjs::enable("epsg_output")
      }
      
      
    })
    
    # Disable buttons at the start
    shinyjs::disable("downloadconverted")
    shinyjs::disable("switch_cols")

    # End application when a window or a tab is closed
    session$onSessionEnded(stopApp)
    
}

shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(sp)
library(spdep)
library(geojsonio)

# Load data
df1 <- readxl::read_excel("Georgia Parliament 2024/Data from recognising project/Georgia-2024-parliament 26.10.2024 3100 preliminary data checked by Ivan Shukshin.xlsx")
geodata <- read_csv('Georgia Parliament 2024/Data from the CEC/2024_parliamentary_round_1_proportional_electronic.csv')
geo_data <- geojson_read("ge.json", what = "sp")

# Data processing
df1 <- df1 %>%
    mutate(
        district = as.numeric(substr(number, 4, 5)),
        precinct_number = as.numeric(substr(number, 7, 8))
    ) %>%
    filter(district != "87") %>%
    mutate(district_urban = ifelse(district %in% c(1:10, 20, 59, 64, 67:70, 79), 1, 0)) %>%
    filter(is.na(dirty), registered + added != 0)

# Check for empty data and avoid division by zero
if (any(is.na(df1$valid) | df1$valid == 0)) {
    stop("Invalid data detected. 'valid' values must be non-zero and non-NA.")
}

df1$total_voters <- df1$added + df1$registered
df1$turnout <- df1$valid / df1$total_voters
df1$gd_share <- df1$`Georgian Dream` / df1$valid

geodata <- geodata %>% select(lat, lng, division_id) %>%
    rename(number = division_id)

# Merge geodata with df1
df1 <- merge(df1, geodata)

# Shiny UI
ui <- fluidPage(
    titlePanel("Georgia 2024 Parliamentary Elections"),
    
    leafletOutput("map", height = 600),  # Use leafletOutput for the map
    
    # Absolute panel for sliders
    absolutePanel(
        top = 10, right = 10, # Adjust position as needed
        sliderInput("radius", "Radius (km):", min = 20, max = 100, value = 60),
        numericInput("threshold", "Threshold:", value = 0.15, min = 0, step = 0.01),
        actionButton("update", "Flag Anomalies!"),
        style = "background: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.5);"
    )
)

# Shiny Server
server <- function(input, output, session) {
    
    # Initialize the map with default values and add the legend once
    output$map <- renderLeaflet({
        # Ensure gd_share has valid values before creating the color palette
        if (all(is.na(df1$gd_share))) {
            stop("No valid 'gd_share' data to initialize the map.")
        }
        
        color_palette <- colorNumeric(palette = "RdYlGn", domain = df1$gd_share, na.color = "transparent")
        
        leaflet(df1) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(data = geo_data, color = "black", fillColor = "white",
                        fillOpacity = 0.5, weight = 0.5) %>%
            setView(lng = mean(df1$lng, na.rm = TRUE), lat = mean(df1$lat, na.rm = TRUE), zoom = 7) %>%
            addCircleMarkers(
                lng = ~lng, lat = ~lat,
                color = ~color_palette(gd_share),
                fillOpacity = 1,
                radius = 5,
                stroke = FALSE,
                popup = ~paste("GD Share:", round(gd_share, 2),
                               "// Turnout:", round(turnout, 2))
            ) %>%
            addLegend("bottomright", pal = color_palette, values = ~gd_share,
                      title = "GD Share", opacity = 1)  # Add legend only once
    })
    
    observeEvent(input$update, {
        # Get user input
        radius_km <- input$radius
        threshold <- input$threshold
        
        # Process the data
        coords <- as.matrix(df1[, c("lng", "lat")])
        
        km <- function(x) {
            out <- x * 0.01  # Convert meters to kilometers
            return(out)
        }
        
        nb <- dnearneigh(coords, 0, km(radius_km))
        listw <- nb2listw(nb, style = "W")
        
        # Adding tryCatch to handle potential errors
        tryCatch({
            df1 <- df1 %>%
                mutate(local_mean_gd_share = lag.listw(listw, gd_share, zero.policy = TRUE)) %>%
                mutate(spike_flag = ifelse(gd_share - local_mean_gd_share > threshold, 1, 0))
            
            # Check for NA values before plotting
            if (any(is.na(df1$gd_share))) {
                stop("gd_share contains NA values.")
            }
            
            color_palette <- colorNumeric(palette = "RdYlGn", domain = df1$gd_share, na.color = "transparent")
            
            # Update the map with new data
            leafletProxy("map", data = df1) %>%
                clearMarkers() %>%
                addCircleMarkers(
                    lng = ~lng, lat = ~lat,
                    color = ~ifelse(spike_flag == 1, "black", color_palette(gd_share)),
                    fillOpacity = 1,
                    radius = 5,
                    stroke = FALSE,
                    popup = ~paste("GD Share:", round(gd_share, 2),
                                   "// Local Average:", round(local_mean_gd_share, 2),
                                   "Turnout:", round(turnout, 2))
                )
        }, error = function(e) {
            message("Error in updating map: ", e)
        })
    })
    session$onSessionEnded(function() { stopApp() })
}

# Run the app
shinyApp(ui, server)
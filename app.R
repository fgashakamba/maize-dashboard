# This script maps maize distribution data for seasons 2023A and 2023B.
# The data is downloaded from a Google spreadsheet geometry data is added from Rwanda geodatabase.
# The map is imported into a shiny app to make it interactive.
# Author: Faustin GASHAKAMBA
# Date: October 2023
# Updated: June 2025 (tmap v4 compatibility)
#################################################################################
if(!"remotes" %in% installed.packages()){
  install.packages("remotes")
}
# list my required packages
required_pkgs = c("magrittr", "dplyr", "stringr", "tidyr", "googlesheets4", 
                  "shiny", "bslib", "shinycssloaders", "leaflet", "tmap", 
                  "plotly", "sf", "jsonlite", "openssl", "janitor")

# Get the missing required packages and install them
missing_pkgs <- setdiff(required_pkgs, installed.packages()[, "Package"])
if(length(missing_pkgs) != 0){
  remotes::install_cran(missing_pkgs)
}
# load the libraries of my required packages
lapply(required_pkgs, require, character.only = T)

# Authenticate google sheets with a service account
#---------------------------------------------------
# The JSON service account file has been encoded to base64 and stored in the 
# environment variable called "GSHEET_SERVICE_JSON_BASE64" like this:
#cat(openssl::base64_encode(readChar("shiny-gsheets-service-account-file.json", file.info(json_path)$size)))
b64 <- Sys.getenv("GSHEET_SERVICE_JSON_BASE64") # Read the encoded service account json file
decoded_raw <- base64_decode(b64) # Decode the JSON string
tmp <- tempfile(fileext = ".json") # Create a temporary file
writeBin(decoded_raw, tmp) # Write decoded text to the created temporary file as binary
gs4_auth(path = tmp)# Authenticate with the service account

# Download maize aggregation data from the google spreadsheet
#url <- "https://docs.google.com/spreadsheets/d/1JK6Y6romZympCv46Iyyy6XpKHxmEWNV6MW0x4ylM5DM"
# I don't own this spreadsheet and therefore I can't share it with my service account
# So, I made a copy to the address below
url = "https://docs.google.com/spreadsheets/d/18ZV3z6mIMtpTzb7hzeIx5HTZRVfH3g7McKg5NFvsG-4"
data <- range_read(ss = url, sheet = "Aggr - Per Truck data", col_names = T, range = "A4:N",
                   col_types = paste(c(rep("c", 9), rep("n", 2), rep("c", 2), rep("n", 1)), collapse = ""))

# clean the column names and remove all_NA columns and rows
data %<>% clean_names(.) %>% select(where(~!all(is.na(.)))) 
data %<>% filter(!if_any(c(sector, district, number_of_farmers, volume_1_in_kgs), ~is.na(.)))
data %<>% rename(total_volume = volume_1_in_kgs)

# Sanitize district & sector names
data %<>% mutate(across(c(district, sector), ~str_to_title(str_remove_all(str_squish(.),"[^[:alnum:][:space:]]"))))
data %<>% mutate(across(c(district, sector), ~ifelse(!str_detect(., "(?<!^)ri"), ., str_replace_all(., "(?<!^)ri", "li"))))

# manual correction of typos in the names of districts & sectors
data %<>% mutate(district = if_else(sector == "Mimuli", "Nyagatare", district))

sectors <- st_read(dsn = "data/sectors.gpkg")
sectors %<>% clean_names(.) 

sectors %<>% mutate(across(where(is.character), ~str_to_title(str_remove_all(str_squish(.),"[^[:alnum:][:space:]]"))))
sectors %<>% mutate(across(where(is.character), ~ifelse(!str_detect(., "(?<!^)ri"), ., str_replace_all(., "(?<!^)ri", "li"))))

# # load other basemap layers
lakes <- st_read(dsn = "data/lakes.gpkg")
np <- st_read(dsn = "data/national_parks.gpkg")
country <- st_read(dsn = "data/country.gpkg")


# fix invalid geometries (and reproject to UTM 36S)
lakes %<>% st_transform(crs = 32736) %>% st_zm(drop = T, what = "ZM") %>% st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)
np %<>% st_transform(crs = 32736) %>% st_zm(drop = T, what = "ZM") %>% st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)
country %<>% st_transform(crs = 32736) %>% st_zm(drop = T, what = "ZM") %>% st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)
sectors %<>% st_transform(crs = 32736) %>% st_zm(drop = T, what = "ZM") %>% st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)

# Join the boundaries to the data
data %<>% group_by(season, district, sector) %>% summarize(number_of_farmers = sum(number_of_farmers), total_volume = sum(total_volume), .groups = "drop") %>% 
  left_join(sectors, by = c("district", "sector")) %>% st_as_sf()

# rename the display columns and format numbers for human readability
data %<>% mutate(`# Farmers` = format(number_of_farmers, big.mark = ","),  `Volume (Tonnes)` = format(round(total_volume/1000, 2), big.mark = ","))

# CREATE AN INTERACTIVE MAP
#=====================================================
# set map options
#tmap_mode("view") # Set the view mode to "interactive"
#tmap_mode("plot") # Set the view mode to "interactive"

# Use shiny to visualize the map interactively by accepting user input
#======================================================================
# Create the UI using fluidPage and card
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #sector_performanceCard{
        background-color: white; /* Set white background */
        border: 2px solid #63A0AE; /* Add border with desired color */
        height: 30vh; /* Set height */
      }
      #sector_performanceCard .card-body {
        padding: 0; /* Remove inner padding */
      }
      #distr_totalCard{
        background-color: white; 
        border: 2px solid #63A0AE; 
        display: flex; /* Use flexbox */
        justify-content: center; /* Center horizontally */
        height: 30vh; 
      }
      #distr_totalCard .card-body {
        position: absolute; /* Set position to absolute */
        top: 50%; /* Set top to 50% */
        left: 50%; /* Set left to 50% */
        transform: translate(-50%, -50%); /* Translate back by 50% of its own dimensions */
        text-align: center; /* Center text */
        width: 100%; /* Set width to 100% */
      }
    "))
  ),
  theme = bs_theme(version = 5, bootswatch = "lumen"),
  titlePanel("Maize aggregation map, Year 2023"),
  fluidRow(
    column(
      width = 6,
      card(
        style = "height: 30vh;",
        full_screen = FALSE,
        card_header("Select map parameters"),
        card_body(
          radioButtons("my_season", 
                       label = "Choose season", 
                       choices = list("2023A", "2023B"),
                       selected = "2023A",
                       inline = TRUE),
          radioButtons("statistic",
                       label = "Choose the statistic to visualize", 
                       choices = list("Number of farmers"="number_of_farmers",
                                      "Quantity of aggregated maize"="total_volume"),
                       inline = FALSE)
        )
      ),
      fluidRow(
        column(
          width = 6,
          card(
            id = "distr_totalCard",
            title = NULL,
            full_screen = FALSE,
            card_body(
              withSpinner(htmlOutput("distr_total"), 
                          type = 5, hide.ui = FALSE,
                          color = "#30804e")
            )
          )
        ),
        column(
          width = 6,
          card(
            id = "sector_performanceCard",
            title = NULL,
            full_screen = FALSE,
            card_body(
              withSpinner(plotlyOutput("sect_perform"), 
                          type = 5, hide.ui = FALSE,
                          color = "#30804e")
            )
          )
        )
      ),
      card(
        id = "detailsCard",
        style = "height: 35vh;",
        full_screen = FALSE,
        card_header("Details of other sectors"),
        card_body(
          withSpinner(tableOutput("details"), 
                      type = 5, hide.ui = FALSE,
                      size = .5,  color = "#30804e")
        )
      )
    ),
    column(
      width = 6,
      card(
        id = "mapCard",
        style = "height: 100vh;",
        full_screen = TRUE,
        card_header("Map"),
        card_body(
          leafletOutput("map")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  my_season <- reactive(input$my_season)
  statistic <- reactive(input$statistic)
  # filter the data according to the user-selected season
  my_data <- reactive({
    # filter the data according to the user-selected season
    data_filtered <- data %>% filter(season == my_season()) 
    return(data_filtered)
  })
  
  # Display the map
  output$map <- renderLeaflet({
    # Define a color-blind friendly palette
    cud_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
    
    # create the tmap object using v4 syntax
    tmap_object <- tm_shape(sectors) +
      tm_borders(col = "#A76948", fill_alpha = .6) +
      tm_shape(my_data()) +
      tm_borders() +
      tm_fill(
        fill = statistic(),
        fill.scale = tm_scale_intervals(
          style = "jenks",
          values = cud_palette
        ),
        fill.legend = tm_legend(
          title = str_to_sentence(str_replace_all(statistic(), "_", " "))
        ),
        id = "sector", # to specify which column to use for tooltips
        popup.vars = c("Sector:"="sector", "Value:"=statistic()),
        fill_alpha = .6
      ) +
      tm_shape(lakes) +
      tm_polygons(col = "#2CA2E6", fill_alpha = .6, popup.vars = c("Lake:"="name")) +
      tm_shape(np) + 
      tm_polygons(col = "#158849", fill_alpha = .6) +
      tm_shape(country) + 
      tm_borders(lwd = 2) +
      tm_view(bbox = st_bbox(country))  # Set the initial extent to fit the "country" layer
    
    # convert the tmap object to a leaflet map
    tmap_leaflet(tmap_object)
  })
  
  # Observe the map click event and add a marker at the clicked spot
  observe({
    # clear previous click
    leafletProxy("map") %>%
      clearMarkers()
    
    # get new click
    click <- input$map_click
    
    # add click to map
    leafletProxy("map") %>%
      addMarkers(lng = click$lng, lat = click$lat)
  }) %>%
    bindEvent(input$map_click)
  
  # get the clicked point as a reactive variable
  pt <- reactive({
    if (!is.null(input$map_click$lat)) {
      pt <- st_point(
        c(
          input$map_click$lng,
          input$map_click$lat
        )
      ) %>%
        st_sfc(crs = 4326) %>%
        st_as_sf() %>%
        st_transform(st_crs(my_data()))
      
      return(pt)
    } else {
      NULL
    }
  })
  # Get the clicked district
  clicked_district <- reactive({
    if(!is.null(pt())){
      my_data() %>% st_filter(pt(), .predicate = st_intersects) %>%
        st_drop_geometry() %>% pull(district)
    } else {
      NULL
    }
  })
  # Get the total number of farmers in the clicked district
  tot_dist_stat <- reactive({
    if (!is.null(clicked_district())) {
      my_data() %>% 
        filter(district == clicked_district()) %>% 
        pull(!!sym(statistic())) %>% 
        sum()
    } else {
      NULL
    }
  })
  output$distr_total <- renderUI({
    if (!is.null(clicked_district())) {
      tagList(
        h1(paste0(clicked_district(), ": ", tot_dist_stat()))
      )
    } else {
      h5("District total")
    }
  })
  # Get the clicked sector
  clicked_sector <- reactive({
    if(!is.null(pt())){
      my_data() %>% st_filter(pt(), .predicate = st_intersects) %>%
        st_drop_geometry() %>% pull(sector)
    } else {
      NULL
    }
  })
  # Get the total number of farmers in the clicked sector
  total_sect_stat <- reactive({
    if (!is.null(clicked_sector())) {
      my_data() %>% 
        filter(sector == clicked_sector()) %>% 
        pull(!!sym(statistic())) %>% 
        sum() # is this necessary???
    } else {
      NULL
    }
  })
  # Calculate the sector proportion in district
  sector_percentage <- reactive({
    return(total_sect_stat()/tot_dist_stat())
  })
  # Display sector performance gauge
  output$sect_perform <- renderPlotly({
    progress_value <- sector_percentage() * 100  # Convert to percentage
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = progress_value,
      title = list(
        text = paste("Sector", clicked_sector(), sep = " ")
      ),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, 100)),  # Update range to percentage range
        bar = list(color = "#1E893A"),
        steps = list(
          list(range = c(0, 100), color = "lightgray")  # Update range to percentage range
        )
      ),
      number = list(suffix = "%")  # Display number as percentage
    )
  })
  
  # filter my data and display only the records relevant for the clicked spot
  output$details <- renderTable({
    tryCatch({
      # Attempt to filter the data based on the clicked district
      filtered_data <- my_data() %>% 
        st_drop_geometry() %>% 
        filter(district == clicked_district()) %>%
        select(-c(season, district, number_of_farmers, total_volume))
      
      if (nrow(filtered_data) == 0) {
        return(data.frame(Message = "No records found for the clicked district"))
      } else {
        return(filtered_data)
      }
    }, error = function(e){
      return(data.frame(Message = "Please clik on a colored sector to see more details"))
    })
  })
  
}

shinyApp(ui, server)
# This script maps maize distribution data for seasons 2023A and 2023B.
# The data is downloaded from a Google spreadsheet geometry data is added from Rwanda geodatabase.
# The map is imported into a shiny app to make it interactive.
# Author: Faustin GASHAKAMBA
# Date: October 2023
#################################################################################
if(!"remotes" %in% installed.packages()){
  install.packages("remotes")
}
cran_pkgs = c("magrittr", "dplyr", "stringr", "tidyr", "googlesheets4", "shiny", "RPostgres", "sf", "tmap", "viridis", "janitor")
remotes::install_cran(cran_pkgs)

# load required packages
libraries <- c("magrittr", "dplyr", "stringr", "tidyr", "googlesheets4", "shiny", "RPostgres", "sf", "tmap", "viridis", "janitor")
lapply(libraries, require, character.only = T)

#Authenticate googlesheets4 (replace the email as appropriate)
if(!gs4_has_token()){gs4_auth(email = "faustin.gashakamba@oneacrefund.org")}

# Download tree nurseries data from the google spreadsheet
url <- "https://docs.google.com/spreadsheets/d/1JK6Y6romZympCv46Iyyy6XpKHxmEWNV6MW0x4ylM5DM"
data <- range_read(ss = url, sheet = "Maize", col_names = T, range = "A4:U414",
                   col_types = paste(c(rep("c", 8), rep("n", 1), rep("c", 2), rep("n", 1), rep("c", 2),
                                       rep("n", 1), rep("c", 2), rep("n", 1), rep("c", 1), rep("n", 2)), collapse = ""))
# clean the column names and remove all_NA columns and rows
data %<>% clean_names(.) %>% select(where(~!all(is.na(.)))) %>% filter(!if_any(c(sector, district, number_of_farmers, total_volume), ~is.na(.)))

# Sanitize district & sector names
data %<>% mutate(across(c(district, sector), ~str_to_title(str_remove_all(str_squish(.),"[^[:alnum:][:space:]]"))))
data %<>% mutate(across(c(district, sector), ~ifelse(!str_detect(., "(?<!^)ri"), ., str_replace_all(., "(?<!^)ri", "li"))))

# manual correction of typos in the names of districts & sectors
data %<>% mutate(district = if_else(sector == "Mimuli", "Nyagatare", district))

# Load sector boundaries data
#==============================================
# Establish connection to the database
con <- dbConnect(drv = RPostgres::Postgres(), 
                 dbname = 'main_db',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'Mapengu333?')

# Load sector boundaries data
sectors <- st_read(con, query = "SELECT name AS sector, district, province, geom FROM geo.sectors")
sectors %<>% mutate(across(where(is.character), ~str_to_title(str_remove_all(str_squish(.),"[^[:alnum:][:space:]]"))))
sectors %<>% mutate(across(where(is.character), ~ifelse(!str_detect(., "(?<!^)ri"), ., str_replace_all(., "(?<!^)ri", "li"))))

# load other basemap layers
lakes <- st_read(con, query = "SELECT * FROM geo.lakes")
np <- st_read(con, query = "SELECT * FROM geo.national_parks")
country <- st_read(con, query = "SELECT * FROM geo.country")

# fix invalid geometries
lakes %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)
np %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)
country %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)
sectors %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = F)

# Join the boundaries to the data
data %<>% group_by(season, district, sector) %>% summarize(number_of_farmers = sum(number_of_farmers), total_volume = sum(total_volume)) %>% 
  ungroup() %>% left_join(sectors, by = c("district", "sector")) %>% st_as_sf()

# CREATE AN INTERACTIVE MAP
#=====================================================
# set map options
tmap_options(check.and.fix = TRUE) # check for invalid geometries
tmap_mode("plot") # Set the view mode to "interactive"


# define a function create a tmap object depending on the user input
create_map <- function(my_season, statistic){
  # filter the data according to the user-selected season
  my_data <- data %>% filter(season == my_season)
  
  # create the map object
  my_map <- tm_shape(sectors) + 
    tm_borders(col = "#A76948", alpha = .6) +
    tm_shape(my_data) +
    tm_borders() +
    tm_fill(col = statistic,
            title = str_to_sentence(str_replace_all(statistic, "_", " ")),
            style = "jenks", 
            palette = viridis(7),
            alpha = .6) +
    tm_shape(lakes) + tm_polygons(col = "#2CA2E6", alpha = .6) +
    tm_shape(np) + tm_polygons(col = "#158849", alpha = .6) +
    tm_shape(country) + tm_borders(lwd = 2) +
    tm_compass(type = "4star", position = c("right", "top")) + #not supported in "interactive" mode
    tm_scale_bar() +
    tm_layout(main.title = paste(str_to_sentence(str_replace_all(statistic, "_", " ")), my_season, sep = " - "), # use sep = "\n" for multline title
              main.title.position = "center",
              main.title.color = "#885415",
              main.title.fontface = "bold",
              main.title.size = 1.5)
  
  # display the map
  print(my_map)
}


# Use shiny to visualize the map interactively by accepting user inputp
ui <- fluidPage(
  titlePanel("Maize aggregation map, Year 2023"),
  fluidRow(
    column(2, radioButtons("my_season", label = "Choose season", choices = list("2023A", "2023B"),  inline = TRUE)),
    column(4, selectInput("statistic",
                          label = "Choose the statistic to visualize", 
                          choices = list("Number of farmers"="number_of_farmers", "Quantity of aggregated maize"="total_volume")))
  ),
  fluidRow(
    column(12, plotOutput("map", width = "auto", height = "auto"))
  )
)

server <- function(input, output, session) {
  my_season <- reactive(input$my_season)
  statistic <- reactive(input$statistic)
  output$map <- renderPlot({
    #dev.new(useRStudioGD = TRUE) 
    create_map(my_season(), statistic())
  }, width = 800, height = 600)
}

shinyApp(ui, server)





# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Welcome to the Intermediate Course! (2023-02-06)

# Good Morning
print("Good morning!")
# notes

# versions
version


# Making Directories ------------------------------------------------------

# to create directory from code
dir.create("data")
dir.create("data_output")
dir.create("figs")


# Relative Paths ----------------------------------------------------------

install.packages("here")
library(here)

# download the data we will use

# downloads the file (data.zip) to your data directory
download.file("https://github.com/r4wrds/r4wrds-data-intro/raw/main/data.zip", destfile = "data/data.zip")

# unzip the data we need for the course:
unzip(zipfile = "data/data.zip")

# we can delete the _MACOSX dir
# click on the folder, and "Delete"

# relative paths
file.exists("data/healthy_watersheds/CA_PHWA_TabularResults_170518.xlsx")

# hard path:
file.exists("/Users/rapeek/Dropbox/R_TEACHING/r4wrds_live/data/healthy_watersheds/CA_PHWA_TabularResults_170518.xlsx")

# using the here package
here("data/healthy_watersheds/") # gives the full path

# now we can use this in a function
file.exists(here("data/healthy_watersheds/"))



# R profile ---------------------------------------------------------------

# usethis
# install.packages('usethis')
library(usethis)

edit_r_profile(scope = 'user')

# edit the project specific file
usethis::edit_r_profile(scope = 'project')

# restart R after editing rprofiles

# let's add a cowsay message
# install.packages('cowsay')

# what I'm putting in .rprofile
# cowsay::say("happy early birthday Ryan!")

print('Hi there!')


# THIS IS WHAT WENT INTO .RPROFILE...
library(cowsay)
library(glue)

cowsay::say(what = "Moo", by = 'cow')


# get a vector of all animals
animals <- names(cowsay::animals)

# get link to quotes
# get pieces to make link
repo <- "JakubPetriska/060958fd744ca34f099e947cd080b540"
csv <- "raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"
github <- 'https://gist.githubusercontent.com/'

URL <- glue('{github}{repo}/{csv}')

quotes <- readr::read_csv(URL)

quotes$full_quote <-  glue("{quotes$Quote} ~ {quotes$Author}")

# get random animal and quote
random_animal <- sample(x = animals, size = 1) # names might have caused teh issue
random_quote <- sample(quotes$full_quote , size = 1)
cowsay::say(by = random_animal, what = random_quote)

# END RPROFILE

# .Renviron files ---------------------------------------------------------

Sys.getenv()

Sys.getenv('USER') # on a mac
Sys.getenv('USERNAME') # on a windows


# create and edit a .renviron file with usethis
usethis::edit_r_environ()

Sys.getenv('ANIMAL')



# storing functions -------------------------------------------------------

# function = a set of codes that R reads and executes
mean(c(1, 2, 3))

# define functions
my_first_function <- function(){
  print("hello")
}

my_second_function <- function(){
  print("world")
}

# do the rest of your script below
my_first_function()
my_second_function()

# wehn you have a lot of functions, can put your custom function in a 
# seperate script

# I can call upon those functions by using source
source('scripts/two_functions.R')

my_third_function <- function(x){
  x2 <- x + 1
  x2 * 2
}
my_third_function(x = 5)


# another way to read in functions

# define the folder that contains my functions
functions_files <- fs::dir_ls('scripts/functions/')

purrr::walk(functions_files, ~source(.x))



# renv --------------------------------------------------------------------

# go check out this website for how to package up your packages
# https://rstudio.github.io/renv/articles/renv.html


# Interactive Visualization: Mapping from Googleform ----------------------

# the packages we will need
# install.packages("tidygeocoder")
library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)

# the spreadsheet your data went to:
# https://docs.google.com/spreadsheets/d/e/2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-/pub?gid=1462593645&single=true

form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                "/pub?gid=1462593645&single=true&output=csv")

# read in url and clean
dat <- read_csv(form_data) %>% 
  janitor::clean_names() %>% 
  dplyr::rename(dining_name = 3, dining_address = 4)

# let's filter to just data from this year (2023)
library(lubridate)

# format our date column as datetime
str(dat)

dat$timestamp <- mdy_hms(dat$timestamp)
glimpse(dat)

# filter to just this year
# create a new column of year
dat <- dat %>%
  mutate(year = year(timestamp), .after = timestamp)

# filter by year with year column
dat23 <- dat %>% 
  filter(year == 2023)

# now we look up addresses and get a lat lon for each
# geocode using Open Street Map (osm) API because it's free
dat_geo <- dat23 %>%
  geocode(address = dining_address, method = 'osm', lat = latitude , long = longitude)


# fix a few errors
dat23 <- dat23 %>% 
  mutate( dining_address = case_when(
    dining_name == "Blue Plate" ~ "3218 Mission St, San Francisco, CA 94110",
    grepl("Sala Thai", dining_name) ~ "3101 Zinfandel Dr #148, Rancho Cordova, CA, 95670",
    TRUE ~ dining_address
  ))

# geocode using Open Street Map (osm) API because it's free
dat_geo <- dat23 %>%
  geocode(address = dining_address, method = 'osm', lat = latitude , long = longitude)

# make into sf object so we can map (using the sf package)
dat_geo <- dat_geo %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = 4326, remove = FALSE)

# make an interactive map
mapview(
  dat_geo, 
  zcol = "comfort_using_r", 
  layer.name = "R comfort level", 
  cex = 6.5
)

# to save an interactive map, you can navigate to the Viewer tab, and look for the "Export" button. click the dropdown arrow, and "Save as Web Page". This should give you the ability to save a stand-alone html file. 
# You can then save this whereever you like. To share with collaborators, I often zip this html (so end up with a *.html.zip), email or share via flash drive. 
# folks can then unzip, and open in any browser
# note, maps with lots of data can get big, so this is best for smaller datasets.

# make a static map in R
ggplot() + geom_sf(data=dat_geo, aes(color=comfort_using_r))+ theme_classic()



# More Interactive Viz ----------------------------------------------------

library(igraph)
library(networkD3)

# create a dataset:
data <- tibble(
  from = c(
    "Dam","Dam","Dam", "Dam",
    "River","River","River", "River","River",
    "Canal", "Canal", 
    "Diversion","Diversion", 
    "Reservoir", "Reservoir","Reservoir",
    "Lake","Lake","Lake", "Lake", 
    "Road","Road","Road",
    "Culvert", "Culvert",
    "Fish", "Fish","Fish",
    "Frog","Frog","Frog",
    "MacroInvertebrates","MacroInvertebrates"
  ),
  to = c(
    "River","Reservoir","Canal","Diversion",
    "Lake","Reservoir","Frog","Fish","MacroInvertebrates",
    "Diversion", "Reservoir",
    "Dam", "River",
    "River","Dam","Fish",
    "Fish","Dam","Frog","MacroInvertebrates",
    "Fish","Dam", "Canal",
    "Road", "Dam",
    "Frog", "River","MacroInvertebrates",
    "Fish", "River", "Lake",
    "River", "Lake"
  )
)

# Plot
(p <- simpleNetwork(data, height = "600px", width = "600px", 
                    fontSize = 16, fontFamily = "serif",
                    nodeColour = "darkblue", linkColour = "steelblue",
                    opacity = 0.9, zoom = FALSE, charge = -500))


# for loops ---------------------------------------------------------------

library(tidyverse)
library(here)

# # # these dont actually exist
# # eldorado <- read_csv(here("data", "gwl", "county", "El Dorado.csv"))
# # placer   <- read_csv(here("data", "gwl", "county", "Placer.csv"))
# # sac      <- read_csv(here("data", "gwl", "county", "Sacramento.csv"))
# # 
# 
# # I need to create 3 csv files from a single one
# # read in a csv
# big_file <- read_csv('data/gwl/gwl.csv')
# 
# # create a folder for county
# dir.create('data/gwl/county')
# 
# # create a csv file for 3 different counties
# big_file$COUNTY_NAME %>% unique
# big_file %>% 
#   filter(COUNTY_NAME == 'Sacramento') %>% 
#   write_csv('data/gwl/county/Sacramento.csv')
# big_file %>% 
#   filter(COUNTY_NAME == 'Sacramento') %>% 
#   write_csv('data/gwl/county/Sacramento.csv')

# redownload the correct data folder

# downloads the data.zip file to the `data` directory
download.file("https://github.com/r4wrds/r4wrds-data-intermediate/raw/main/data.zip", destfile = "data/data.zip")

# unzip the data:
unzip(zipfile = "data/data.zip")

# if get resulting __MACOSX folder (artifact of unzip), remove:
unlink("__MACOSX", recursive = TRUE)


# I want to read in 3 different csv
sacramento <- read_csv('data/gwl/county/Sacramento.csv')
placer <- read_csv('data/gwl/county/Placer.csv')
eldorado <- read_csv('data/gwl/county/El Dorado.csv')

# list out the files you want to read in
files_in <- fs::dir_ls('data/gwl/county/')

# instantiate a list of known length
l <- vector('list', length = length(files_in))

# for-loop
for(idx in seq_along(files_in)){
  l[[idx]] <- read_csv(files_in[idx])
}

str(l) # this gives blueprint of an object
l[[1]] # access the first element in the list

# you want to write multiple csvs but joined together by a different variable
ldf <- l %>% 
  bind_rows()
ldf$BASIN_NAME %>% unique

ldf <- ldf %>% 
  split(ldf$BASIN_NAME)

# loop over each list element and write a csv file
fs::dir_create(here("data", "gwl", "basin_name"))


# here we make a list of files names ( names(ldf) is a vector )
files_out <- glue::glue("{here('data', 'gwl', 'basin_name', names(ldf))}.csv")

for(i in seq_along(ldf)){
  write_csv(ldf[[i]], files_out[i])
}



# for loops again ---------------------------------------------------------

library(tidyverse)

# I repeated 3 functions and used up 3 lines
eldorado <- read_csv('data/gwl/county/El Dorado.csv')
placer <- read_csv('data/gwl/county/Placer.csv')
sac <- read_csv('data/gwl/county/Sacramento.csv')


# list all files we want to read in
files_in <- fs::dir_ls("data/gwl/county")

# initialize a list of defined length
l <- vector("list", length = length(files_in))
# another way to make a list, but only creates a list with 1 element: list(NULL)

# loop over all files and read them into each element of the list
for(i in seq_along(l)){
  l[[i]] <- read_csv(files_in[i])
  print(paste0('step: ', i))
}

# access list elements with [[ ]] notation
l[[1]] # returns first list element
l[[2]] # returns the second list element
l[[3]] # returns the third element, and so on

# # why is making l important? 
# rm(l)
# for(i in seq_along(files_in)){
#   read_csv(files_in[i])
# }

# to test out the for loop, you can say i = 1
i = 1
l[[i]] <- read_csv(files_in[i])


# combine all list elements into a single dataframe
# then split into another list by SITE_CODE.
ldf <- bind_rows(l) # originally a list, turn into a single df.
ldf <- split(ldf, ldf$SITE_CODE) # split that dataframe into a list, grouped by a different variable

# loop over each list element and write a csv file
fs::dir_create("data/gwl/site_code") # make a folder
# here we make a list of files names ( names(ldf) is a vector )
files_out <- glue::glue("data/gwl/site_code/{names(ldf)}.csv") 

paste0('data/gwl/site_code/', names(ldf), '.csv') # another way to do what glue did

# note that the string "\n" means "new line"
for(i in seq_along(ldf)){
  write_csv(ldf[[i]], files_out[i])
  cat("Just wrote file #", i, "\n")
}



# lapply - a for loop simplified ------------------------------------------

# read in our files with read_csv and lapply
# lapply stands for LIST apply
# takes two arguments, a vector/list followed
# by a function, and it APPLIES the function on the list
# and returns a list as output (list in, list out)

# create filepaths read, as before
files_in <- fs::dir_ls("data/gwl/county/")

l <- lapply(files_in, read_csv)
class(l)
l[[1]]
unique(l[[1]]$COUNTY_NAME)
length(l)

# bind the list elements (3 dataframes) together into 
# one dataframe and split into another list by SITE_CODE
ldf <- bind_rows(l)
ldf <- split(ldf, ldf$SITE_CODE)

# write each site code dataframe to a csv
# remember we have the variable files_out from above

# use mapply (MULTIPLE apply) start with a function, 
# and pass it multiple arguments"
#   1. (list of site dataframes, ldf)
#   2. output file paths to writte the files to, files_out 

# this works - just give your function
mapply(write_csv, ldf, files_out)

# or save as a function if you want to apply a more complex 
# function
my_function <- function(x, y){
  write_csv(x, y)
}

# do the same thing as before, but with a named function
mapply(my_function, x = ldf, y = files_out)



# map - same as apply, but cleaner syntax ---------------------------------

# read in 3 dataframes from 3 counties
# map(some_list, ~some_function(.x))
# .x is your "i" from the for loop
l <- map(files_in, ~read_csv(.x))

# bind and split by site code
ldf <- bind_rows(l)
ldf <- split(ldf, ldf$SITE_CODE)

# write a csv for each site code
# .x and .y are the "counters" or "placeholders" for the
# first two arguments of the function
walk2(ldf, files_out, ~write_csv(.x, .y))

# what this is doing is the following:
# write_csv(ldf[[1]], files_out[1])
# write_csv(ldf[[2]], files_out[2])
# .
# .
# .
# write_csv(ldf[[n]], files_out[n])

# this is the structure of walk2
# walk2(.x, .y, ~function(.x, .y))




# iterate over shapefiles -- a more complex example -----------------------

# read csv, filter to a well type, create a new column,
# convert to a spatial object, reproject the coordinates, 
# write the shapefile -- and ITERATE

# import dataframe, filter to observation wells, convert well 
# depth feet to meters, project to epgs 3310, & export the data
f_import_clean_export <- function(file_in, file_out){
  read_csv(file_in) %>% 
    filter(WELL_USE == "Observation") %>% 
    mutate(well_depth_m = WELL_DEPTH * 0.3048) %>% 
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>% 
    sf::st_transform(3310) %>% 
    sf::st_write(file_out, delete_layer = TRUE)
}

# create a directory to store results
fs::dir_create("results")

# need files in and files out
files_in <- fs::dir_ls("data/gwl/county/")
shp <- str_replace_all(basename(files_in), ".csv", ".shp")
files_out <- glue::glue("results/{shp}")

walk2(files_in, files_out, ~f_import_clean_export(.x, .y))
map2(files_in, files_out, ~f_import_clean_export(.x, .y))

# walk is the same as map except it silently performs the function

# the above is the same as:
# f_import_clean_export(files_in[1], files_out[1])
# f_import_clean_export(files_in[2], files_out[2])
# f_import_clean_export(files_in[3], files_out[3])



# shiny app breakout ------------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)

# load sac county groundwater data and sac county polygon
gwl <- read_csv("gwl_sac.csv") %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>% 
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))

# ------------------------------------------------------------------------
# user interface
ui <- fluidPage(
  
  # change default theme to "united"
  theme = shinytheme("united"),
  
  # application title
  titlePanel("Sacramento County Groundwater Level Data"),
  
  # sidebar with a dropdown input for site_code
  sidebarLayout(
    sidebarPanel(
      selectInput("site_code",
                  "Select a site code:",
                  choices = unique(gwl$SITE_CODE))
    ),
    
    # tabs with hydrograph and data table
    mainPanel(
      tabsetPanel(
        tabPanel("Hydrograph", plotly::plotlyOutput("gwl_plot")),
        tabPanel("Data", DT::dataTableOutput("gwl_data"))
      ),
      tabPanel("Map", leafletOutput("map")
      )
    )
  )
)

# ------------------------------------------------------------------------
# define server logic to plot a hydrograph and create data table
server <- function(input, output) {
  
  # --------------------------------------------------
  # create hydrograph
  output$gwl_plot <- plotly::renderPlotly({
    
    # draw the ggplot based on the "site_code" user input
    filter(gwl, SITE_CODE == input$site_code) %>%
      ggplot(aes(MSMT_DATE, WSE)) +
      geom_line(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = input$site_code,
           x = "", y = "Groundwater level (ft AMSL)")
    
  })
  
  # --------------------------------------------------
  # create data table
  output$gwl_data <- DT::renderDataTable({
    
    # draw the plot based on the "site_code" user input
    DT::datatable(
      filter(gwl, SITE_CODE == input$site_code),
      extensions = 'Buttons',
      options =
        list(dom = 'Bfrtip',
             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
    )
  })
  
  # create the leaflet map  
  output$map <- renderLeaflet({
    gwl %>% 
      group_by(SITE_CODE) %>%
      slice(1) %>% 
      filter(SITE_CODE == input$site_code) %>% 
      ungroup() %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(lng = ~long, lat = ~lat)
  })
}

# ------------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)

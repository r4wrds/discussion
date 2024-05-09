# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Live Code ------------------------------------------

# Good Morning!

print("Hello")
print("Good morning")


# Make Data Set and Download ----------------------------------------------

dir.create("data")
dir.create("scripts")

# download with code:
download.file("https://github.com/r4wrds/r4wrds-data-intermediate/raw/main/data.zip", destfile = "data/data.zip")
# if you get a SSL error, try manually downloading the file via a web browser
# then move the data.zip file into the RProject data folder before proceeding.

# unzip
unzip(zipfile = "data/data.zip")
unlink("__MACOSX", recursive = TRUE)

# updating R and R Studio -------------------------------------------------

# updating R version quarterly, using the CRAN website: https://cran.r-project.org/]

# updating RStudio: Help > Check for Updates

# This is a Section label -------------------------------------------------

# get a section label by Ctrl + Shift + R

# then look for little button in upper right of Source panel that is little horizontal lines

# .rprofiles --------------------------------------------------------------

# in case you don't have this package
# install.packages("usethis")

# to set the global .rprofile file
usethis::edit_r_profile()

# project level rprofile
usethis::edit_r_profile(scope = 'project')


# environmental variables -------------------------------------------------

names(Sys.info())
Sys.getenv()
# see what env vars
names(Sys.getenv())

# how to access an environmental var
Sys.getenv()[['USER']] # mac

Sys.info()[['user']] # windows
# also windows:
Sys.getenv('USERNAME')

# how to create a new environemtnal variable
usethis::edit_r_environ()

# after setting my new env var, restart session, and access it
Sys.getenv()[['ANIMAL']]


# functions and how to store them -----------------------------------------

# load up packages
library(tidyverse)
library(fs)

# list out your functions
my_first_function <- function(x){
  return(x) 
}
my_second_function <- function(x){
  x2 <- x * 2
  return(x2)
}
my_third_function <- function(x){
  return(x*2)
}


# test out my funciton 
my_first_function(10)
my_second_function(10)
my_third_function(10)

# let's restart R

# want to load up functions from the `function.R` file
source('scripts/functions.R')


# Interactive Visualization -----------------------------------------------

# if we want to install a vector c() of packages:
lib.list <- c("tidygeocoder", "tidyverse", "janitor","glue", "sf",
              "mapview", 
              "basemaps", "tigris","terra",
              "tidyterra")

# install all of them
install.packages(lib.list) # all of them
# install only a few:
install.packages(lib.list[c(3:4)])

# these are packages we'll be using:
library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)

# try the demo for geocoding
form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                    "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")

# read in url and clean
dat <- read_csv(form_data) |>  # read data
  janitor::clean_names() |> #
  dplyr::rename( dining_name = 3, dining_address = 4)

# next filter to only the places that haven't been geocoded
dat_na <- dat |>
  dplyr::filter(is.na(y_lat))

# geocode using Open Street Map (osm) API because it's free
dat_geo <- dat_na |> 
  tidygeocoder::geocode(address = dining_address, 
                        method = 'osm', 
                        lat = latitude , long = longitude)

# we want to use this new geocoded data and merge with the old geo data
dat_geo_filt <- dat_geo |> 
  filter(!(is.na(latitude))) |>
  select(-c(y_lat, x_lon))

# bind that to the other dataset...but first remove NAs and rename columns
dat_final <- dat |> 
  filter(!(is.na(y_lat))) |> 
  rename(latitude = y_lat, longitude = x_lon) |> 
  dplyr::bind_rows(dat_geo_filt)

# make into sf object so we can map
dat_final_geo <- dat_final |>   
  filter(!is.na(latitude) & !is.na(longitude)) |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# map!
mapview::mapview(dat_final_geo)

# this adds some nuance
mapview::mapview(
  dat_final_geo, 
  zcol = "comfort_using_r", 
  layer.name = "R comfort level", 
  cex = 6.5
)

# what if we want to add some county or state data
library(tigris)
library(basemaps)
library(terra)
library(tidyterra)


# Get State / County Boundaries -------------------------------------------

ca <- tigris::states() |> filter(STUSPS=="CA") |> sf::st_transform(4326)
cnty <- tigris::counties(state = "CA") |> st_transform(4326)


# Subset Spatially --------------------------------------------------------

dat_final_geo_ca <- dat_final_geo[ ca,]

mapview(dat_final_geo_ca)



# Static Map --------------------------------------------------------------

base_map <- 
  ggplot() + 
  geom_sf(data=cnty, fill=alpha("gray",0), color="gray40") +
  geom_sf(data=ca, color="blue", lwd=2, fill=alpha("gray",0))+
  geom_sf(data=dat_final_geo_ca, fill="yellow", pch=21, size=5, alpha=0.8) 

base_map




# for loops ---------------------------------------------------------------

my_vector <- c('apple', 'banana', 'cherry')

# capitalize all of these words
toupper(my_vector)

# how a for loop works
for(i in 1:length(my_vector)){
  print(toupper(my_vector[i]))
}


# break this down
i = 1
toupper(my_vector[i])
i = 2
toupper(my_vector[i])


# write out a for loop and store the results in a list
# create an empty list to store the output
l <- vector('list', length = length(my_vector))
l

# rewrite the same for loop but add data the list
for(i in 1:length(my_vector)){
  l[[i]] <-  toupper(my_vector[i])
}
l

# this is why it's good to use `length(my_vector)` rather than hard coding 1:3
my_vector <- c(my_vector, 'dog')
for(i in 1:3){
  l[[i]] <-  toupper(my_vector[i])
}
l # doesn't include everything in my_vector




# Shift + Command/Control + R = New Section


# Iteration ---------------------------------------------------------------

library(tidyverse)
library(glue)

# read in gwl data for 3 counties in northern California
eldorado <- read_csv("data/gwl/county/El Dorado.csv")
placer   <- read_csv("data/gwl/county/Placer.csv")
sac      <- read_csv("data/gwl/county/Sacramento.csv") 

# read 3 dataframes into a list

# vector of all the files we want to read in
files_in <- fs::dir_ls("data/gwl/county/")

# initialize a list of defined length
l <- vector("list", length = length(files_in))

l[[1]] # list indexing: return the first element of the list

# loop over all files and read them into the list
for(i in seq_along(l)){
  cat("Reading data from:", files_in[i], "\n")
  l[[i]] <- read_csv(files_in[i])
}

# Aside: for loop primer
for(i in 1:1000000){
  cat(i, "\n")
}

# Aside: seq() to create sequences
seq(2, 98, by = 4)


# inspect data
l[[1]]
l[[1]]$COUNTY_NAME
l[[3]] # Sac Co df

# combine lists into a single dataframe
colnames(l[[1]])
colnames(l[[2]])
colnames(l[[3]])

ldf <- bind_rows(l)
ldf <- split(ldf, ldf$SITE_CODE) # returns a named list

# one approach that does not scale to tens, hundreds, or more elements
write_csv(ldf[[1]], "data/site_code/1.csv")
write_csv(ldf[[2]], "data/site_code/2.csv")
write_csv(ldf[[3]], "data/site_code/3.csv")
write_csv(ldf[[4]], "data/site_code/4.csv")
write_csv(ldf[[5]], "data/site_code/5.csv")
write_csv(ldf[[6]], "data/site_code/6.csv")
...
...
...
...
write_csv(ldf[[723]], "data/site_code/723.csv")

# create a directory to write results to
fs::dir_create("data/gwl/site_code")

# vector (1D data structure) of file names to write to
# glue combines characters with {R variables}
# glue("my characters {my R variables}")
files_out <- glue("data/gwl/site_code/{names(ldf)}.csv")

for(i in seq_along(ldf)){
  cat("Step:", i, "writing", names(ldf)[i], "\n")
  write_csv(ldf[[i]], files_out[i])
}



# iteration with map() ----------------------------------------------------

# syntax of map()
# map(input_list_or_vector, ~function(.x))
# ~ means start the function here
# .x means input elements 1:length(input) here
# map() always returns a list equal in length to the input list/vector

l <- map(files_in, ~read_csv(.x))

ldf <- bind_rows(l)
ldf <- group_split(ldf, SITE_CODE) # does not return a named list

# write
# walk2(input_x, input_y, ~function(.x, .y)) synax
walk2(ldf, files_out, ~write_csv(.x, .y))

# combine map() with pipes
# map_df() is shorthand for map() %>% bind_rows()
map_df(files_in, ~read_csv(.x)) %>% 
  group_split(SITE_CODE) %>% 
  walk2(files_out, ~write_csv(.x, .y))

# you can extend this example to do powerful things
f_import_clean_export <- function(file_in, file_out){
  read_csv(file_in) %>% 
    filter(WELL_USE == "Observation") %>% 
    mutate(well_depth_m = WELL_DEPTH * 0.3048) %>% 
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>% 
    sf::st_transform(3310) %>% 
    sf::st_write(file_out, delete_layer = TRUE)
}

files_out_shp <- glue('data/gwl/shp/{str_replace_all(basename(files_in), ".csv", ".shp")}')

walk2(files_in, files_out_shp, ~f_import_clean_export(.x, .y))




# check if mapview works --------------------------------------------------

# this should produce a map in the viewer
install.packages('mapview')
mapview::mapview()




# basic map in ggplot -----------------------------------------------------

library(tidyverse)
library(sf) # for vectorbased data

# raster data: `terra`, `raster`

# get CA and CA county polygons
# install.package('tigris')
states <- tigris::states(cb = T)
class(states)

ca <- states %>% 
  filter(STUSPS == 'CA')
plot(ca) # plots every field in the df
plot(st_geometry(ca)) # plots just the geometry

# counties
co <- tigris::counties(state = 'CA')

# lets read in the sac county shapefile
path_to_sac <- 'data/shp/sac_county/sac_county.shp'
sac <- st_read(path_to_sac)

# the crs doesn't match the others
# transform sac
st_transform(sac, 4269) # this works, but not great practice to hard code values
sac <- st_transform(sac, st_crs(ca)) # the better way
sac <- st_transform(sac, 4269) # just for allessandra

# make a bounding box around sac
bbox <- st_make_grid(sac, 1) 

# plot a map in ggplot
theme_set(theme_bw())

ggplot() +
  geom_sf(data = ca) + 
  geom_sf(data = co, color = 'white') + 
  geom_sf(data = sac, fill = 'lightblue') + 
  geom_sf(data = bbox, color = 'yellow', fill = NA, lwd = 1)


# ingest points from stations
gw_files <- fs::dir_ls('data/gwl/county/')

# wrangle data
gw_data <- map(gw_files, read_csv) %>% 
  bind_rows() %>% # binds the df
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326) # converts df to sf
gw_data <- st_transform(gw_data, st_crs(ca))
gw_data <- st_transform(gw_data, st_crs(4269))

# plot it to make sure it works
ggplot() +
  geom_sf(data = ca) + 
  geom_sf(data = co, color = 'white') + 
  geom_sf(data = sac, fill = 'lightblue') + 
  geom_sf(data = bbox, color = 'yellow', fill = NA, lwd = 1) +
  geom_sf(data = gw_data, color = 'pink')



# mapview -----------------------------------------------------------------


library(mapview)

mapview(gw_data, zcol ="COUNTY_NAME", layer.name = "GW Stations") +  # can change the colors based on a field
  mapview(sac, legend = F)



# spatial manipulations ---------------------------------------------------

# make a plotting function to see what the manipulations do
plot_gg <- function(dat){
  ggplot() +
    geom_sf(data = sac) +
    geom_sf(data = dat, aes(color = COUNTY_NAME))
}
plot_gg(gw_data)

# filtering data
head(gw_data)
gw_sac <- gw_data %>% 
  filter(COUNTY_NAME == "Sacramento")
plot_gg(gw_sac)

# spatial indexing
gw_sac <- gw_data[sac,]
plot_gg(gw_sac)

# st_intersection
gw_sac <- st_intersection(gw_data, sac)
gw_sac # 9 additional columns were added
plot_gg(gw_sac)


# antijoins with st_difference
st_difference(gw_data, sac) %>% plot_gg()



# more details on mapping, including rasters ------------------------------

# https://github.com/ryanpeek/r_tidbits/blob/main/scripts/map_demo.R

# add a basemap from raster data
# install.packages('basemaps')
library(basemaps)
library(terra)

base_ca <- basemap_terra(ca)
plot(base_ca)

# this will chnage to sat imagery
set_defaults(map_service = 'esri', map_type = 'world_imagery')
base_ca <- basemap_terra(ca)
plot(base_ca)

# manipulations I need to do
base_ca_crop <- crop(base_ca, st_transform(ca, crs(base_ca)), mask = T)
plot(base_ca_crop)

# map this baby in ggplot
library(tidyterra)

ggplot() +
  geom_sf(data = ca) + 
  geom_spatraster_rgb(data = base_ca_crop) +
  geom_sf(data = co, color = 'white', fill = NA) + 
  geom_sf(data = sac, fill = 'lightblue') + 
  geom_sf(data = bbox, color = 'yellow', fill = NA, lwd = 1)





# Shiny App (need to copy/paste this into an app.R file) ------------------
# ensure required data is in the same directory as the .app file

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)

# load sac county groundwater data and sac county polygon
gwl <- read_csv("gwl_sac.csv")
gwl_sf <- gwl %>% 
  group_by(SITE_CODE) %>% 
  slice(1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269, remove = FALSE)

# ------------------------------------------------------------------------
# user interface
ui <- fluidPage(
  
  # change default theme to "united"
  theme = shinytheme("flatly"),
  
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
      leaflet::leafletOutput("map")
    )
  )
)

# ------------------------------------------------------------------------
# define server logic to plot a hydrograph and create data table
server <- function(input, output) {
  
  # --------------------------------------------------
  # create map
  output$map <- renderLeaflet({ 
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(
        lng   = filter(gwl_sf, SITE_CODE == input$site_code)$LONGITUDE, 
        lat   = filter(gwl_sf, SITE_CODE == input$site_code)$LATITUDE, 
        popup = filter(gwl_sf, SITE_CODE == input$site_code)$SITE_CODE
      )
  })
  
  # --------------------------------------------------
  # create hydrograph
  output$gwl_plot <- plotly::renderPlotly({
    
    # draw the ggplot based on the "site_code" user input
    p <- filter(gwl, SITE_CODE == input$site_code) %>%
      ggplot(aes(MSMT_DATE, WSE)) +
      geom_line(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = input$site_code,
           x = "", y = "Groundwater level (ft AMSL)")
    
    # render the plotly object
    plotly::ggplotly(p)
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
}

# ------------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)

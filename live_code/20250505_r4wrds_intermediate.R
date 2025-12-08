
# Live Code ------------------------------------------

#################################################################
# DAY 1 --------------------------------------------------------
#################################################################

# What is my working directory?
getwd()

read.csv("data/nwis_sites_american_river.csv")

# Install the here package
install.packages('here')
library(here)

read.csv(here("data", "nwis_sites_american_river.csv"))

# Development environments 

## 1. R environments ---- 
## APIs as data sources: EPA, USGS, California Water Exchange, Open ET

Sys.getenv()

Sys.getenv('USER') # 'USERNAME'

# use the 'usethis' package to look at different environments
install.packages('usethis')
library(usethis)

# When setting new environments, need to restart
edit_r_environ()

# allows to store private information without it being saved into a script or hard coded
Sys.getenv('API_KEY') 

# remove this
edit_r_environ()

# 2. R profiles ----
# R profile lets you run a standard script or code at the start of every project

# 2 levels: user, project
usethis::edit_r_profile(scope = "project")

# 3. sourcing in files ----
# Similar to the R profile, source in a specific script to run it quickly
# Common with function scripts
## This script will in your intro_proj

# source('scripts/functions.R')

# 4. sessionInfo ---- 
## can share and freeze the package and r versions to make it fully reproducible 
sessionInfo()

# renv package to help 'lock' your sessionInfo



# module 4: interactive visualization -------------------------------------

# load up the packages
# if you need to, install packages with: 
# `install.packages('<package name>`)
library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)

# import the data
form_data <- paste0(
  "https://docs.google.com/spreadsheets/d/e/",
  "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
  "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
  "/pub?gid=1462593645&single=true&output=csv")

# read in the data
dat <- read_csv(form_data) %>% # pipe %>% cntl + shift + M
  clean_names() %>% # cleans up column names
  rename(dining_name = 3, dining_address = 4)

head(dat)

# geocode the address strings to become lat/long coords
dat_geo <- dat %>% 
  geocode(address = dining_address, lat = latitude, long = longitude)

# the string address
dat$dining_address[1:5]

# the spatial coordinates
dat_geo$longitude[1:5]
dat_geo$latitude[1:5]

dat_geo %>% 
  print(width = Inf)

# convert dat_geo to a spatial dataframe
dat_geo <- dat_geo %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% # filters out missing data
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, remove= F)

# the dimensions
dim(dat_geo)

# map it using mapview
mapview(
  dat_geo, zcol = 'comfort_using_r', 
  layer.name = 'R comfort leve', 
  cex = 6.5
)


library(leafpm)
library(leaflet)
library(leaflet.extras)
library(htmltools)


# set up our map
m <- leaflet() %>%
  # add tiles or the "basemaps"
  # REMOVE TONER LITE BASEMAP
  addTiles(group = "OSM") %>% # defaults to Open Street Maps
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>% 
  addCircleMarkers(
    lng = -121.4944, lat = 38.5816, fillColor = "red", color = "black",
    popup = "Sacramento!", group = "Home",
  ) %>% 
  addCircleMarkers(
    data = dat_geo, group = "Food & Drink",
    label = ~htmlEscape(first_name),
    popup = glue(
      "<b>Name:</b> {dat_geo$first_name}<br>
      <b>Food_Name:</b> {dat_geo$dining_name}<br>
      <b>Food_Address:</b> {dat_geo$dining_address}<br>
      <b>R comfort (1-10):</b> {dat_geo$comfort_using_r}"
    )
  )  %>% 
  addLayersControl(
    # REMOVE TONER LITE FROM BASEGROUPS
    baseGroups = c("Positron", "OSM"),
    overlayGroups = c("Home", "Food & Drink"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addMeasure()

m  # Print the map



# short refresher on functions --------------------------------------------


# see modules for references: 
# https://r4wrds.netlify.app/intermediate/m_project_management
# https://r4wrds.netlify.app/intro/m_functions

# the teaser

# install.packages('cowsay')
library(cowsay) # animals!
library(glue)   # pasting things together

# get vector of all animals
animals <- cowsay::animals

# get pieces to make link
repo <- "JakubPetriska/060958fd744ca34f099e947cd080b540"
csv <- "raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"

# get dataframe of inspirational quotes
quotes  <- readr::read_csv(glue("https://gist.githubusercontent.com/{repo}/{csv}"))  

# make full quote
quotes$full_quote  <- glue("{quotes$Quote} - {quotes$Author}")

# now use it!
cowsay::say(sample(quotes$full_quote, 1), by = sample(animals, 1))

# by the end of this lesson, you'll be able to write a custom function
# that randomly selects an animal and a quote


# function basics
x <- 1:10
mean(x) # mean = the function

# anatomy of a function
# function_name <- function(input){
# # do stuff in your fucntion, manipulate inputs
# res <- ....
# return(res)
# }

my_mean <- function(x){
  # sum(x)/length(x)
  total <- sum(x)
  count <- length(x)
  result <- total/count
  return(result)
}
my_mean(x)

# a fucntion iwth 2 parameters
my_mean2 <- function(x, na.rm = T){
  if(na.rm==T){
    x <- x[!is.na(x)] # remove NA
  }
  # do the same thing as my_mean
  total <- sum(x)
  count <- length(x)
  result <- total/count
  return(result)
}
my_mean2(x)
y <- c(1:10, NA)
my_mean2(y)
my_mean2(y, na.rm = T)
my_mean2(y, na.rm = F)


# lets make a custom function

wise_animal <- function(animal_name = NA){
  
  if(is.na(animal_name)){
    # randomly choose an animal name
    animal <- sample(cowsay::animals, 1)
  }else{
    animal <- animal_name
  }
  
  # get pieces to make link
  repo <- "JakubPetriska/060958fd744ca34f099e947cd080b540"
  csv <- "raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"
  
  # get dataframe of inspirational quotes
  quotes  <- readr::read_csv(glue("https://gist.githubusercontent.com/{repo}/{csv}"))  
  
  # make full quote
  quotes$full_quote  <- glue("{quotes$Quote} - {quotes$Author}")
  
  # now use it!
  cowsay::say(sample(quotes$full_quote, 1), by = animal)
  
}

wise_animal(NA)
cowsay::animals # all the animals that I can choose
wise_animal("anxiouscat")



# iterations --------------------------------------------------------------

# for loops
# apply family (lapply, mapply, sapply)
# map


# loop reading in a file path, manipulating it, writing out

library(tidyverse)

# the slow way
eldorado <- read_csv('data/gwl/county/El Dorado.csv')
placer <- read_csv('data/gwl/county/Placer.csv')
sac <- read_csv('data/gwl/county/Sacramento.csv')

# a faster way with a for loop
# gather all the paths
paths <- fs::dir_ls('data/gwl/county/')
paths
class(paths)


# initialize a vector (or list) to store the results
l <- list(NULL)

for(i in 1:length(paths)){
  l[[i]] <- read_csv(paths[i])
}
l

# concatenate and write out
l_df <- bind_rows(l)

#################################################################
# DAY 2 --------------------------------------------------------
#################################################################
## Make sure you follow these instructions to get the right data for today:
# 1. Make sure you are in your intro_proj R Project
# 2. Manually delete your data folder
# 3. Run the following code: 

download.file('https://github.com/r4wrds/r4wrds-data-intermediate/raw/main/data.zip', destfile = 'data.zip')
unzip('data.zip')
file.remove('data.zip')

## Check: do you have the correct files?
fs::dir_ls('data/gwl/county/')

# For loops for iteration ----

# Structure of a for-loop:
# for(<SYMBOL REPRESENTING INDEX> in <A SEQUENCE OF VALUES>){
#       run the code inside the curly brackets
#       as many times as specified in <A SEQUENCE OF VALUES>
#       and use the <SYMBOL REPRESENTING INDEX>
#       to index from whatever it is you are iterating over
#}

# Simple example

for(i in 1:10){
  print(paste("The loop is running through iteration:", i))
  Sys.sleep(1)
}

files_in <- fs::dir_ls('data/gwl/county')
files_in
typeof(files_in)

# create a sequence as long as vector we want to iterate over
# hard code the sequence
1:3
# soft code in by using length()
1:length(files_in)

# for loop to read in csvs and compile them
library(tidyverse)

# 1. initialize a vector or list that is empty to store out output
l <- list(NULL)

# 2. Iterative through the different file names
for(i in 1:length(files_in)){
  l[[i]] <- read_csv(files_in[i])
}

# remember that l[[i]] calls on the ith element in the list, and files_in[i] calls on the ith element of a vector

# how to keep the name of the csv?
basename(files_in)
# manipulate text to make it cleaner
#install.packages('stringr')
library(stringr)

# reduce the file paths down to their country names
# added them to a names vector
names_v <- str_remove(basename(files_in), '\\.csv')

l <- list(NULL)
# integrate this names vector into our for loop
for(i in 1:length(files_in)){
  l[[i]] <- read_csv(files_in[i])
  names(l)[i] <- names_v[i]
}

l[[1]]

# 3. Compiling 
ldf <- bind_rows(l)
?bind_rows

# differ rbind?
ldf_rbind <- rbind(l)

# do.call(rbind)
ldf_docall <- do.call(rbind, l)

identical(ldf, ldf_docall)

# Example 2 of for loops, splitting and writing the data

unique(ldf$BASIN_NAME)
ldf_split <- split(ldf, ldf$BASIN_NAME)

# Create a new directory to store these CSVs
fs::dir_create('data_output/basin')
# Make a vector of the file names to save each csv as
files_out <- paste0("data_output/basin/", names(ldf_split), ".csv")
files_out

# use a for loop to write/save the data
for(i in 1:length(files_out)){
  write_csv(ldf_split[[i]], files_out[i])
}

# sidenote: paste versus paste0
?paste
# default separation
paste("Hello", "world")
# default no space -- good for file paths or urls
paste0("Hello", "world")

# Checking in that we're dealing with the right data
ldf <- bind_rows(l)
unique(ldf$BASIN_NAME)

# lists and indexing
# take the 1st element of a vector
files_in[1]
# don't use double brackets with vectors b/c unneccessary
files_in[[1]]
# lists have more depth
l[1] # gives us the whole data frame
l[[1]] # gives us the whole data frame more more focus on the more nested element

# Are lists automatically more nested? -- not necessaily but have potential
test_list <- list("cat", 1, 5:10, ldf[1])

# apply functions ----
?lapply
# lapply(vector/list, function)

files_in
# 1. reading in files
# iteration happens in 1 line with lapply
l <- lapply(X = files_in, FUN = read_csv)

# 2. Bind our rows
ldf <- bind_rows(l)

# 3. split our list by basin
ldf_split <- split(ldf, ldf$BASIN_NAME)

# 4. write the files
?mapply
# mapply(FUN, arguments...)

mapply(FUN = write_csv,
       x = ldf_split, file = files_out)

# pros: fast, less code
# cons: harder to troubleshoot -- for loops are nice for de-bugging because you can troubleshoot by printing in the loop or seeing what the value of i is when the error happens
## pro uses of for loops: relying on values from previous iteration and nest for loops

# parsing through a list within lapply
# lapply(x = complex_list, function(x) {read(x[['item1']]['deeper_object'])})

# use anonymous functions inside apply
mapply(FUN = function(x, y) {write_csv(x, y)},
       x = ldf_split, y = files_out)

# purrr::map functions ----
# tidyverse version of apply, has specific outputs and plays nice with tidy

# 1. read in our files from files_in
?map
# .x  = vector/list
# .f = function
# . means passthrough with pipe %>% 

l <- map(.x = files_in, .f = read_csv)

# let's say you wanted to adjust an argument in the function
# if you wanted to skip over row 1
l <- map(.x = files_in, .f = read_csv(skip = 1))
# ^ does not work, specifying agruments in the the function requires treating the function as formula
# ~ highlights to the map function that you are going to specify all of your functions' arguments inside the function itself
l <- map(.x = files_in, .f = ~read_csv(x = .x, skip = 1))
# rewrite this correctly with all rows
l <- map(.x = files_in, .f = ~read_csv(x = .x)) ## redundant form of writing the map function that is not necessary
# this creates the same output, and is written more simply
l <- map(.x = files_in, .f = read_csv)

# bind and split .2 and .3
ldf <- bind_rows(l)
ldf_split <- split(ldf, ldf$BASIN_NAME)

# 4. writing the csv
?map2
# map2 expects 2 arguments
map2(ldf_split, files_out, write_csv)
# use walk2
walk2(ldf_split, files_out, write_csv)



# spatial data ------------------------------------------------------------
# https://r4wrds.netlify.app/intermediate/m_advanced_spatial

# sf package
# read in data, look at, transform the crs, spatial manipulations, write data out

# install.packages('<package>')
library(tidyverse)
library(janitor)
library(glue)
library(sf)
library(mapview)
library(tigris)

# read in CA and county boundaries
USStates <- states(cb = T, progress_bar= F)
head(USStates)

ca <- USStates %>% 
  filter(NAME == 'California')
ca

# load up counties
ca_co <- counties('CA', cb = T, progress_bar = F)
ca_co


# load up Sac county shp from local drive
# st_read will read in lots of file types: shp, fgb, geojson, not geoparquet
sac_co <- st_read('data/shp/sac_county_shp/sac_county.shp')
sac_co

# to test if the crs matches, use st_crs
st_crs(sac_co)$epsg

# transform sac_co crs to the other one
sac_co <- st_transform(x = sac_co, crs = st_crs(ca))
st_crs(sac_co) == st_crs(ca)

# a grid
sac_box <- st_make_grid(sac_co, n = 1)

# plot this
plot(ca)
plot(ca$geometry, col = 'gray', border = 'black', lwd = 2)
plot(ca_co$geometry, add = T, border = 'purple', col = NA)
plot(sac_co$geometry, add = T, col = 'skyblue', alpha = .4, lwd = 2)
plot(sac_box, add = T, border = 'orange', col = NA, lwd = 1.4)

# mapping ground water data (points)
paths <- fs::dir_ls(path = 'data/gwl/county/', glob = '*.csv')
paths

# read in all those files
gw_df <- map_df(paths, ~ read.csv(.x))

# can do the same thing and will automaticlly concat df without looping
gw_df <- read_csv(paths)
gw_df

gw_df <- st_as_sf(gw_df, coords = c('LONGITUDE', 'LATITUDE'), 
                  crs = 4326, remove = F) %>% 
  st_transform(crs = st_crs(ca))
gw_df

# preview with mapview
mapview(gw_df, zcol = 'COUNTY_NAME', layer.name = 'GW Stations') +
  mapview(sac_co, legend = F)

# filter gw stations to Sacramento--intersect or tidyverse filter
gw_df %>% 
  filter(COUNTY_NAME == "Sacramento") %>% 
  mapview(zcol = 'COUNTY_NAME', layer.name = 'GW Stations') +
  mapview(sac_co, legend = F)

# filter spatially

# 1) base R indexing
idx <- 1:10
gw_df[idx,]

gw_df[sac_co] # wont work, needs comma
gw_sac_join1 <- gw_df[sac_co,]
dim(gw_df) # original
dim(gw_sac_join1) # new

plot(sac_co$geometry)
plot(gw_sac_join1$geometry, add = T, col = 'blue', pch = 16)

# 2) st_join / st_intersection
# st_join ~ left_join or right_join
gw_sac_join2 <- st_join(gw_df, sac_co, left = F)
gw_sac_join2

# how would you identify the extra columns? 
newcols <- names(gw_sac_join2)
oldcols <- names(gw_df)
newcols[!newcols %in% oldcols] # the new columns added
janitor::compare_df_cols(gw_df, gw_sac_join2)

gw_sac_join3 <- st_intersection(gw_df, sac_co)
gw_sac_join3

# anti join
# wont work! gw_df[!sac_co,]

# this is what lengths does
l <- list(a = 1:5, b = 5:100, c = 3:7)
l
lengths(l)

# a boolean index to refer if points do NOT lie in Sac county
idx <- !lengths(st_intersects(gw_df, sac_co))
gw_df_anti <- gw_df[idx,]
gw_df_anti

mapview(gw_df_anti, col.regions = 'maroon', cex = 3, layer.name = 'stations outside Sac') +
  mapview(sac_co, alpha.regions = 0, color = 'black', lwd = 3, legend = F)


# write out the df, split it into multiple layers based on county name, share as one file. 
# gdb file = ESRI propietary file has multiple layers in 1 file
# open source version = gpkg

co_names <- gw_df$COUNTY_NAME %>% unique
gw_df_split <- gw_df %>% 
  group_split(COUNTY_NAME) %>% 
  set_names(co_names)

map2(.x = gw_df_split, .y = co_names, 
     .f = ~st_write(.x, 
                    dsn = 'data/county_gw_points.gpkg', 
                    layer = glue("{.y}_gw_pts"),
                    delete_layer = T, 
                    quite = T))

st_layers('data/county_gw_points.gpkg')
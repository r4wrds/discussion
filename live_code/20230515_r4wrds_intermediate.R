# 2023-04-17 and 2023-04-19

# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Welcome to the Intermediate Course! (2023-05-15)


# GOOD MORNING

print("Learning R!")

# check R version

R.version


# Setup the project and data: https://www.r4wrds.com/intermediate/#download-data

# downloads the data.zip file to the `data` directory
dir.create("data")
dir.create("scripts")


download.file("https://github.com/r4wrds/r4wrds-data-intermediate/raw/main/data.zip", destfile = "data/data.zip")

# unzip the data:
unzip(zipfile = "data/data.zip")

# if get resulting __MACOSX folder (artifact of unzip), remove:
unlink("__MACOSX", recursive = TRUE)


# Project management --------
## https://www.r4wrds.com/intermediate/m_project_management.html 


# Notes on data: Often we don't store raw data with a project itself, but rather it lives in a shared drive. R Projects should still be your home base, but you'll want to have strategies to read data directly into your project via a link or path to network/cloud

# R profiles ----

# global profile -- every session
# project profile -- project

install.packages('usethis')
usethis::edit_r_profile(scope = "user")

# let's try this with my project rprofile
usethis::edit_r_profile(scope = "project")

# install new packages: cowsay
install.packages("cowsay")
library(cowsay)
say(what = "Hello, moo", by = "cow")

# notes for pasting into the R Profile
url <- "https://gist.githubusercontent.com/JakubPetriska/060958fd744ca34f099e947cd080b540/raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"

install.packages('readr')
library(readr)

# Code above was for R profile
usethis::edit_r_profile(scope = "project")

## PASTE THIS INTO YOUR R PROFILE
library(cowsay)
# take names of shapes from cowsay
animal_names <- names(animals)

url <- "https://gist.githubusercontent.com/JakubPetriska/060958fd744ca34f099e947cd080b540/raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"

quotes <- readr::read_csv(url)

# get random animals and quotes
random_animal <- sample(animal_names, size = 1)
random_quote <- sample(quotes$Quote, size = 1)

say(what = random_quote, by = random_animal)
## END R PROFILE

# R environ ----

Sys.getenv('USER')
# Windows
Sys.getenv('USERNAME')

# This doesn't work
USER

usethis::edit_r_environ()

# use the = not the <- to assign variables new names
SECRET_KEY = "password123"

# we want to access our environ object
Sys.getenv('SECRET_KEY')

usethis::edit_r_environ()

# Interactive Visualization --------------------------

library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)

# if package isn't installed, we can use install.packages("glue")

# copy the webpath to csv
# the url for the Form data 
form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                    "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")

# %>% is a pipe from the dplyr / magrittr package to pass data to the right
# default R pipe looks like this |> (does the same thing, mostly)
# %>% Ctrl + Shift + M is shortcut for pipe 

# read in the data from a URL and clean names
dat <- readr::read_csv(form_data) %>%  
  janitor::clean_names() %>% 
  dplyr::rename(dining_name = 3, dining_address = 4)

# great package for working with dates and times
library(lubridate) # if needed: install.packages("lubridate")

str(dat)

# lubridate has many options for formatting date times
dat$timestamp <- mdy_hms(dat$timestamp)

# filter to just the year we want
dat <- dat %>% 
  mutate( year = year(timestamp), .after = timestamp)

dat$year
table(dat$year, useNA="ifany") # if any NAs in data, will get a count in addition to values
table(dat$first_name) # character data

# make a new dataset for only 2023 records
dat23 <- dat %>% 
  filter(year == 2023)

# double check number of records, does it look right?

dat_geo <- dat23 %>%
  geocode(address = dining_address, method = 'osm', lat = latitude , long = longitude)
# some NAs

# check how many NAs in lat or long?
sum(is.na(dat_geo$latitude)) # 9 NAs

# clean a few errors with "case_when()", a function from dplyr
dat23 <- dat23 %>% 
  mutate( dining_address = case_when(
    dining_name == "Blue Plate" ~ "3218 Mission St, San Francisco, CA 94110",
    grepl("Sala Thai", dining_name) ~ "3101 Zinfandel Dr #148, Rancho Cordova, CA, 95670",
    TRUE ~ dining_address
  ))
  
# geocode using Open Street Map (osm) API because it's free
dat_geo <- dat23 %>%
  geocode(address = dining_address, method = 'osm', lat = latitude , long = longitude)


sum(is.na(dat_geo$latitude)) # 8 NAs

# make into sf object so we can map (using the sf package)
dat_geo <- dat_geo %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  # this uses SF to make data spatial
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = 4326, remove = FALSE)


# make a map!
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


# Iteration -------------------------------------
## Using code in a way that saves you time and simplified the process through processing repeatable tasks
# https://www.r4wrds.com/intermediate/m_iteration.html

# for loop

for(i in 1:10){
  print(i)
}

# for-loop:
# for(<SYMBOL REPRESENTING INDEX> in <A SEQUENCE OF VALUES>){
#       run the code inside the curly brackets
#       as many times as specified in sequence
#       of values, and use the <SYMBOL REPRESENTING INDEX>
#       to index from whatever it is you are iterating over
#}

for(x in 1:8){
  print(paste0("The loop is running through the iteration:", x))
  Sys.sleep(.5)
}

# Doing loops with more than printing

practice_list <- vector('list')
practice_list

# We can use square brackets to fill in items
practice_list[[1]] <- c('filename1', 'filename2', 'filename3')
practice_list[[2]] <- 'hello world'
practice_list

practice_list[[1]][2]

animal_list <- vector('list')

animal_names <- names(cowsay::animals)
animal_names

seq_along(animal_names)

i = 5
# input each animal name as its own item in animal_list
for(i in seq_along(animal_names)){
  animal_list[[i]] <- animal_names[i]
}

animal_list[[7]][1]
animal_names[7]
animal_names
































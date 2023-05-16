# 2023-05-15 and 2023-05-16

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


# DAY 2 (2023-05-16) -------------------

# 2023-05-16

usethis::edit_r_profile(scope = "project")

# for loops:

# for-loop:
# for(<SYMBOL REPRESENTING INDEX> in <A SEQUENCE OF VALUES>){
#       run the code inside the curly brackets
#       as many times as specified in sequence
#       of values, and use the <SYMBOL REPRESENTING INDEX>
#       to index from whatever it is you are iterating over
#}

# lists -- data structures that can hold different items, indexed as listname[[item]]

# reading in multiple csvs
list.files('data/gwl/county', full.names = T)

# to get the data:
# downloads the file (data.zip) to your data directory
download.file("https://github.com/r4wrds/r4wrds-data-intermediate/raw/main/data.zip", destfile = "data/data.zip")

library(tidyverse)
sacramento <- read_csv("data/gwl/county/Sacramento.csv")
# don't want to do this 3 or 58 times

# created an empty list to fill up
files_in <- list.files("data/gwl/county/", full.name = T)
files_in

# creating an empty list, 'soft coding' the length of the list to match the length of the file names (e.g. not having to write in 3)
l <- vector('list', length = length(files_in))
l[[1]]

# seq_along gives us a count range of whatever object we put in
for(i in seq_along(files_in)){
  l[[i]] <- read_csv(files_in[i])
}

# can check the first item in the list
l[[1]]

# we can bind the data frames together out of the list
ldf <- bind_rows(l)

# we want to reassign new dfs based on basin name
ldf
# double check: is this a data frame like we expect?
# it is a type of list and a class of data frame and tibble
typeof(ldf)
class(ldf)

table(ldf$BASIN_NAME)

# subdivide by basin
# save csvs by basin in a new directory

# Create a new directory -- if it already exists, gets a warning
dir.create('data/gwl/basin')
# Error = stop sign. Code did not run and something is wrong
# Warning = yield sign. Code ran but there is something to flag

# split up the data frame into basin categories
l_basin <- split(ldf, ldf$BASIN_NAME)

# paste0 for pasting together text
# glue is a lot like paste, different syntax
#install.packages('glue')
library(glue)

# create a vector of filenames to write out to
names(l_basin)
files_out <- glue("data/gwl/basin/{names(l_basin)}.csv")
files_out

# for loop to actually write the csvs into the basin directory
for(i in seq_along(files_out)){
  write_csv(l_basin[[i]], files_out[i])
}

# [[]] are for lists (nested 1 dimensional) and [] are for vector (or 1 dimensional list)
# e.g. files_out is 1D
# it has 1 D, which is a length of 10
length(files_out)
lengths(files_out)
# it has 1 D, which is a length of 10
length(l_basin)
# but is has nestedness, so the inner 'lengths' can be different
lengths(l_basin)




# repeat for loops --------------------------------------------------------

# why loops: because it saves us time and there are some jobs that are so
# big that we MUST use loops to complete them

# previously used list.files() to return files within a directory. 
# we can also use dir_ls() to return files in a directory. 

# install.packages("fs")

library(fs)
library(tidyverse)

# to be a power use of fs, type "dir_" and "file_" and use the dropdown
# to access the functions you need

# files_in <- list.files("data/gwl/basin/", full.names = TRUE)

# dir_ls() by default reuturn the full name of the file 
files_in <- dir_ls("data/gwl/basin/")

l <- vector("list", length = length(files_in))
l # empty list. rooms in a house that we need to fill

# we fill the rooms with data from files_in! 

b1 <- read_csv(files_in[1])
b2 <- read_csv(files_in[2])
b3 <- read_csv(files_in[3])
b4 <- read_csv(files_in[4])
b5 <- read_csv(files_in[5])
b6 <- read_csv(files_in[6])
b7 <- read_csv(files_in[7])
b8 <- read_csv(files_in[8])
b9 <- read_csv(files_in[9])
b10 <- read_csv(files_in[10])

# how to do this with less lines of code? Because this will NOT scale 
# probably past 100 files. There's a better way!

# cat() stands for "concatenate" which means "combine" and prints whatver
# is inside to the console. "\n" means "new line" and "\n\n" means 2 new
# lines. This is useful when printing to the console and adding new lines. 
# It's like hitting return in a Word processor

for(i in seq_along(files_in)){
  cat("We are on step", i, "\n")
  cat("  Reading file:", basename(files_in[i]), "\n\n")
  l[[i]] <- read_csv(files_in[i])
}



# apply() functions, another way to iterate -------------------------------

library(tidyverse)
library(fs)
library(glue)

files_in <- dir_ls("data/gwl/basin/")

# we are going to work with lapply(), the "l" in lapply stands for "list"
# lapply takes a vector or a list as input, and always returns a list as 
# output. 

# lapply(<vector or a list>, <function to apply across the input>)

# read csvs for each of the 10 basins. turn 5 lines of code (loop) into 1.
l <- lapply(files_in, read_csv)
length(l)
names(l)
# l$ for tab autocomplete to see your list elements
l[[1]] # same as l$<name of first list element>

# bind csvs and 
ldf <- bind_rows(l)
unique(ldf$BASIN_NAME) # we have 10 unique basin names in this data.frame

# split the dataframe by SITE_CODE
unique(ldf$SITE_CODE) # 717 unique site codes
ldf <- split(ldf, ldf$SITE_CODE)
ldf[[1]]
length(ldf)
names(ldf)

# write to csv with apply

# need paths to write to
files_out <- glue("data/gwl/site_code/{names(ldf)}.csv")
dir_create("data/gwl/site_code")
# mapply ("m" stands for multiple apply)
# mapply(<function>, arguments)
mapply(write_csv, ldf, files_out)



# map() - a third an final way to iterate ---------------------------------

# map synax:
# map(<vector or list to map over>, ~function(.x))
# ~ is the start of the function
# .x is where you would put i if writing a loop

library(tidyverse)
library(fs)
library(glue)

# define files in to read
files_in <- dir_ls("data/gwl/basin/")

# read the files with map()
l <- map(files_in, ~read_csv(.x))

# bind rows
ldf <- bind_rows(l)

# split by site cod using group_split
ldf <- split(ldf, ldf$SITE_CODE)   # base R
# ldf <- group_split(ldf, SITE_CODE) # tidyverse way
names(ldf)

# write the ldf list of dataframes to csv files in files_out
files_out <- glue("data/gwl/site_code/{names(ldf)}.csv")
# walk2(<list or vector 1>, <list of vector 2>, ~function(.x, .y))
# .x refers to list or vector 1
# .y refers to list or vector 2

walk2(ldf, files_out, ~write_csv(.x, .y)) # silent: does not print
map2(ldf, files_out, ~write_csv(.x, .y))  # verbose: prints to console

# same as...
# write_csv(ldf[[1]], files_out[1])
# write_csv(ldf[[2]], files_out[2])
# write_csv(ldf[[3]], files_out[3])
# ...
# write_csv(ldf[[717]], files_out[717])



# piping ------------------------------------------------------------------

df <- read_csv(dir_ls("data/gwl/basin/"))

# why pipe? We read in English, from left to right. 

# tell me which of the equivalent statements is easier to read
class(length(unique(df$BASIN_NAME))) # nesting
df$BASIN_NAME %>% unique() %>% length() %>% class() # piping

# %>% = "then do this"


# piping with map ---------------------------------------------------------


library(tidyverse)
library(fs)
library(glue)

# define files in to read
files_in  <- dir_ls("data/gwl/basin/")
files_out <- glue("data/gwl/basin2/{basename(files_in)}")

dir_create("data/gwl/basin2")
# read in 10 files, bind into 1 dataframe, split into 717 site code
# dataframes in a list, then write 717 site code dataframes. In 3 lines of code. 

# map_df() same as map() %>% bind_rows() same as bind_rows(map())
map_df(files_in, ~read_csv(.x)) %>% 
  group_split(BASIN_NAME) %>% 
  # <add a new column to the dataframe> %>% # we can add more steps in here
  # <fit a liner model> %>% 
  walk2(files_out, ~write_csv(.x, .y))

# Mapmaking and spatial data --------------------------
# https://www.r4wrds.com/intermediate/m_advanced_spatial.html

# GENERAL PACKAGES
library(tidyverse) # data wrangling & viz
library(purrr) # iteration
library(janitor) # name cleaning
library(glue) # pasting stuff together

# SPATIAL PACKAGES
library(sf) # analysis tools
library(mapview)  # interactive maps!
mapviewOptions(fgb = FALSE) # to save interactive maps


# a new package with spatial boundary data for the US
# there are many (see tidycensus or rnaturalearth)
install.packages("tigris")
library(tigris)

# get sf CA boundary...note we need to filter to CA
ca <- states(cb=TRUE) %>% 
  dplyr::filter(STUSPS == "CA")

# get sf object of counties for CA
ca_co <- counties("CA", cb = TRUE)

# reading in a shape file our data
sac_co <- st_read("data/shp/sac_county_shp/sac_county.shp")

# coord ref system -- what is this? tablecloth vs. orange peel analogy (flat vs spherical) with different precision of measurements at different areas
st_crs(sac_co)$epsg
st_crs(ca_co)$epsg

# make sure these crs match between the data
sac_co <- st_transform(sac_co, st_crs(ca_co))
st_crs(sac_co) == st_crs(ca_co)

# read in groundwater data monitoring
gw_sac <- read_csv('data/gwl/county/Sacramento.csv')
str(gw_sac)

?st_as_sf
gw_sac <- st_as_sf(gw_sac, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(., st_crs(ca_co))

mapview(sac_co, legend = FALSE) +
  mapview(gw_sac, layer.name = "GW Stations")
?read.csv2































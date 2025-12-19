# Live Code ------------------------------------------

#################################################################
# DAY 1 --------------------------------------------------------
#################################################################

8 + 5

# Remember, to execute or 'run' code in a script, use Ctrl + Enter
library(here)

# Getting an error? there is no package called 'here'
# Need to install the package called here with this function:
install.packages("here")

# Note: we only need to install R packages once
# Try to load in the library again 
library(here)
# Needs to happen for each session

# Warnings = message that something might not be as expected / something you should know but not necessarily take action 
# Error = message to say that something is wrong. The code did not run and you need to correct something

# These is a function that should return, in your console, you full "absolute" file path
here()
# functions = how R gets work done; those we've seen include: install.packages, dir.create, download.file, here, library
# functions have parentheses
# arguments go inside a function

# Let's define a path in our data by reading in a CSV file -- this provides the full path for a target CSV
here("data/nwis_sites_american_river.csv")

# Read it in with read.csv()
read.csv(here("data/nwis_sites_american_river.csv"))

# This could also work -- several ways to accomplish the same task in R
read.csv("data/nwis_sites_american_river.csv")
# Do we really need to there here package?

read.csv("data/nwis_sites_american_river.csv")

# Did anyone have to do 'data/data/nwis_...'?
read.csv(here("data/data/nwis_sites_american_river.csv"))
# Why? dragging and dropping files


# Reading in data ----

# Question mark in front opens the help file for that function
?read.csv()

# Read the csv
read.csv("data/gwl/stations.csv")
# Arguments in functions, do not need to specify names of arguments if they are added in the right order
# this also works, by being specific about which argument you are answering
read.csv(file = "data/gwl/stations.csv")

# Assign a new object with the station datd
stations <- read.csv(file = "data/gwl/stations.csv")

# read.csv versus read_csv -- the first is base R's function for reading in CSVs, and the second is the 'tidyverse's' function for reading in CSVs

# to use read_csv, load in the 'readr' package -- part of the tidyverse
install.packages('readr') # this should be unnecessary for most of you
library(readr) # can also check in your packages tab
stations2 <- read_csv("data/gwl/stations.csv")

# Remembering to run each line of code as you write it
# Green checks: Do you have 2 objects called station and station2?

# starting to analyze data:
# structure
str(stations)
nrow(stations)
ncol(stations)

# read_csv creates a special data frame that is a little 'fancier', but does not really make much of a difference for our purposes
str(stations2)

# stations...
View(stations)

# indexing a data frame, extracting information along its x and y dimensions
# dfobject[x,y] # x = rows and y = columns, square brackets to index

# Extract first row and all columns
stations[1,]
# Extract first column and all rows
stations[,1]
# Be specific to first row and first column
stations[1,1]
# Specify more than one location, a colon indicates range or sequence
stations[1:10,]

# More common way to index column, is by using the $ -- shortcute to identifying a single column/variable in a dataframe
stations[,1]
# It is equivalent to the line above
stations$STN_ID

# table function for summarizing categorical data 
table(stations$BASIN_NAME)

# Reading in Excel files ----
library(readxl)
# If this does not work, install it
install.packages('readxl')

# read_xlsx only read in one sheet at a time
ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx")
str(ces)

?read_xlsx

# Other important arguments in the xlsx functions: skip, sheet
# When you have multiple arguments in a function, separate them by commas
metadata <- read_xlsx(path = "data/calenviroscreen/ces3results.xlsx", sheet = 2, skip = 6)

# You have to read in 1 sheet at a time, and each sheet should be assigned to a new object

# Shp files ---

library(sf)
sac_county <- st_read('data/shp/sac/sac_county.shp')
str(sac_county)

# For demonstration, easier to see that data by plotting, so here is a preview of plotting for day 2
library(ggplot2)
ggplot(sac_county) + geom_sf()

#################################################################
# DAY 2 --------------------------------------------------------
#################################################################

# WELCOME BACK!

# Start our session by loading in a library, which has the functions we want to use today
library(tidyverse)

# Read in the stations data and assign to the an object called 'stations'
stations <- read.csv('data/gwl/stations.csv')

# Check that your Rproj (from your working directory) has the files you expect
list.files()

# Remember for those you who have nested data folder 'data/gwl/stations.csv', 'data/data/gwl/stations.csv', that path might work

# Module 6 -----------

# Structure function
str(stations)


# OBJECT CLASSES ----
## 4 main classes in R: character (chr), numeric (num, int), logical, factor

## 1. characters -----
# Class function reports out the object class
class(stations$SITE_CODE)
# not a machine readable number, anything in quotes
river_name <- "Sacramento River"
class(river_name)

# Anything inside the quotes, even numbers, are still of class character
class("7")

# *vectors* are combined data, 'concatenated' because we use the c() function
# combine elements/objects, separated by commas, inside the c() function

# create a vector of reach names 
reach <- c("Wild Rapids", "Dry Creek", "Raging Waters")
class(reach)

# 2. numeric = machine-readable numbers ----
str(stations)
class(stations$LATITUDE)

flow <- c(0,57,128)
class(flow)

# When to use quotes or not? Should not need to use them with numbers
# But if you tried to mix object types, R will get confused / choose a default
## mixed_vector <- c(7, 10, 76t) # commenting out so that R doesn't keep trying to run and get annoyed

# What if I create a vector of different classes?
mixed_vector <- c(7, 10, "76t")
class(mixed_vector) # R will default mixed class vectors to characters

# *vectorization* = R will run a function over each element in the vector automatically
flow
flow_gpm <- flow * 448.83
flow_gpm

# 3. logical ---- 
# Boolean, special class for TRUE or FALSE

# logical vector: is the reach dry or not during observation?
dry <- c(TRUE, FALSE, TRUE)
class(dry)


# 4. factors ----
# The most complicated of the data classes: categorical data, written as characters but have an underlying numeric order

date <- c("July", "January", "February")
class(date) # for now, a character

# use the factor function to coerce to the factor class
# run it on the existing object 'date' and overwrite itself
date <- factor(date)
class(date)

# Defaults to alphabetization -- without telling what order, it will go alpha.
levels(date)

# re write once more
date <- factor(date, levels = c("January", "February", "July"))
class(date)
levels(date)

# Date types ----
# Great packages for dates: lubridate
# Built in 'constant' objects in R that identify the factor of these special objects
# these are still character object classes but they provide months in ready-made order to specify as levels
month.name
class(month.name)
month.abb
class(month.abb)

# pre-made objects
pi
letters

# lubridate demo for date
library(lubridate)
lubridate::year("10-12-1995") # doesn't work
lubridate::year("1995-10-12") # works

# Data structures ----
## 3 structures: vectors, lists (combine vectors of different lengths), and data frames

# create a data frame with the data.frame() function
riv <- data.frame(reach, flow, dry, date)
riv

# Navigating within a data frame
riv$reach

nrow(riv)

# Can add names following the $
riv$tech <- c("Liza", "Ryan")

riv$tech <- c("Liza", "Ryan", "Rich")
riv

# Check where functions are coming from
class

riv
date

## Spreadsheet Tips

# Data Wrangling ----------------------------------------------------------

# add section breaks in your code with Ctrl + Shift + R

library(tidyverse)

# read in our stations data
stations <- read.csv("data/gwl/stations.csv")

## filter ------------------------------------------------------------------

# filter function in dplyr
class(stations)
str(stations)

# filter to stations in Sacramento county
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento")

# sometimes if you get an error
stations_sac <- dplyr::filter(stations, COUNTY_NAME == "Sacramento")

table(stations$COUNTY_NAME) # good check character/factor type data

# stations_sac <- filter(stations, COUNTY_NAME = "Sacramento") # gives useful error message to fix
# stations_sac <- filter(stations, COUNT_NAME == "Sacramento") # object not found one of most common errors

# table is handy way to count categories in a column (variable)
table(stations_sac$WELL_USE)

stations_sac <- filter(stations, COUNTY_NAME == "Sacramento", 
                       WELL_USE == "Residential")

# can filter to multiple counties
sep_counties <- c("Sacramento", "sacramento", "Placer", "El Dorado")

stations_multcounties <- filter(stations, COUNTY_NAME %in% sep_counties)

table(stations_multcounties$COUNTY_NAME)

# ignore or exclude data, we need to "negate" using !
stations_trim <- filter(stations, !COUNTY_NAME %in% c("Yolo")) # can add multiple values
stations_trim <- filter(stations, !COUNTY_NAME == "Yolo") # can only use one value
stations_trim <- filter(stations, COUNTY_NAME != "Yolo") # can only use one value

table(stations_trim$COUNTY_NAME)

## select ------------

# select specific columns to keep
stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME))
names(stations_sel1)

# to remove a column or drop from dataframe
stations_sel2 <- select(stations, -c(LATITUDE:BASIN_NAME, COUNTY_NAME, WCR_NO))

# subfunctions to help search or get data you want
stations_sel3 <- select(stations, starts_with("WELL"), contains("DEP"))
stations_sel3 <- select(stations, starts_with("W"), contains("NAME"))
stations_sel3 <- select(stations, starts_with("W") | contains("NAME"))
# above is an either/or using arguments separated by ,  or the |

# to specify AND, means it must match BOTH arguments in select
stations_sel3 <- select(stations, starts_with("W") & contains("NAME"))

# PIPE --------------------------------

# what is the pipe: %>% or |>
# type Ctrl + Shift + M to get a pipe symbol: %>% 

# filter and select with pipe:
stations_multcounty1 <- filter(stations, COUNTY_NAME %in% c("Sacramento", "Placer")) %>% # step one is filter then pass with pipe to next step
  select(starts_with("W"), contains("NAME"), contains("ID"))

# to remove objects in environment can use rm(): rm(stations_multcounties)

# build your piping steps one by one

stations_multcounty1 <- filter(stations, COUNTY_NAME %in% c("Sacramento", "Placer")) %>% # step one is filter then pass with pipe to next step
  select(starts_with("W"), contains("NAME"), contains("ID")) %>% 
  # add another function to rename columns, needs NEW_NAME = OLD_NAME
  rename(station_id = STN_ID)

# can comment out in front of pipe to test code

# MUTATE ---------

# adding a new column to existing data frame
stations_mutate1 <- stations %>% # can start with data first and pipe to functions
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048)
# can add additional columns in same call to function
## WELLDEPTH_in = WELL_DEPTH_m * inches )

# defaults to putting new columns at the end, but we can specify or rearrange where they go

# can use .before or .after
stations_mutate1 <- stations %>% # can start with data first and pipe to functions
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048, .after = WELL_DEPTH)

# GGPLOT2 ----------------------

# always need data= and always need aes(x and y)
ggplot(data=stations_mutate1) +
  geom_point(aes(x=STN_ID, y=WELL_DEPTH))

# put all in the ggplot() they apply all layers moving forward unless otherwise specified
ggplot(data=stations_mutate1, aes(x=STN_ID, y=WELL_DEPTH)) +
  geom_point() #+
#geom_line() +

# more flexibility to specify different data sets or aes with each layer
ggplot() +
  geom_point(data=stations_mutate1, aes(x=STN_ID, y=WELL_DEPTH)) +
  geom_point(data=stations_mutate1, aes(x=STN_ID, y=WELL_DEPTH_m), color="maroon")

# make some plot adjustments
ggplot() +
  geom_point(data=stations_mutate1, aes(x=STN_ID, y=WELL_DEPTH), color="cyan4", alpha=0.5) +
  geom_point(data=stations_mutate1, aes(x=STN_ID, y=WELL_DEPTH_m), color="maroon", pch=21, alpha=0.7)

# built in themes:
ggplot() +
  geom_point(data=stations_mutate1, aes(x=STN_ID, y=WELL_DEPTH), color="cyan4", alpha=0.5) +
  geom_point(data=stations_mutate1, aes(x=STN_ID, y=WELL_DEPTH_m), color="maroon", pch=21, alpha=0.7) +
  theme_bw() +
  labs(title = "Great GGPLOT", subtitle = "Super Subtitle", x="Station ID", y="Well Depths")

# filtering NAs
stations_mutate1 %>% 
  filter(!is.na(WELL_DEPTH)) %>% # filter out the NAs, to remove Warning message
  ggplot() +
  geom_point(aes(x=STN_ID, y=WELL_DEPTH), color="cyan4", alpha=0.5) +
  geom_point(aes(x=STN_ID, y=WELL_DEPTH_m), color="maroon", pch=21, alpha=0.7) +
  theme_bw() +
  labs(title = "Great GGPLOT", subtitle = "Super Subtitle", x="Station ID", y="Well Depths")

## GROUP_BY -----------

n_by_county <- stations %>% 
  group_by(COUNTY_NAME) %>% 
  count()










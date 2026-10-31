# Live Code for R4WRDS Course: Oct 26/28th 2026


# 2026-10-26 --------------------------------------------------------------

# Making sure to start with a blank slate: we always want to start with a blank slate.
# In RStudio go to Tools > Global Options > General (on left)		
# Make sure to uncheck the “Restore .RData into the workspace at startup” and change “Save workspace to .RData on exit” to Never.
# To open a directory/folder from inside of RStudio, find the Files Pane, then click the little blue gear wheel with “More” > and scroll down and click on Show Folder in New Window
# As long as we are in an RStudio project, this will open a new directory window for the location of your RStudio project (or the folder within the project you selected in the Files Panel).

x <- 1


# Relative Paths ----------------------------------------------------------


# the importance of relative paths
path <- 'data/nwis_sites_american_river.csv' # <- is equal to =
read.csv(file = path)

# name conventions with dates: 20261026_file.csv

# I can access other files with tab complete
path <- "data/gwl/county/El Dorado.csv"


# Downloading Files -------------------------------------------------------

# downloads the file (data.zip) to your data directory
download.file("https://github.com/r4wrds/r4wrds-data-intro/raw/main/data.zip", destfile = "data/data_copy.zip")

# unzip the datasets for the course:
unzip(zipfile = "data/data_copy.zip")



# Import and Export -------------------------------------------------------

# this is the lesson 5: https://r4wrds.netlify.app/intro/m_importing_and_exporting_data

## Function Overview ------------------------------------------------------------------

sum(1,5,5)

seq(0, 1, length.out=11)
seq(0, 10, 1)
seq(from=0, to=10, by=1) # knowing argument order matters
seq(0, 1, 10) # if we don't specify, sometimes we won't notice if no error

sum(1:10)

## Import From CSV ---------------------------------------------------------

library(tidyverse)
library(readr) # part of tidyverse

# read a csv
stations <- read_csv("data/gwl/stations.csv")
# can use rainbow parentheses with Tools > Global Options > Code > scroll down and select "Use Rainbow Parentheses"
head(stations) # gives first few rows

library(tidyverse)
glimpse(stations)

# read straight from a URL
stations <- read_csv("https://github.com/r4wrds/r4wrds/blob/main/intro/data/gwl/stations.csv?raw=true")

nrow(stations)
ncol(stations)


# Data Frames -------------------------------------------------------------

# what is a data frame?
# tidy data concept: every variable/col is the same type of data
# every row is an observation of that variable

# we can view data:
View(stations)

stations$WELL_TYPE[1]
stations$WELL_TYPE[1:10]

# brackets can access elements (cells) or can access rows and columns
# when using brackets, always ROWS then COLUMNS
stations[1,5]

# function to count unique variables
table(stations$WELL_TYPE)

summary(stations$LATITUDE)



# Read Excel --------------------------------------------------------------

library(readxl) # install.packages("readxl")

# reads the first tab by default
ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx") 
glimpse(ces)
# NAs are causing trouble

ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx", na = c('NA', 'n/a')) 

# read a different tab by number
ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx",sheet = 2, na = c('NA', 'n/a')) 

# read a different tab by name
ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx",sheet = "Demographic profile", na = c('NA', 'n/a')) 
# will assign random col name if col is blank ("...8")


# Reading Shapefile/Spatial Data ------------------------------------------

library(sf) # get a message about spatial data pieces linked
# the simple features: sf package is great
# navigate to Files Pane and data/shp/sac:
# should have 5 files:README.txt,  and 4 sac_county.dbf/prj/shp/shx

sac_county <- st_read("data/shp/sac/sac_county.shp")

# make a map of our spatial data

# make sure ggplot2 library (part of tidyverse)
library(ggplot2)
ggplot(sac_county) + geom_sf()

# can also use base plot function
plot(sac_county)
plot(sac_county$geometry) # spatial data only


# RDA and RDS -------------------------------------------------------------

# r specific data 
# RData (rda)
stations <- read_rds("data/gwl/stations.rds") 
# RDA files can store multiple objects
#save(ces, sac_county, stations,file="PATH/TO/FILE.RDA")
# RDS is for only one specific object



# Export Data -------------------------------------------------------------

# CSV
write_csv(stations, "data_output/my_stations.csv")

# SHP
st_write(sac_county, "data_output/sac_county.shp")


# FOR EXCEL: we need a package, recommend "writexl": https://docs.ropensci.org/writexl/
# library(writexl)


# Data Classes ------------------------------------------------------------

# Lesson 7: https://r4wrds.netlify.app/intro/m_data_structures
class(stations)

class(stations$SITE_CODE) # vector of character data
# dataframes require all vectors to be same length
# atomic vector: same type of data
# logical, numeric (integer or double), factor (categorical/ordinal), character

# 2026-10-28 --------------------------------------------------------------

# Setting up ------------------------------------------

# Let's return to your RProject: RStudio > Open Project > intro_r4wrds

# Make sure your work space is clear: 
## If you have data already loaded, got to: RStudio Tools > Global Options > General (on left)		
## Uncheck the “Restore .RData into the workspace at startup” and change “Save workspace to .RData on exit” to Never.

# Check you you are in the right working directory:
getwd() # Should see a path that ends in intro_r4wrds

# Review  --------------------------------------------

## Cheat sheets -------------------------------------------- 

## Data import: https://opensource.posit.co/resources/cheatsheets/data-import/
## Data wrangling: https://opensource.posit.co/resources/cheatsheets/data-transformation/ 
## Data visualization: https://opensource.posit.co/resources/cheatsheets/data-visualization/
## Spatial with sf: https://rstudio.github.io/cheatsheets/sf.pdf 

## Unanswered questions ------------------------------------

## R Shiny = a framework for creating web applications (i.e. interactive dashboards)
## More for intermediate to advanced learners:
## https://r4wrds.netlify.app/intermediate/m_simple_shiny 

## < RL in Excel, best practice? Suggestion: 0 (keep the measurements numeric)
## If there is an importance in the distinction between 0 and <RL, add column
## titled 'below_RL', with TRUE or FALSE

## Reading in different file types -------------------------
## CSV files 
library(readr)
stations <- read_csv("data/gwl/stations.csv")
head(stations)

## Excel files 
install.packages("readxl")
library(readxl) # install.packages("readxl")
# reads the first tab by default
ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx") 
head(ces)

## RDS files 
# R-specific data (you make a choice to save something as an RDS) 
stations_rds <- read_rds("data/gwl/stations.rds") 
head(stations_rds)


## Shape files (spatial data from ArcGIS)
library(sf) 
# navigate to Files Pane and data/shp/sac:
# should have 5 files:README.txt,  and 4 sac_county.dbf/prj/shp/shx
sac_county <- st_read("data/shp/sac/sac_county.shp")
head(sac_county)


# MODULE 7: Data structures and classes ------------------------

# 4 object classes: characters, numeric, logical, factor
?sum
?paste

# Characters ----
river_name <- "Sacramento River"
class(river_name)
## anything with quotation marks is a character
class("7")

# we can combine data into a vector using the c() -- stand for concatenate or combine
# vectors must be of the same class

reach <- c("Wild Rapids", "Dry Creek", "Raging Water")
class(reach)

# Numeric ----
# make a vector of numbers
flow <- c(0, 57, 128)
class(flow)

# feature of vectors: when we run functions on them, the function is *vectorized* -- the function applies across the whole vector

flow_gpm <- flow * 448.83
flow_gpm
# Want your comments to wrap? Tools > Global Options > Code > Display > Soft wrap source files

# Logical ----
## Boolean -- special case for: TRUE/FALSE, or T/F

dry <- c(TRUE, FALSE, FALSE)
class(dry)

dry <- c(T, F, FALSE)
class(dry)

# R does see the 0/1 underneat the F/T
sum(dry)
# at the core, T/F are just 0/1
# but these do stay as a numeric class
# dry <- c(1, 0, 0)
# class(dry)

# this only works for these special words: TRUE FALSE
# test <- c(YES, NO, NO)

# Factors ----
## special case: characters, with an underlying order (have a numeric underside)

date <- c("July", "January", "February")
class(date)
## to make it a factor (e.g. to give it an order), we use the factor function

date <- factor(c("July", "January", "February"))
class(date)
date

# by default, factor assigns levels of a character vector by alphabetical order
# assign your own order with th elevels argument 

date <- factor(c("July", "January", "February"), 
               levels = c("January", "February", "July"))
date

# 3 DATA STRUCTURES -----
## vectors, lists, data frames

# 1. Vectors ----
## essentially a column of elements, all the same class
reach
flow
dry
date

# functions to inspect vectors
length(flow)

# 2. List ----
## collection of vectors of **different** lengths

single_string <- "A character object with a length of 1"
class(single_string)
length(single_string)

l <- list(dry, reach, flow, single_string)

# explore the list with indexing [] [[]]
## index the first item in the list
l[[1]]
l[[2]]

# 3. Data frames ----
# a collection of vectors that are all the same length -- spreadsheet

riv <- data.frame(reach, date, dry, flow)
riv

# index column with $ after the data frame name
riv$flow

# inspecting data frames
str(riv)

# What is want to a new column, and what about size?
# R will recycle the string to the length of the data frame
riv$tech <- "Liza"
riv$tech

# R will error if try to align it with a vector of different length
riv$tech <- c("Liza", "Ryan")
riv$tech <- c("Liza", "Ryan", "Liza")
riv

# ^ All of this is in 'base R' -- no packages need, all 'native' to R language

# Missing values ------

z <- c(2, 4, 10, NA, 5)
class(z)

# function on numeric vector with NA
mean(z)
?mean

# default is to include NAs, this is true for most arithmetic functions
mean(z, na.rm = TRUE)

# 2 important NA features
## na.rm: argument inside many functions, often needs to be toggled to T
## is.na(): function that logically evaluates: Is there an NA in this object?

is.na(z)
table(is.na(z))

# ------------------------------------------------------------
# MODULE 8: DATA WRANGLING
# -----------------------------------------------------------

library(tidyverse)
# primarily using dplyr for now

# read in some data = load data into current project (into the 'environment') so that we can use it
stations <- read_csv("data/gwl/stations.csv")
str(stations)

# Filter ----
# Reducing rows based on a condition
# want only Sac counties
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento")
nrow(stations_sac)
stations_sac$COUNTY_NAME

# Conditions:
## == means "equals to" ; whereas a single single = is about assigning an argument
## | means "or"
## %in% means "contains"
## use ! to negate any evaluation 

table(stations$WELL_USE)

# for filter: , means 'and'
stations_sac_res <- filter(stations, COUNTY_NAME == "Sacramento", WELL_USE == "Residential")

# Let's say we want several counties
stations_multcounties <- filter(stations, COUNTY_NAME == "Sacramento" |
                                  COUNTY_NAME == "Placer" | COUNTY_NAME == "El Dorado")
nrow(stations_multcounties)

# %in% contains can be helpful
counties_of_int <- c('Sacramento', 'Placer', 'El Dorado')

stations_multcounties2 <- filter(stations, COUNTY_NAME %in% counties_of_int)
nrow(stations_multcounties2)
identical(stations_multcounties, stations_multcounties2)

# ! negate conditions
## Condition: give me all counties that are NOT Yolo
## It will reverse any logical claim
stations_trim <- filter(stations, COUNTY_NAME != 'Yolo')
nrow(stations_trim)

# does not work because filter returns a data frame, not a logical value
stations_trim_test <- !filter(stations, COUNTY_NAME == 'Yolo')
?identical
?filter

## R understands " and ' the same
## Niche time it does matter: if you have a string with an apostrophe

#string_obj <- 'Let's get pizza'
string_obj <- "Let's get pizza"
string_obj <- 'She said "hello"'


# 15 minute break: Return at 15 till

# Select -- reduce column
colnames(stations)

# List the names we want as a vector -- this vector does not need quotes, contrary to everything we just discussed earlier (tidyverse rules)
stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME))
head(stations_sel1)

# Use range : symbol to pull out a range of columns
stations_sel2 <- select(stations, c(LATITUDE:BASIN_NAME))
head(stations_sel2)

# Additional functions that can help you pull out certain features of column
## starts_with() or contains()
## , here is more like a listing feature, not AND
stations_sel3 <- select(stations, starts_with("W"), contains("NAME"))
head(stations_sel3)

# Pipe %>% symbol or |> symbol (they are equivalent)
## Connect/combine several steps into a 'pipeline'; move data through several functions

# County Sacramento, columns that have name
# start your pipeline with the dataframe
# Keystroke: Ctrl + Shift + M
piped_data <- stations |> 
  filter(COUNTY_NAME == "Sacramento") |> 
  select(contains("NAME")) 
head(piped_data)

# Why does contains only look at column names?
?contains
stations |> 
  # select forces contains to look only at column names, not all the text in the data frame
  select(contains('Sacramento'))

# mutate = create a new column
# always need to re-write or overwrite the data for the column to stay
stations_newcol <- stations |> 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048)
str(stations_newcol)

stations |> 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048)

colnames(stations)

# --------------------------------------------------------
# MODULE 6 GGPLOT2 and VISUALIZATION
# --------------------------------------------------------

# Starting fresh, re load in packages and data
library(tidyverse)
gwl <- read_csv('data/gwl/gwl.csv')

str(gwl)

# Layer 1 of ggplot
ggplot()
# Additional layers:
## data -- where are we getting the info
## mappings -- telling R which parts of the data to match with which plot features (e.g. x and y coordinates)
## geoms -- shape(s) that you want your data to take
## + to add a whole suite of features

# Task: Viz depth of groundwater at this site over time. MSMT_DATE x axis, GSE_WSE water surface elevation data y axis; data could be a line, could be points

## Using the plus sign to add layers
ggplot(data = gwl) +
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE))

# Experiment with different geoms
## alpha is transparenct, on a gradient 0-1
p <- ggplot(data = gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = .2,
             color = "violet") +
  # theme will change the background of the plot
  theme_linedraw() +
  # labs() function where you can add labels
  labs(x = 'Measurement date', y = 'Water surface elevation',
       title = 'Water surface elevation over time')

p +
  labs(caption = 'Created in Intro R4WRDS')

# What about using categorical data?
table(gwl$WELL_USE)
str(gwl)

ggplot(data = gwl) +
  geom_bar(aes(x = COUNTY_NAME))

# histogram for distribution of data

ggplot(gwl) +
  geom_histogram(aes(GSE_WSE))


# Order of data on axis
gwl$WELL_USE[400:500] <- 'Residental'
gwl$WELL_USE[1000:1100] <- 'Private'
table(gwl$WELL_USE)

ggplot(gwl) +
  geom_bar(aes(x = factor(WELL_USE, levels = c('Residental', 'Unknown', 'Private'))))

# use ggplot with spatial data (sf)

library(sf)
sac <- st_read('data/shp/sac/sac_county.shp')
stations <- read_csv('data/gwl/stations_sac.csv')

# important step: making sure CRS match to what we expect
# convert stations into an sf object, specifying the crs

stations <- st_as_sf(stations,
                     coords = c('LONGITUDE', 'LATITUDE'),
                     crs = 4269,
                     remove = FALSE)
class(stations)

# does sac have the same crs?
st_crs(sac)

sac <- st_transform(sac, crs = 4269)

identical(st_crs(stations), st_crs(sac))

spatial_p <- ggplot() +
  # sac is an outline
  geom_sf(data = sac) +
  # stations are points
  geom_sf(data = stations, color = 'blue', alpha = 0.5)

spatial_p +
  labs(x = 'Longitude (NAD83)', y = 'Latitude',
       title = 'Groundwater monitoring stations',
       subtitle = 'Sacramento County') +
  theme_minimal()

install.packages('ggthemes')
library(ggthemes)

spatial_p +
  labs(x = 'Longitude (NAD83)', y = 'Latitude',
       title = 'Groundwater monitoring stations',
       subtitle = 'Sacramento County') +
  theme_wsj()











# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: 
# https://github.com/r4wrds/discussion/tree/main/live_code

# Getting Started ---------------------------------------------------------



# Welcome! 

print("Welcome!")

print("Good Morning")

# Welcome welcome!

# this is a comment 4+5
4 + 5

4 + 8

4 / 2 # can add a comment at the end of a line

# this is basic calculator stuff


# Storing Variables in the Environment ------------------------------------

# this is a section, and I can hide a section, or use a section to navigate

# a variable in R can't start with a number
# a variable in R must not have spaces
# 1 object 
# object_1 # this is good
# object-1 # this won't work!

# assign the value 4 to the variable stream
stream <- 4 
stream<-4 # same as above 

pebble <- 8 

pebble2 <- 7

# assignment is <- and shortcut Alt + -
habitat <- stream + pebble 
# add a comment here
habitat

# how to make a variable or change it
Habitat <- stream + stream

# be consistent, avoid mixing upper and lower case names in variables
# better
#habitat_1
#habitat_2

# not better
#Hab_1
#hab2


# Packages ----------------------------------------------------------------


# to load a package
library(tidyverse)

# :: is a way to access a function in a package
# dplyr::filter()
dplyr::filter()
# dplyr::



# Getting Help ------------------------------------------------------------

# getting help
help("filter")

?filter # looks specifically for the "filter" function

??filter # fuzzy search...looks for anything with the word filter


# Setting R PRoj ----------------------------------------------------------

# make a directory
dir.create("data")

# download data
# downloads the file (data.zip) to your data directory
download.file("https://github.com/r4wrds/r4wrds-data-intro/raw/main/data.zip", destfile = "data/data.zip")

# unzip the data we need for the course (tab autocomplete INSIDE the quotations is your friend!)
unzip(zipfile = "data/data.zip")



# File Paths --------------------------------------------------------------

# absolute file paths: always break on another persons computer
# C:/myname/documents/more_folders/on/on/on

# relative file path
# "data/data.zip"

# quick way to check your working directory in R:
getwd()

# setwd() # avoid using

# here package
# install with
install.packages("here") # if you get an error doesn't exist, try this

library(here)

# here makes a complete path to a file
here("data", "nwis_sites_american_river.csv")

# these lines are all the exact same thing, with slight variations in 
# how the path is called:

# here it allows us to read that file in safely
read.csv(here("data", "nwis_sites_american_river.csv"))

read.csv(here("data/nwis_sites_american_river.csv"))

# this is the same with a relative file path
read.csv("data/nwis_sites_american_river.csv")

# Import Export -----------------------------------------------------------


library(readr)

# read.csv and read_csv
stations <- read_csv(file = "data/gwl/stations.csv")

# view the top 6 rows
head(stations)

# print the top 10 rows to the console
head(stations, 10)

# read the "stations" csv from the Github URL
stations <- read_csv("https://github.com/r4wrds/r4wrds/blob/main/intro/data/gwl/stations.csv?raw=true")

# return the number of rows or columns in a dataframe
nrow(stations)
ncol(stations)

# nrow and ncol in one go!
dim(stations)

# check the "class" of an object
class(stations)

# view an interactive table of a dataframe
View(stations)

# access a specific column by NAME with the $ operator
stations$STN_ID
stations$WELL_NAME

# use bracket notation [] to subset values from a vector (think column)
stations$STN_ID[1]

# use bracket notation with the colon operator to pull out a sequence
stations$STN_ID[1:10]
stations$WELL_TYPE[5:9]

# aside on sort, which arranges a vector from low to high
sort(c(4, 2, 1, 7))
sort(stations$LATITUDE[1:5])

# use the table() function to return counts of unique values in a vector
table(stations$WELL_TYPE)

# calculate proportions of each well type
table(stations$WELL_TYPE) / nrow(stations)


# Read Excel files into R -------------------------------------------------

# install.packages("readxl")
library(readxl)
library(here)

# read an excel file from a relative file path with here, and assign
# it to a variable called ces
fp <- here("data/calenviroscreen/ces3results.xlsx")
ces <- read_xlsx(fp)

# print the first 10 rows
head(ces, 10)
View(ces)

# read in a sheet from a multisheet excel file that is NOT sheet #1, 
# and further, let's skip some rows before we read the data.
metadata <- read_xlsx(fp, sheet = 2, skip = 6)


# Read shapefiles into R --------------------------------------------------

unzip(here("data", "shp", "sac_county.zip"), 
      exdir = here("data", "shp", "sac"))

library(sf)
sac_county <- st_read(here("data/shp/sac/sac_county.shp"))

library(ggplot2)
ggplot(sac_county) + geom_sf()
mapview::mapview(sac_county)



# write data from R to csv ------------------------------------------------

# write "stations" to a file in the data_output folder called "my_stations.csv"
my_stations <- stations[1:5, ] # row column indexing (rows 1:5), all columns

# actually we only want the first 2 columns
my_stations <- stations[1:5, 1:2]

write_csv(my_stations, here("data_output", "my_stations.csv"))


# write the Sacramento county polygon to a shapefile
st_write(sac_county, here("data_output", "sac_county.shp"))

# write the Sacramento county polygon to an rds file
write_rds(sac_county, here("data_output", "sac_county.rds"))

# check if files exist with a logical test with list.files() and file.exists()

my_results <- list.files(here("data_output"))
my_files  <- c("sac_county.shp", "sac_county.rds", "i love my cats")

# test if my files are in my results with %in%
my_files %in% my_results


# this will return an error b/c there is no 
# sc_county object in the Environment
st_write(sc_county, here("data_output/sac_county.shp"))



# day 2: review read and write some data -----------------------------------

library(readxl)
library(here)
library(readr)

# read the stations dataframe from a URL
stations <- read_csv("https://github.com/r4wrds/r4wrds/blob/main/intro/data/gwl/stations.csv?raw=true")

# view the stations data frame
View(stations)

# modify the stations dataframe and then write it (export) to a file
# export the top 10 rows and all columns with [row, column] indexing
# FORMULA  =====>    dataframe[rows, columns]
first10_stations <- stations[1:10, ]
  
# export the first 10 stations to a csv file
write_csv(first10_stations, here("data_output/first10_stations.csv"))

# how to install a package - readr as an example
install.packages("readr")
library(readr)


# day 1 review: read and write a shapefile --------------------------------

library(sf)
library(here)

# read a shapefile of Sac County
sac <- st_read(here("data/shp/sac/sac_county.shp"))
mapview::mapview(sac)

# in a working day scenario, we'd modify the shapefile here before
# writing it out (e.g., buffer, join, add data, intersection, etc)

# write the shapefile to an output path
st_write(sac, here("data_output/my_shapefile.shp"))



# visualization -----------------------------------------------------------

library(tidyverse)
library(here)

# read some groundwater level (timeseries) data
gwl <- read_csv("data/gwl/gwl.csv")

# view the top 10 rows of the data
head(gwl, 10)

# create our first ggplot!
ggplot()

# add a geom_line() to the plot we created above and make a plot
# of depth to groundwater over time.
ggplot(data = gwl) +
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE))

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# recreate he plot above with fewer lines of code, now
# that we know the "order of the arguments"
ggplot(gwl) +
  geom_line(aes(MSMT_DATE, GSE_WSE))

# let's make a point plot now
ggplot(gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE))

# let's make our points semi-transparent with the alpha argument
ggplot(gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.2)

# ggplots are built in layers. Let's add a line on top of 
# our points
ggplot(gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.2) +
  geom_line(aes(MSMT_DATE, GSE_WSE), color = "red", alpha = 0.7)

# make a histogram of the depth to groundwater at this well
ggplot(gwl) +
  geom_histogram(aes(GSE_WSE), bins = 100)


# data visualization of multiple groundwater level stations ---------------

library(tidyverse)

# read in gwl data for 10 stations in Sac and Placer counties
gwl_10 <- read_csv("data/gwl/gwl_10.csv")

# make a point plot
ggplot(gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE))

# print the unique values in the SITE_CODE column
unique(gwl_10$SITE_CODE)

# re-create the plot above and color by SITE_CODE column
# there is a column that we want to associate to the color
# of the point, so we put it inside aes(color = )
ggplot(gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE, color = SITE_CODE))

# let's decorate a plot
ggplot(gwl_10) +
  geom_boxplot(aes(GSE_WSE, SITE_CODE, fill = WELL_USE)) +
  labs(
    title = "Depth to groundwater at 10 sites",
    subtitle = "Sacramento and Placer Counties (1960 - present)",
    x = "Measurement Date",
    y = "Depth to Groundwater (ft below land surface)",
    caption = "Source: Periodic Groundwater Level Database, CA DWR. Accessed XXXX"
  )



# facets: the art of making subplots --------------------------------------

# a facet takes a column, and creates a subplot for each 
# unique value in the column
length(unique(gwl_10$SITE_CODE)) # there are 10 unique site codes

# make a hydrograph subplot for each of the 10 unique site codes
ggplot(data = gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE)) +
  facet_wrap(~SITE_CODE) # this is new. don't forget the tilde!

# let's arrange the facets as a grid of 5x2, and "free" the x 
# and y scales so we can effectively "zoom in" on the data
ggplot(gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE)) +
  facet_wrap(~SITE_CODE, ncol = 2, scales = "free")


# saving plots ------------------------------------------------------------

# assign a gplot to a variable
my_plot <- ggplot(gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE)) +
  facet_wrap(~SITE_CODE, ncol = 2, scales = "free")

# create a subdirectory called "figures" in the project dir
dir.create("figures") 

# save a plot with ggsave() - write to png and pdf
ggsave(
  filename = "figures/my_facet.png", 
  plot     = my_plot, 
  width    = 10, 
  height   = 8
)
# ggsave("figures/my_facet.pdf", my_plot, width = 10, height = 8)



# {dplyr} -------------------------------------------------------------------


# load library once per session
library(tidyverse)

# now we need to import some data
stations <- read.csv("data/gwl/stations.csv")



## filter which is for ROWS ------------------------------------------------

# filter to just sacramento county data
stations_sac <- dplyr::filter(stations, COUNTY_NAME == "Sacramento") 

# filter ROWS to more than one condition?

## link conditions by "," which is same as "AND" (similar to SQL)
stations_sac <- filter(stations, 
                       # first condition to filter by
                       COUNTY_NAME == "Sacramento",
                       # second condition
                       WELL_USE == "Residential")


table(stations_sac$WELL_USE)
table(stations$WELL_USE)

# filter to multiple counties?
# use the "%in%"

sep_counties <- c("Sacramento", "Placer", "El Dorado")
sep_counties

stations_multcounties <- filter(stations, COUNTY_NAME %in% sep_counties )


# filtering for MATCHES, but what if we want to exclude?
# we want everything except something in a row
stations_trim <- filter(stations, !COUNTY_NAME == "Yolo")
stations_trim <- filter(stations, COUNTY_NAME != "Yolo") # same as above
stations_trim <- filter(stations, !COUNTY_NAME %in% c("Yolo")) # same as above

table(stations_trim$COUNTY_NAME)



# select for COLUMNS ------------------------------------------------------

stations_sel1 <- dplyr::select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME))

# drop a specific column (or exclude)
# 1:3 # sequence... can use same concept to list columns to and from
stations_sel2 <- dplyr::select(stations, -c(LATITUDE:BASIN_NAME, COUNTY_NAME))

# so in select, the comma means OR (unlike in filter, where it means AND)
stations_sel3 <- dplyr::select(stations, starts_with("W"), contains("NAME"))

# check column names
names(stations_sel3)


## The %>% PIPE ------------------------------------------------------------


# filter ROWS
stations_multcounty1 <- filter(stations, COUNTY_NAME %in% 
                                 c("Sacramento", "Placer"))

# select COLUMNS
stations_multcounty2 <- select(stations_multcounty1, starts_with("W"), 
                               contains("NAME"), contains("ID"))

# rename the STN_ID to station_id: rename(new_col_name, old_col_name)
# make sure colnames don't start with a number, and no spaces in name
stations_multcounty3 <- rename(stations_multcounty2, station_id = STN_ID)


# rewrite the code above using the PIPE: %>%  (Ctrl + Shift + M)
stations_multcounty <- stations %>% 
  # step 1: filter
  filter(COUNTY_NAME %in% c("Sacramento", "Placer")) %>% 
  # step 2: select
  select(starts_with("W"), contains("NAME"), contains("ID")) %>% 
  rename(station_id = STN_ID)



# mutate (to transform or add things to your data frame) ------------------

# mutate will modify the existing dataframe

stations_mutate1 <- stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048)

# visual check:
stations_mutate1 %>% 
  ggplot() +
  geom_point(aes(x=STN_ID, y = WELL_DEPTH), color = "cyan4", alpha = 0.5) +
  # add a point geom in meters
  geom_point(aes(x = STN_ID, y = WELL_DEPTH_m), color = "maroon", pch = 21, alpha = 0.8)

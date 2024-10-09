# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Live Code ------------------------------------------

# Good Morning!

print("Good Morning!")
print("Hello")
print()


## A section Break! --------------------------------------------------------

# so to make section breaks we can use Ctrl + Shift + R, and then
# navigate via the outline button in the top right of the source panel

# Calculator --------------------------------------------------------------

8/4 # this is just a calculation


# Assigning Variables to the Environment ----------------------------------

# now we will assign a variable name to a value
stream <- 5
pebble <- 8

# Alt + - is the shortcut key for the assignment "<-"
stream + pebble

flow <- stream + pebble

# to remove a variable name: 
strem <- 4
rm(strem)

# 4stream <- 5
Stream # this doesn't exist! Remember R is case sensitive
stream_2 <- 7
stream_in_yard <- 11

stream_in_yard # tab autocomplete is your friend!

# CamelCase
# snake_case


# Installing Packages -----------------------------------------------------

# install.packages("beepr")
# Tools > Install Packages > then type in package "beepr", then Ok
library(beepr)
beep(3)

library(tidyverse)


# Getting Help ------------------------------------------------------------

# google is your friend, more specific queries are better:
# how do I import excel with R package

# help within R
help("print")
help("dplyr")
help("mutate")



# Module 4: importing and exporting data ----------------------------------

# import csv, xlsx, shp, rds

# comments out out

library(readr)
stations <- read_csv('data/gwl/stations.csv')

# a few ways to explore your df
head(stations)
print(stations, width = Inf)
nrow(stations)
ncol(stations)
dim(stations)
class(stations)
View(stations)

# what does class do-- describes an R object
class(1)
class(TRUE)

# to access a vector
stations$WELL_TYPE
stations$WELL_TYPE[1]
print(stations, width = Inf)
stations$WELL_TYPE[1:10]
1:10


# xlsx files
library(readxl)
ces <- read_xlsx('data/calenviroscreen/ces3results.xlsx')
print(ces, width = Inf)

# a whole bunch warnings interpretted NA as a character
ces <- read_xlsx('data/calenviroscreen/ces3results.xlsx', na = c('', 'NA'))
head(ces)
print(ces, width = Inf)

# get help
?read_xlsx

metadata <- read_xlsx('data/calenviroscreen/ces3results.xlsx', 
          sheet = 2, skip = 6)
ces
metadata

# view in R
View(metadata)

# spatial data--shp
unzip('data/shp/sac_county.zip', exdir = 'data/shp/sac')

# import this package to work with spatial data
library(sf)
sac_county <- st_read('data/shp/sac/sac_county.shp')
sac_county
plot(sac_county$geometry)

# write csv files out
write_csv(x = stations, file = 'data_output/my_stations.csv')

# write spatial data out
st_write(sac_county, 'data_output/copy_of_sac_county.shp')

x <- 1


# Data Structures ---------------------------------------------------------

# we use <- to assign objects. = assign parameters

# data frame
stations <- read.csv(file = 'data/gwl/stations.csv')  
head(stations)
head(station) # will not exist

# say i accidentally created an ojbect with a name that i don't want
station <- read.csv('data/gwl/stations.csv')
rm(station)

str(stations)

# look up the class of an object
class(stations)

# look up the class of columns within the data frame
class(stations$LATITUDE)
class(stations$STN_ID)

# lets review logical, numeric, character, factors


# a df with 2 vectors, chr, logical
animals <- data.frame(animals = c('cat', 'bird', 'bat', 'dog'),
           flies = c(F, T, T, F))
class(animals$animals) # characters bc they're words
class(animals$flies)


# LOGICAL CLASS
dry <- c(TRUE, FALSE, FALSE) # different from 'true' and 'false'
dry
dry <- c(T, F, F)
class(dry)

# NUMERIC CLASS
flow <- c(0, 57, 128)
class(flow)

# characters
reach <- c('Dry Creek', 'Raging Waters', 'Wild Rapids')
class(reach)

# factors = ordered characters
months <- c('July', 'January', 'February', 'August')
months
sort(months)

month_order <- c('January', 'February', 'July', 'August')
date <- factor(months, levels = month_order)
date
sort(date)

# sidenote: package that makes factors easy forcats

# R is a vector based program
toupper(reach)
flow * 10

# data frame vs. tibble
library(readr)
stations_tib <- read_csv('data/gwl/stations.csv')
stations_tib
class(stations_tib)

# special case of NA
NA # NA, logical class
'NA' # character
class('NA')
class(NA)
z <- c(2, NA, 4)
mean(z, na.rm = T)

y <- c(2, 'NA', 4)
mean(y, na.rm = T)



# data wrangling ----------------------------------------------------------

library(tidyverse)

# import the data
stations <- read_csv('data/gwl/stations.csv')
stations

# filter: remove/keep rows that you want
stations_sac <- filter(stations, COUNTY_NAME == 'Sacramento')
print(stations_sac, width = Inf)

# looks up all the unique values within a vector
unique(stations$WELL_USE)
stations_sac <- filter(stations, COUNTY_NAME == 'Sacramento' & WELL_USE == 'Residential')

# to include multiple values in a data frame, use %in%
use_these_counties <- c('Sacramento', 'Placer', 'El Dorado')
stations_multicounty <- filter(stations, COUNTY_NAME %in% use_these_counties)
stations_multicounty$COUNTY_NAME

# to remove values !
filter(stations, COUNTY_NAME != 'Yolo') # not equal. 
# filter the stations to include all the counties EXCEPT Yolo County
filter(stations, !COUNTY_NAME == 'Yolo')

# select
stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME))
names(stations)
names(stations_sel1)
1:10

# : allows you to select columns without directly naming them, 
# and means that you're selecting columns in a sequence
select(stations, STN_ID:WELL_NAME)

# exclude columns
stations_sel2 <- select(stations, -c(LATITUDE:BASIN_NAME, WCR_NO))
names(stations)
names(stations_sel2)

# The Pipe %>% cntl + shift + M
# %>% 

# the old way
stations_filt <- filter(stations, COUNTY_NAME == 'Sacramento')
stations_sel <- select(stations_filt, c(STN_ID, SITE_CODE, COUNTY_NAME))
stations_rename <- rename(stations_sel, station_id = STN_ID)
stations_rename

# the new way--use the pipe!
stations_piped <- stations %>% 
  # filter the df
  filter(COUNTY_NAME == 'Sacramento') %>% 
  # select columns
  select(c(STN_ID, SITE_CODE, COUNTY_NAME)) %>% 
  # rename one of the columns
  rename(station_id = STN_ID)
stations_piped

# check out the function rename
rename(stations_piped, county = COUNTY_NAME) # new name = old name
rename(stations_piped, COUNTY_NAME = county)

# if you every need help with the function, look up the documentatoin
?rename

# the pipe is a way to say "AND THEN" and continues your operations


# MUTATE = change a column and create a new one
names(stations) # remind myself what cols exist
stations_mutate <- stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * .3048)
print(stations_mutate, width = Inf)

# if else
x <- c(0, 1, 10, 15, 5, 3, 20)
x
if_else(x >= 10, TRUE, FALSE)
stations_mutate %>% 
  mutate(WELL_DEPTH_100 = if_else(WELL_DEPTH_m >= 100, T, F)) %>%
  print(width = Inf)


# to summarize data, group_by, summarize
stations %>% 
  group_by(COUNTY_NAME) %>% 
  summarise(n_stations = n(),
            mean_lat = mean(LATITUDE),
            mean_lon = mean(LONGITUDE), 
            n_wells_100 = sum(WELL_DEPTH > 100, na.rm = T)) %>% 
  filter(n_wells_100 > 200) %>% 
  arrange(-n_wells_100)



# Shift + Control + 0 = restart R


# ggplot2 for visualization -----------------------------------------------

library(tidyverse)

# read in data
gwl <- read_csv("data/gwl/gwl.csv")

# make our first ggplot
ggplot() # this is blank, b/c we didn't add geoms

# What's a geom? Example: geom_line()
ggplot(data = gwl) +
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE))


# Anatomy of a ggplot:
# ggplot(data = <DATA>) +
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
# geoms can include: geom_point(), geom_col(), geom_boxplot(),
# geom_histogram(), geom_line(), and so on.
# aes() are aesthetics like: x-axis, y-axis, color, fill, shape

# Change the preior plot from a line to a point
ggplot(data = gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.3)


# Combine point and line geometries.
ggplot(data = gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.5) +
  geom_line(aes(MSMT_DATE, GSE_WSE), color = "red", alpha = 0.7)


# Make a histogram
ggplot(gwl) +
  geom_histogram(aes(GSE_WSE))

# Change the default number of bins (30) to a larger number, like 100
ggplot(gwl) +
  geom_histogram(aes(GSE_WSE), bins = 100)



# A more complex example --------------------------------------------------

# read groundwater level data from 10 monitoring sites in Sac and Placer
gwl_10 <- read_csv("data/gwl/gwl_10.csv")

ggplot(gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE, color = SITE_CODE),
             alpha = 0.5) +
  geom_hline(aes(yintercept = 100), linetype = "dashed") +
  geom_vline(aes(xintercept = as.Date("2012-10-01")))


# Faceting subplots with ggplot -------------------------------------------

# save this plot to your computer
p <- gwl_10 %>% 
  ggplot() +
  geom_line(aes(MSMT_DATE, GSE_WSE)) +
  facet_wrap(~SITE_CODE, scales = "free", ncol = 2)

p
ggsave(filename = "my_plot.png", plot = p)

# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Live Code ------------------------------------------

# Good Morning!

print("Hello")

# storing variables

stream <- 4 # the shortcut for <- is Alt + - or Opt + - on mac

# using equals (=) generally reserved for inside function "arguments"
mean(x = 4)

# remove a variable from your environment
rm(x)

pebble <- 8

# add/use variables together
stream + pebble

habitat <- stream + pebble

# R case sensitive, starting variables with numbers is not allowed, spaces, etc.

habitatt # spelled wrong, gives error

# 2habitats <- 11 # can't start with a number


# COMMENT YOUR CODE EARLY AND OFTEN!!

# Packages ----------------------------------------------------------------
# Ctrl + Shift + R gives a section break in a R script

library(tidyverse)

# if you get an error, no package found
install.packages("tidyverse")

# once installed, then try to load library again

# getting help...really important
# help tab, look at arguments, and examples
?mean
?max

# for functions that may be very new or not within a currently loaded package, search for a wildcard that matches anything close
??mutate

# Project Management ------------------------------------------------------

# absolute file path vs. relative

absolute_filepath <- "/Users/rapeek/Downloads/intro_proj/data/nwis_sites_american_river.csv"

read.csv(absolute_filepath)

# so this would break on anyone else's computer

# same thing setwd()
setwd("/Users/rapeek/Downloads") # sets my working directory to this path
# this means no matter what someone has to change this code or this line

# sometimes in macosx: the ~ is a shortcut for /Users/rapeek

# using relative filepaths and Rprojects

relative_filepaths <- "data/nwis_sites_american_river.csv"

# Reminders ----
## Running code in script: Ctrl + Enter (will run the line your cursor is on)
2+2
## Project check
getwd()
## Error note: reason for "could not find function" is usually either 1) package not loaded or 2) misspelling
list.files("data")


# Module 4: ----

?read_csv
library(tidyverse)
?read_csv

## Importing and exporting data
# read csv for comma separated values
stations <- read_csv("data/gwl/stations.csv")

## Briefly explore data
# exploration functions
head(stations)
nrow(stations)
ncol(stations)
View(stations)

# data dimensions -- extract certain parts of the data for exploration
colnames(stations)
# use $ to 'index' certain columns
stations$WELL_TYPE
# add to this by taking a row-wise dimension with square brackets []
stations$WELL_TYPE[1]
# colon signifies range
stations$WELL_TYPE[1:10]
# trick for bad column names -- avoid having to do this
stations$`WELL_TYPE`

## Export data
?write_csv
# x is the first argument, it is the object you want to save
write_csv(stations, "data_output/my_stations.csv")

# Excel
library(readxl)
#install.packages('readxl')
ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx")
# how R informs you of issues
# red X: unmatched open bracket, or comma
# warning: your function does run but maybe an issue
# error: function does not run
# with any error or warning -- google
?read_xlsx

View(ces)


# Module 6: data structures -----------------------------------------------

## basic data classes -- building blocks of working with data in R
str(stations)
# 4 main classes: character, numeric, logical, factor

## 1. character (also called strings) -- anything in quotes
class("seven")
sum("7", "6")
?paste
paste("High", 5)

# vectors: multiples values of the same class, 'concatenated' together using the c() function, elements separated by commas

reach <- c("Wild Rapids", "Dry Creek", "Raging Waters")
reach
class(reach)

## 2. Numeric
flow <- c(0, 57, 218)
class(flow)

date <- "7/19/21"
class(date)
date2 <- 7/19/21
date2
# lubridate package for dealing with date times


## 3. logical (Boolean): special class of data for true/false
dry <- c(TRUE, FALSE, FALSE)
class(dry)

## 4. factors: characters with a prescribed order
# example month
month <- c("July", "January", "February")
class(month)
# use the factor function
month <- factor(month)
class(month)
month
# specify the order of our factors unless we want it alphabetical
?factor
month <- factor(month, levels = c("January", "February", "July"))
class(month)
month


# 3 data structures: vectors, data frames and lists
## vectorization: operation is applied along the entire vector
# cfs converted to gallons per minute: * 448.83
flow * 448.83

paste("I am monitoring", reach)

## data frames: set of named vectors, arranged as columns, all of the vectors should be the same length
riv <- data.frame(reach, month, dry, flow)
head(riv)

riv$reach[2]
# also use the dollar sign to create new column
riv$tech <- "Rich"
riv$tech

riv$tech <- c("Rich", "Liza")
riv$tech <- c("Rich", "Liza", "Ryan")
riv

## list: can hold many different types of data in vector and vectors can take different length
single_string <- "A character object with a length of 1"
class(single_string)
length(single_string)

l <- list(dry, reach, flow, single_string)
l
# we cannot use our dollar sign with a list
#l$dry
# extract element using double brackets
l[[1]]
l[[4]]
l[[3]][1]

# properties of R structures ^
# dealing with NAs

z <- c(2, 5, NA)
class(z)
mean(z)
?mean
mean(z, na.rm = T)


# Data wrangling with dplyr -------

library(tidyverse)
stations <- read_csv("data/gwl/stations.csv")
class(stations)

# Filter: reducing rows based on a condition ----
## Conditions: equals ==, does not equal !=,
## or |, contains %in%, greater than >, greater than and equal to >=
colnames(stations)
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento")
dim(stations_sac)
# combine several conditions with , 
# Sacramento only and well use
table(stations$WELL_USE)
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento", 
                       WELL_USE == "Residential")
# filter based on several countries
# one way: use an OR | conditional, or we could use the contains %in%

counties_of_interest <- c("Sacramento", "Placer", "El Dorado")
stations_multicounty <- filter(stations, COUNTY_NAME %in% counties_of_interest)
# check with code
table(stations_multicounty$COUNTY_NAME)

# Select -------
colnames(stations)
# name the columns you want to keep
stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE,
                                    COUNTY_NAME))
colnames(stations_sel1)

# use the range symbol :
stations_sel2 <- select(stations, c(LATITUDE:BASIN_NAME))
# spacing question
vector <- c("Sacramento", "Placer", "El Dorado", "Sacramento")

# additional functions that can help select columns: starts_with, contains
stations_sel3 <- select(stations, starts_with("WELL"), contains("NAME"))
colnames(stations_sel3)
# select can help you re order columns

# Pipe %>% -------
# Ctrl + Shift + M
# Pipe: streamline your coding pipe an object forward... values on the left hand side will move along into functions on the right hand side of the pipe

# Step 1: filter
stations_filtered <- filter(stations, COUNTY_NAME %in% c("Sacramento", "Placer"))
# Step 2: select from the data I've filtered
stations_selected <- select(stations_filtered, starts_with("WELL"))

# Pipe: start with data and pipe the data into the function
stations_piped <- stations %>% 
  # filtered based on county
  filter(COUNTY_NAME %in% c("Sacramento", "Placer")) %>% 
  # selected only well columns
  select(starts_with("WELL")) %>% 
  # renaming well use, making new name = old name
  rename(well_use = WELL_USE)

# left hand side of pipe %>% right hand side of the pipe

# Mutate -----
## create a new column
stations %>% 
  select(WELL_DEPTH) %>% 
  # mutate: new name = transformation
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048) %>% 
  # na removal with is.na(): logical question
  # ! is a negative
  filter(!is.na(WELL_DEPTH_m))

# Group_by and count and summarize -----
# How many groundwater monitoring sites are in each county? And why type of well uses are they?

# group by is always used with a summary type of function
stations %>% 
  # group_by on almost meaningless on its own
  group_by(COUNTY_NAME) %>% 
  count()

stations %>% 
  group_by(COUNTY_NAME, WELL_TYPE) %>% 
  count() %>% 
  # arrange function orders, use desc() to reverse order
  arrange(desc(n))

# summarize ----
# What is the mean well depth in each county?
stations %>% 
  # think of group by as the columns you want in your summry table
  group_by(COUNTY_NAME) %>%
  # specify what kinds of summary functions to use
  # single = to assign a new variable, not ==
  summarize(mean_well_depth = mean(WELL_DEPTH, na.rm = TRUE)) %>% 
  arrange(desc(mean_well_depth))



# Data Visualization with ggplot2 -----------------------------------------

library(tidyverse)


# read data
gwl <- read_csv("data/gwl/gwl.csv")

# make our first ggplot - lay out the "canvas"
ggplot()

# add layers to our ggplot with "geoms"
ggplot(data = gwl) +
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE))

# anatomy of a ggplot
# ggplot(data = << data.frame >>) +
#   geom_<< type >>(mapping = aes(<< mappings >>)) 

# new geom: geom_point()
ggplot(data = gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.3)

# point and line plot
ggplot(data = gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.3) +
  geom_line(aes(MSMT_DATE, GSE_WSE),  color = "red", alpha = 0.7) +
  # add a linear regression line
  geom_smooth(aes(MSMT_DATE, GSE_WSE), method = "lm", se = FALSE)

# geom_histogram
ggplot(data = gwl) +
  geom_histogram(aes(GSE_WSE)) +
  geom_vline(aes(xintercept = median(gwl$GSE_WSE)))

# read 10 groundwater stations
gwl_10 <- read_csv("data/gwl/gwl_10.csv")

# color as an aesthetic colors the geoms by unique values in a column
ggplot(gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE, color = SITE_CODE), alpha = 0.3)

# faceting: how to make subplots by unique values in a column
ggplot(gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE)) +
  facet_wrap(~SITE_CODE, ncol = 2, scales = "free")

# saving plots: 2 steps 
# Step 1 = assign plot to a variables
# Step 2 = use ggsave()
p <- ggplot(gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE)) +
  facet_wrap(~SITE_CODE, ncol = 2, scales = "free") +
  labs(
    x = "", 
    y = "Depth to groundwater (ft below land surface)",
    color = "Well Use",
    title = "My title",
    subtitle = "My subtitle",
    caption = "My caption"
  )
p

# save your plot to your computer
ggsave("data_output/gwl_10.png", p, width = 10, height = 6)













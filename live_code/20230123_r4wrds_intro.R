# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Welcome to the Intro Course! (2023-01-23)

# Good Morning

# lets try some live code and check packages
c( "tidyverse", "sf", "viridis", "mapview" ) %in% installed.packages()

# typing things after a # sign is a comment, that won't be evaluated
# to change panels, use Tools > Global Options > Pane Layout

print("Hello")

# R can be used like a calculator (very fancy)
8 / 4
10 / 3

# for a list of things
c(4, 6)

8 * 5
9 / 5 + 32 - (45-2) / ((2/5)*2)

# create variable names

stream <- 120

# strm <- 120 # oops!

pebble <- 8

lake <- 10

# using help for a popup, use F1
# LakeHuron

# this breaks because it starts with a number
# 8stream <- 8

stream + pebble - lake
Stream + pebble # case sensitive
strm + pebble # spelling sensitive

# how to remove a value?

rm(strm, lake, pebble, stream)

# Project Management



# to add a section header to your code or script, use Ctrl + Shift + R
# or Code > Insert Section

# Section Headers ---------------------------------------------------------


# Downloading Data --------------------------------------------------------

# two options to get data into your R project
dir.create("data")

# now let's download some data
# downloads the file (data.zip) to your data directory
download.file(url = "https://github.com/r4wrds/r4wrds-data-intro/raw/main/data.zip", destfile = "data/data.zip")

# soft wrap of source file is when you type a really long line or have a really long line of code that is way too long

getwd() # get the current working directory

unzip(zipfile = "data/data.zip")

# setwd() should be avoided


# Making Directories ------------------------------------------------------

# can do with code
dir.create("docs")
dir.create("figures")

# or use the buttons in Files tab

# Testing livecode


# Reading in data ---------------------------------------------------------

# install.packages()... only need to do this once
library(tidyverse)

?read_csv
# ask R where you are in your computer: you should be in your project folder
getwd()
# read_csv function to read in csv (comma separated values) files
# arguments go inside the parentheses
stations <- read_csv("data/gwl/stations.csv")

# data frame is a common class for data
class(stations)

# initially inspecting data with...
head(stations) # glance
nrow(stations) # number of rows
ncol(stations) # number of columns
dim(stations) # dimensions
View(stations)

# indexing data: pulling columns and rows
colnames(stations)
# $ to index columns
stations$WELL_TYPE
# use [] to index rows in column
stations$WELL_TYPE[1]
stations$WELL_TYPE[1:10] # : means range

# Excel sheets
library(readxl)
ces <- read_xlsx("data/calenviroscreen/ces3results.xlsx")

# Error = function did not work
# Warning = may or may not have done what you expected

dim(ces)
head(ces)

?read_xlsx
# commas separate arguments 
# order matters when you don't specify argument
metadata <- read_xlsx(skip = 6, path = "data/calenviroscreen/ces3results.xlsx", sheet = 2)
head(metadata)

# argument out of order without specification throw error
#metadata <- read_xlsx(6, "data/calenviroscreen/ces3results.xlsx", 2)

# shp files
library(sf)

# need to unzip first
#st_read("data/shp/sac_county.zip")

?unzip
library(here)
here()
# Make sure you specify the directory to extract the files to
unzip("data/shp/sac_county.zip", exdir = "data/shp/sac")

sac <- st_read("data/shp/sac/sac_county.shp")



# Plotting ----------------------------------------------------------------


# ggplot2 -- built into tidyverse
library(ggplot2)
# bonus item -- if its not working, AOK
# ggplot(sac) + geom_sf()

# Create a blank canvas with the base function
ggplot()

# 3 parts: (1) data, (2)geom (geometric function) = shapes, (3) mappings = x,y but also data driven features

gwl <- read_csv("data/gwl/gwl.csv")
dim(gwl)
head(gwl)

# visualize depth to groundwater over time (x = MSMT_DATE, y = GSE_WSE); as a line

# ggplot feature: layers with +
# can space after the +
ggplot(data = gwl) + 
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE))

ggplot(data = gwl) + # space AFTER
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE)) +
  # don't need to specify arguments if in correct order
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.5)
# alpha is transparency 0-1











































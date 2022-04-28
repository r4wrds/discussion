# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github:
# 2021-08-10: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210810_r4wrds_intro.R
# 2021-08-24: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210824_r4wrds_intro.R 
# 2021-09-21: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210921_r4wrds_intermediate.R 
# 2021-10-05: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20211005_r4wrds_intermediate.R 


# Intro To Console --------------------------------------------------------


print("hello world!")

print("Good morning!")

print("Good morning again!")

version

# check what RTools or "make" version you have
Sys.which("make")

# Assignment Variables
stream <- 4 # then hit Ctrl + enter to send to console
# spaces between values aren't interpreted by R
# mainly there for readability
pebble <- 8
stream + pebble # adding to variables that are assigned

# we can also use "=" as an assignment operator
stream = 4 # same as <-
seq(from = 1, to = 5, by = 1) # a function to sequence numbers

# shortcuts for everything: assignment shorcut is Alt + - 
# to find shortcuts, go to Help in RSTudio, type shortcuts, and click Keyboard Shorcuts Help

habitat <- stream + pebble

# R is case sensitive, be detail oriented
Habitat # error doesn't work
habitat # try tab after first few letters to autocomplete


# Install Packages --------------------------------------------------------


# installing packages
# Tools > Install Packages > type name, and hit enter
# or 
# install.packages("readr")

# load a package once per session (these are just examples)
# library(calculus)
# library(abbreviate)
# install.packages(abreviate)

# to see the functions in a package we can use ::
# calculus::

# can access a function from a package without loading, using double ::
# readr::read_csv()

# Section Break Followed by At least 4 "-" ------------------


# Setting Up Project Data -------------------------------------------------
# enable rainbow parentheses with Tools > Global Options > Code > Display tab > check Rainbow Parantheses
# (((((((((((((())))))))))))))

# create a directory, if it exists, get a warning message
dir.create(path = "data")

# downloads the file (data.zip) to your data directory
# download.file(url = "https://github.com/r4wrds/r4wrds-data-intro/raw/main/data.zip", destfile = "data/data.zip")

# now lets unzip
# unzip(zipfile = "data/data.zip")

# how would we put local data into an R project? 
# click More button in Files Tab, open Directory and drag/drop or copy/paste into directory


# Using Relative Paths with {here} ----------------------------------------

# install.packages("here") 
library(here)

here() # identifies the path to your .Rproj file (your working dir)

read.csv(here("data/nwis_sites_american_river.csv"))

read.csv("data/nwis_sites_american_river.csv")


# Importing Data ----------------------------------------------------------
library(here)
library(readr)

stations <- read_csv(here("data", "gwl", "stations.csv"))
stations <- read_csv('data/gwl/stations.csv') # same without here
stations <- readr::read_csv('data/gwl/stations.csv') # same accessing single function from readr package

# we can look in the Environment Tab and see more about stations
# click blue arrow to see more about data
# click the name of the object to preview or 
View(stations) # needs to be uppercase!
# view(stations) # won't work
head(stations)

# read in directly from a URL:
# read the "stations" csv from the Github URL
stations <- read_csv(file = "https://github.com/r4wrds/r4wrds/blob/main/intro/data/gwl/stations.csv?raw=true" )

gwl <- read_csv("data/gwl/gwl.csv")

# how to remove an object from your environment
rm(gwl_new_name)
rm(gwl)

# how many rows or cols?
nrow(stations)
ncol(stations)

# dimension
dim(stations)


## Accessing Data in a Dataframe -------------------------------------------

# data frames are collections of columns
# columns are VECTORS of the same kind or class of data

# access a single column in a dataframe
stations$WELL_TYPE
head(stations$WELL_TYPE)

# use brackets to subset or acces the data as [ROWS, COLUMNS]
stations$WELL_TYPE[1]
stations$WELL_TYPE[1000]
# 1:10 a colon denotes a sequence from left to right
stations$WELL_TYPE[1:10]

# access different rows of data
stations$WELL_TYPE[c(1, 8, 12)]

# tally the values by a column?
table(stations$WELL_TYPE)
table(stations$WELL_TYPE, useNA = "ifany")

# summary function is really handy
summary(stations)


# Excel -------------------------------------------------------------------

# install.packages("readxl")
library(readxl)

ces <- read_xlsx(path = "data/calenviroscreen/ces3results.xlsx")
# can always type: "warnings()" to see what happened
# data still exists

names(ces)
head(ces)
ces$`Census Tract` # backticks allow spaces/characters in col names

metadata <- read_xlsx("data/calenviroscreen/ces3results.xlsx", 
                      skip = 6, sheet = 2)

## Challenge 1:

index_summary <- read_xlsx(here("data/healthy_watersheds/CA_PHWA_TabularResults_170518.xlsx"), sheet = 2, skip = 4)
names(index_summary)


# Shapefiles --------------------------------------------------------------

unzip("data/shp/sac_county.zip", exdir = "data/shp/sac")

library(sf)

sac_county <- sf::st_read("data/shp/sac/sac_county.shp")

# view the projections:
st_crs(sac_county)

library(ggplot2)

ggplot(data = sac_county) + geom_sf()

library(mapview)
mapviewOptions(fgb = FALSE)

mapview(sac_county, color="orange", lwd=4)


# dbf files ---------------------------------------------------------------

# install.packages("foreign")
library(foreign)

foreign::read.dbf("data/shp/sac/sac_county.dbf")



# RDA & RDS ---------------------------------------------------------------

# rds files we can assign an object name when we import
# also there can only be one object per rds file
stations_rds <- read_rds("data/gwl/stations.rds")

# RDA files, can save multiple objects in the same file
# use load() to load that object or objects back to environment
# however they are imported with the same name they were saved

# save an RDA file
save(stations, gwl, file = "data_output/demo_rdata_file.rda")

# rm to remove
rm(gwl, stations)

# load back to environment
load("data_output/demo_rdata_file.rda")


# SQL ---------------------------------------------------------------------

# install.packages("RSQLite")
library(RSQLite)

dbpath <- here("data/gwl/gwl_data.sqlite")
dbpath

dbcon <- dbConnect(dbDriver("SQLite"), dbpath)
dbcon

dbListTables(dbcon)

head(dbReadTable(dbcon, "stations"))
stations <- dbReadTable(dbcon, "stations")

# often you need to run a query 
dbGetQuery(dbcon, "SELECT * from measurements_sep WHERE STN_ID = 4775 LIMIT 5")



# Export ------------------------------------------------------------------

# writing data out

# csv
readr::write_csv(stations, file = "data_output/my_stations.csv")

# read_csv can write a zipped directly (and read a zipped file directly)
readr::write_csv(stations, file = "data_output/my_stations.csv.zip")

readr::read_csv(file = "data_output/my_stations.csv.zip")

readr::read_csv(file = "data/gwl/stations.csv.zip")

# writing out shapefiles
st_write(sac_county, "data_output/sac_county.shp")


# xlsx
# xlsx::write.xlsx()



# ggplot ------------------------------------------------------------------

library(tidyverse)

gwl <- read_csv("data/gwl/gwl.csv")

ggplot() + geom_line(data=gwl, mapping = aes(x=MSMT_DATE, y=GSE_WSE))


# ggplot2 website: https://ggplot2.tidyverse.org
# Starting here on day 2


# Day 2: ggplot -----------------------------------------------------------


# making sure we have the data we need

# create a directory, if it exists, get a warning message
#dir.create(path = "data")

# downloads the file (data.zip) to your data directory
#download.file(url = "https://github.com/r4wrds/r4wrds-data-intro/raw/main/data.zip", destfile = "data/data.zip")

# now lets unzip
#unzip(zipfile = "data/data.zip")

# how would we put local data into an R project? 
# click More button in Files Tab, open Directory and drag/drop or copy/paste into directory


# Import data and library -------------------------------------------------

# install.packages("tidyverse")
library(tidyverse)

gwl <- readr::read_csv("data/gwl/gwl.csv")
#gwl <- readr::read_csv("")

# Alt + -  will give "<-" <- 
# check the working directory
# getwd()

# variables in ggplot are typically NOT quoted
ggplot(data=gwl) + 
  geom_line(mapping = aes(x=MSMT_DATE, y = GSE_WSE)) +
  labs(title = "Groundwater Elevations")

# Ctrl + I will auto indent your code, can highlight a whole chunk of code and hit Ctrl + I to format correctly.

# + to make new layers in ggplot, but can run Ctrl + Enter on any line of the ggplot to execute the code.

# make point plot
ggplot(data=gwl) + 
  geom_point(mapping = aes(x=MSMT_DATE, y = GSE_WSE)) +
  labs(title = "Groundwater Elevations")

# alpha = transparency
ggplot(data = gwl) +
  geom_point( aes(x = MSMT_DATE, y = GSE_WSE), alpha=0.5) 

# add color
ggplot(data = gwl) +
  geom_point( aes(x = MSMT_DATE, y = GSE_WSE), alpha=0.5, color="blue") 

# can use the hex code as well:
#6495ED = cornflowerblue
ggplot(data = gwl) +
  geom_point( aes(x = MSMT_DATE, y = GSE_WSE), alpha=0.5, color="#6495ED") 

# geom_area
ggplot(data = gwl) +
  geom_area( aes(x=MSMT_DATE, y=GSE_WSE))

# Adding multiple layers
ggplot(data = gwl) +
  geom_point( aes(x = MSMT_DATE, y = GSE_WSE), alpha=0.5, color="#6495ED") +
  # last geometry is top layer on plot
  geom_line( aes( x= MSMT_DATE, y = GSE_WSE), color="maroon", alpha=0.7)

# switch layer so line is on bottom
ggplot(data = gwl) +
  # last geometry is top layer on plot
  geom_line( aes( x= MSMT_DATE, y = GSE_WSE), color="maroon", alpha=0.7) +
  geom_point( aes(x = MSMT_DATE, y = GSE_WSE), alpha=0.5, color="#6495ED")

# if CONSOLE doesn't have the ">", but ends with a plus, and we try to send more ggplot code along, we may get this error: 
# Error in `ggplot_add()`:
#! Can't add `ggplot(data = gwl)` to a ggplot object.
#Run `rlang::last_error()` to see where the error occurred.

# instead, if we see a "+" (or anything that isn't a ">")
# we can hit Escape in the Console, or Ctrl + C

# Histogram
summary(gwl$GSE_WSE)

# uses 30 bins as default
ggplot() + 
  geom_histogram(data = gwl, aes(x=GSE_WSE))

# make a finer resolution histogram with higher bin number
ggplot() + 
  geom_histogram(data = gwl, aes(x=GSE_WSE), bins = 100)

ggplot() + 
  geom_histogram(data = gwl, aes(x=GSE_WSE), bins = 100) #+
  #ylim(c(20, 40))


# Boxplot

ggplot(data = gwl) + 
  geom_boxplot(aes(x=GSE_WSE), fill="pink", alpha=0.3)

# How do we change the ylimit of the histogram?

ggplot() + 
  geom_histogram(data = gwl, aes(x=GSE_WSE), bins = 100) +
  scale_y_continuous(limits = c(0,25))

# using data in the ggplot() vs. in the geom_()


# Import Additional Data --------------------------------------------------

# 10 different stations from Sac and Placer Counties
gwl_10 <- read_csv("data/gwl/gwl_10.csv")

# plot 
ggplot(data=gwl_10) +
  geom_point(aes(x=MSMT_DATE, y=GSE_WSE))

# command to check how many unique sites in data
unique(gwl_10$SITE_CODE)

# map color to a variable in our data
ggplot(data=gwl_10) +
  geom_point(aes(x=MSMT_DATE, y=GSE_WSE, color=SITE_CODE), alpha=0.5)


# map color to a continuous numeric value
ggplot(data=gwl_10) +
  geom_point(aes(x=MSMT_DATE, y=GSE_WSE, color=WELL_DEPTH), alpha=0.5)

# boxplots of data
ggplot(data = gwl_10) +
  geom_boxplot(aes(x=SITE_CODE, y=GSE_WSE))

# x axis is hard to read...let's switch it
ggplot(data = gwl_10) +
  geom_boxplot(aes(y=SITE_CODE, x=GSE_WSE))

# can also use coord_flip()
ggplot(data = gwl_10) +
  geom_boxplot(aes(x=SITE_CODE, y=GSE_WSE)) +
  coord_flip()

# add some better labeling and titles
ggplot(data = gwl_10) +
  geom_boxplot(aes(y=SITE_CODE, x=GSE_WSE, color=WELL_USE)) +
  labs( y = "",
        x = "Depth to groundwater (ft)",
        color = "Well type",
        title = "Depth to groundwater at 10 stations",
        subtitle = "Sacramento and Placer County (1960-present)",
        caption = "Source: Periodic groundwater level database, CA-DWR")



# Save GGPLOT -------------------------------------------------------------

# create the plot and save it or assign it to a variable

my_plot <- ggplot(data = gwl_10) +
  geom_boxplot(aes(y=SITE_CODE, x=GSE_WSE, color=WELL_USE)) +
  labs( y = "",
        x = "Depth to groundwater (ft)",
        color = "Well type",
        title = "Depth to groundwater at 10 stations",
        subtitle = "Sacramento and Placer County (1960-present)",
        caption = "Source: Periodic groundwater level database, CA-DWR")

my_plot

# to save a plot: 
pdf(file = "figures/my_plot.pdf")
my_plot
dev.off()

# avoid spaces
#pdf(file = "figures w terrible folder name/my_plot_2.pdf")

png(file = "figures/my_plot.png")
my_plot
dev.off()

## Save A Plot with ggplot::ggsave

# if tidyverse is loaded, ggplot2 (the package) is already loaded
# here we can explicitly say the package::function

ggplot2::ggsave(filename = "figures/my_plot_ggsave.jpg", my_plot, 
                height = 10, width = 6, dpi = 300)

ggsave(filename = "figures/my_plot_ggsave.jpg", my_plot, 
                height = 10, width = 6, dpi = 300)



# FACETS ------------------------------------------------------------------

ggplot(data = gwl_10) +
  geom_line(aes(x=MSMT_DATE, y=GSE_WSE)) +
  facet_wrap(~ SITE_CODE)

# even it up and plot across 2 rows only
ggplot(data = gwl_10) +
  geom_line(aes(x=MSMT_DATE, y=GSE_WSE)) +
  facet_wrap(~ SITE_CODE, ncol = 2, scales = "free_y")

# facet across two different variables
ggplot(data = gwl_10) +
  geom_line(aes(x=MSMT_DATE, y=GSE_WSE, color=WELL_USE, group=SITE_CODE)) +
  facet_grid(COUNTY_NAME ~ WELL_USE, scales="free")


# Data Structure & Classes ------------------------------------------------

stations <- read.csv("data/gwl/stations.csv")

# different data classes: int (integer), chr (character), num (numeric)

str(stations)

# access column names with a "$"

class(stations)

class(stations$SITE_CODE)
class(stations$LATITUDE)

# ATOMIC VECTORS
# logical (TRUE or FALSE)
# numeric
# character
# factor (categorical or ordinal data)

c() # to make a vector in R

# logical
dry <- c(TRUE, FALSE, FALSE)

# integer
flow <- c(0, 57, 128)

# factor
date <- factor(c("July", "January", "February"), level = month.name)
sort(date)

# character
reach <- c("Dry Creek", "Deer Creek", "Sac River")

class(dry)
class(date)

# searching in a vector with the %in% and returning a boolean value (TRUE/FALSE)

"Merced River" %in% reach

"Dry Crk" %in% reach # not spelled correctly

"Dry Creek" %in% reach

c("Dry Creek", "Deer Creek") %in% reach

# iterates over each element in the vector
flow_gpm <- flow * 448.83 # vectorized operations in R

flow_gpm
flow

# FACTORS

date_character <- c("July", "January", "February")
sort(date_character)

sort(date)


factor_example <- factor(c("Low", "High", "Medium"), levels = c("Low", "Medium", "High"))

levels(factor_example)

# Data Structures ---------------------------------------------------------

length(dry)

# LISTS can hold different data classes

l <- list(dry, reach, flow, "random text") # 4 different data types
length(l)

# access first element of the list
l[[1]]
l[[2]]

# can add names to a list
names(l) # no names
names(l) <- c("dry", "reach", "flow", "string")

names(l)
l[["dry"]]


# Data Frames -------------------------------------------------------------

# data frame has to have vectors of same length (columns)
riv <- data.frame(reach, date, dry, flow)

riv$date

# drop a column
riv$reach <- NULL

# add a column
riv$reach_name <- reach

# add a column that isn't compatible with a length of 3
riv$tech <- c("Rich", "Ryan") # error because length isn't compatible

riv$tech <- c("Rich") # error because length isn't compatible


# Tibbles -----------------------------------------------------------------

library(readr)

stations <- read_csv("data/gwl/stations.csv")

class(stations)

stations_df <- data.frame(stations)
class(stations_df)



# Dealing with NAs --------------------------------------------------------

z <- c(2, NA, 4)

z

# do some calculations
sum(z, na.rm = TRUE) # default is FALSE, will not remove NAs
mean(z)
mean(z, na.rm = TRUE)

is.na(z)

# to take the opposite or "not equal", use ! 
!is.na(z)

# z2 <- c(4, NA, "na/n", 5)



# Working with dplyr ------------------------------------------------------

library(tidyverse)


stations <- read_csv("data/gwl/stations.csv")

# each function in dplyr does ONE thing

stations_sac <- dplyr::filter(stations, COUNTY_NAME == "Sacramento")

table(stations_sac$WELL_USE)

# chaining filter conditions together, "," means AND
# using a "|" is equivalent to OR
stations_sac <- dplyr::filter(stations, 
                              COUNTY_NAME == "Sacramento",
                              WELL_USE == "Residential")

# matches more values
stations_sac <- dplyr::filter(stations, 
                              COUNTY_NAME == "Sacramento" |
                              WELL_USE == "Residential")

table(stations_sac$WELL_USE)

# matching multiple things in a list or vector
sep_counties <- c("Sacramento", "Placer", "El Dorado")

stations_multcounties <- filter(stations, COUNTY_NAME %in% sep_counties)

table(stations_multcounties$COUNTY_NAME)

# excluding rows with "!"

stations_trim <- filter(stations, !COUNTY_NAME %in% c("Yolo")) # for more than one value, need c()
stations_trim <- filter(stations, !COUNTY_NAME == "Yolo")
stations_trim <- filter(stations, COUNTY_NAME != "Yolo")


# SELECT (columns)

stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME) )

names(stations_sel1)

# rearrange order based on the list order
stations_sel1 <- select(stations, c(COUNTY_NAME, STN_ID, LATITUDE, LONGITUDE) )

# drop multiple columns at once
stations_sel2 <- select(stations, -c(LATITUDE:BASIN_NAME, COUNTY_NAME))
ncol(stations_sel2)

# very powerful searching capabilities
stations_sel3 <- select(stations, starts_with("W"), contains("NAME"))



# The Pipe ----------------------------------------------------------------

# filter
stations_multcounty1 <- filter(stations, COUNTY_NAME %in% 
                                 c("Sacramento", "Placer"))
# select 
stations_multcounty2 <- select(stations_multcounty1, starts_with("W"), 
                               contains("NAME"), contains("ID"))

# rename the STN_ID to station_id: rename(new_col_name, old_col_name)
stations_multcounty3 <- rename(stations_multcounty2, station_id = STN_ID)

# pipe CTRL + SHIFT + M ( %>% )

stations_multcounty <- stations %>% # start with data
  filter(COUNTY_NAME %in% c("Sacramento", "Placer")) %>% 
  select(starts_with("W"), contains("NAME"), contains("ID")) %>%
  rename(station_id = STN_ID)


# Mutate ------------------------------------------------------------------

# Add new columns to existing dataframe

stations_mutate1 <- stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048)

# to specify where that new column goes:
stations_mutate1 <- stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048, .after = WELL_DEPTH)


# visually check!

stations_mutate1 %>% 
  # pass stations_mutate1 into the ggplot function
  ggplot() + 
  # add a point geom in feet
  geom_point(aes(x = STN_ID, y = WELL_DEPTH), color = "cyan4", alpha = 0.5) +
  # add a point geom in meters
  geom_point(aes(x = STN_ID, y = WELL_DEPTH_m), color = "maroon", pch = 21, alpha = 0.8)



# Group and Summarize -----------------------------------------------------

n_by_county <- stations %>% 
  group_by(COUNTY_NAME) %>% 
  count()

n_by_county

stations %>% 
  group_by(COUNTY_NAME) %>% 
  count() %>% 
  arrange(desc(n))


quantile()


# Replace Values ----------------------------------------------------------

stations_rename <- stations %>% 
  mutate(county_name_revised = case_when(
    COUNTY_NAME == "Sacramento" ~ "sacramento",
    COUNTY_NAME == "El Dorado" ~ "el_dorado",
    TRUE ~ COUNTY_NAME
  )) %>% 
  select(COUNTY_NAME, county_name_revised)

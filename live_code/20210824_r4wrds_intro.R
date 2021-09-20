# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

print("Good Morning!)")   


# Navigating Assignment ---------------------------------------------------




8 / 4
4 + 8

# to store a variable
stream <- 4
# same as this: stream = 4, but we want to use assignment <-

stream

# everything in R is case and SPELLING sensitive
Stream <- 6

# stram misspelled so returns an error
pancake <- 4

# can chain variables together
pancake + stream

pebble <- 10

habitat <- pebble + stream

#  = vs. <-  ("assignment")
# same meaning in R, but = generally used to mean equal to a value, 

pebble == stream # this is equivalent

cobble <- -1


# Installing Packages -----------------------------------------------------

# tab autocomplete

install.packages("mapview")

library("mapview")

mapview(breweries)
# should return an interactive map of breweries from germany
# these data are part of the mapview package
# if you have a map where the dots don't appear, try running this:
mapviewOptions(fgb = FALSE)
mapview(breweries)

## so to access any and all functions and data that come with a package,
# we can use "::" after the library
mapview::franconia  # this is another dataset
mapview(franconia)

# mapview::


# Getting Help from Help Section ------------------------------------------

# to get help for a function or a package, we can search using a single "?"
# followed by the name of the thing we want help on

?mapview

# double question mark will match anything close to mapview
??mapview


# Project & Data Management -----------------------------------------------

# install.packages("here")
library("here")

here() # gives the working directory

# use the here function to use relative paths

# option 1, we separate folders and files with commas, and quotes
read.csv( here( "data", "nwis_sites_american_river.csv" )) 

# option 2: use autocomplete with tab
# 
read.csv( here( "data/nwis_sites_american_river.csv" ) )

# using here is much easier than a full absolute path
read.csv("~/Dropbox/R_TEACHING/r4wrds_live/data/nwis_sites_american_river.csv")



# module 4: import and export ---------------------------------------------

# what is a function?
# examples: install.packages(), library(), read.csv(), here()

# all functions have a name, and take any number of "arguments"
# argument is an input to a function

# function = library()
# argument = here, which is the name of the package
library(here)

# install.packages is a function
# the argument it takes is the name of a package you
# wish to install on your computer

# R has a number of built-in functions
sum(1, 200)

# sequence is another built-in R function
seq(0, 10, 1)

# make another sequence by changing arguments
seq(33, 20000, 68)

# create a simple sequence
x <- 1:10 # : is a shorthand for sequence by 1


# read(import) data -------------------------------------------------------

# read some groundwater level station data
library(here)
library(readr)

# read the stations.csv in our data folder
stations <- read_csv(here("data/gwl/stations.csv"))

# head() takes a dataframe and looks at the first n rows
head(stations, 10)

# View() - note the capital "V" opens a interactive table
View(stations)

# you can also read csvs directly from a URL
stations <- read_csv("https://raw.githubusercontent.com/r4wrds/r4wrds-data/main/intro/data/gwl/stations.csv")

# tabular data have dimensions
dim(stations)
ncol(stations)
nrow(stations)

# class() tells you the object class of an object in R
class(stations)

# use the $ operator to access columns of a dataframe
stations$WELL_TYPE

# just take the first entry from a column
stations$WELL_TYPE[1]

# we can use sequences to subset columns
stations$WELL_TYPE[1:10]

# table() counts all unique values in a vector (column)
table(stations$WELL_TYPE)


# xlsx files --------------------------------------------------------------

# you may need to install.packages("readxl")
library(readxl)

# by default read_xlsx reads in the first sheet
ces <- read_xlsx(here("data/calenviroscreen/ces3results.xlsx"))
View(ces)

# read the second sheet of the excel file using the argument "sheet"
metadata <- read_xlsx(here("data/calenviroscreen/ces3results.xlsx"), 
                      sheet = 2, skip = 6)
metadata # print the object
head(metadata) # view the first 6 rows 
View(metadata)


# shp shapefiles ----------------------------------------------------------

unzip(here("data", "shp", "sac_county_shp.zip"), 
      exdir = here("data", "shp", "sac"))

# read the shapefile
library(sf)
sac_county <- st_read(here("data", "shp", "sac", "sac_county.shp"))

library(ggplot2)
ggplot(sac_county) + geom_sf()


# write data --------------------------------------------------------------

# time to export some data
write_csv(stations, here("data_output/my_edited_stations.csv"))




# module 5: data visualization with ggplot2 -----------------------------------------

# let's load some important libraries
library(tidyverse) # loads ggplot2, readr, dplyr, and some other packages
library(here)

# read in some groundwater level data from a single station
# in Sacramento County
gwl <- read_csv(here("data/gwl/gwl.csv"))

# create a ggplot
ggplot()

# add a geom_line() layer onto our ggplot
ggplot(data = gwl) + 
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE))

# let's break down what we just did: anatomy of a ggplot
# ggplot(data = <<DATA>>) +
#   <<geom_FUNCTION>>(mapping = aes(<<MAPPINGS>>))

# access a column of a dataframe with $
# gwl$SITE_CODE

# get colnames of your dataframe from colnames() to 
# copy/paste
colnames(gwl)

# let's add complexity to our ggplot
ggplot(data = gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.2)

# let's use geom_area()
ggplot(gwl) +
  geom_area(aes(MSMT_DATE, GSE_WSE))

# add a line over our points
ggplot(data = gwl) + # layer 1
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.2) +
  geom_line(aes(MSMT_DATE, GSE_WSE), color = "red", alpha = 0.7)

# order matters! ggplots are built in layers
ggplot(data = gwl) + # layer 1
  geom_line(aes(MSMT_DATE, GSE_WSE), color = "red", alpha = 0.7) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.2) 

# add another layer that shows a trendline
ggplot(data = gwl) + # layer 1
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.2) +
  geom_line(aes(MSMT_DATE, GSE_WSE), color = "red", alpha = 0.7) +
  geom_smooth(aes(MSMT_DATE, GSE_WSE), method = "lm") +
  labs(title = "Groundwater level in Sacramento Co.",
       x = "", y = "Depth below ground surface (ft)")


# let's make a histogram
ggplot() +
  geom_histogram(data = gwl, aes(GSE_WSE), 
                 binwidth = 5)

# use binwidth and bins arguments to change appearance
ggplot() +
  geom_histogram(data = gwl, aes(GSE_WSE), 
                 bins = 50, fill = "blue")

ggplot(gwl) +
  geom_boxplot(aes(GSE_WSE))


# read in groundwater level data from 10 stations
# in Sacramento and Placer Counties
gwl_10 <- read_csv(here("data/gwl/gwl_10.csv"))

# plot
ggplot(gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = 0.4)

# use a color aesthetic
ggplot(gwl_10) +
  geom_point(aes(x = MSMT_DATE, 
                 y = GSE_WSE, 
                 color = SITE_CODE),
             alpha = 0.4)

# make a faceted plot (facet means "subplot")
dev.off()
ggplot(gwl_10) +
  geom_line(aes(x = MSMT_DATE, y = GSE_WSE)) +
  facet_wrap(~SITE_CODE)

# save a plot by assigning it to a object and using ggsave()
my_plot <- ggplot(gwl_10) +
  geom_line(aes(x = MSMT_DATE, y = GSE_WSE)) +
  facet_wrap(~SITE_CODE)

# ggsave needs to know the location to save a plot, and the object
ggsave(here("figures/my_gwl_plot.png"), my_plot, 
       height = 8, width = 11.5)


# DPLYR -------------------------------------------------------------------

library(here)
library(tidyverse)


stations <- read_csv(here("data/gwl/stations.csv"))



## FILTER ------------------------------------------------------------------
# filter by rows

# filter to only stations in Sacramento County
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento")

# this doesn't return anything
stations_sac_broken <- filter(stations, COUNTY_NAME == "sacramento")

# figure out what the unique values are in a column of text
unique(stations$COUNTY_NAME)
nrow(stations_sac) # number of rows in dataframe (N=494)

# table: this tabulates the number of values per unique category
table(stations_sac$WELL_USE)


# combine conditions to filter by:
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento",
                       WELL_USE == "Residential")
nrow(stations_sac)


# want a list of counties to filter by:
sep_counties <- c("Sacramento", "Placer", "El Dorado")

# use %in%
stations_multcounties <- filter(stations, COUNTY_NAME %in% sep_counties )

table(stations_multcounties$COUNTY_NAME)

# exclude or trim out something (i.e., not-matching)
stations_trim <- filter(stations, !COUNTY_NAME == "Yolo")

stations_trim <- filter(stations, !COUNTY_NAME %in% sep_counties)



## SELECT ------------------------------------------------------------------

# select by COLUMNS

stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME))

station_sel2 <- select(stations, -c(LATITUDE:BASIN_NAME, COUNTY_NAME))

?select() # get help: see helper functions!

stations_sel3 <- select(stations, starts_with("W"), contains("NAME"))



# THE PIPE %>% ------------------------------------------------------------

# the pipe is: %>% (we can use a shortcut Ctrl + Shift + M)
# %>%  # if you run, get error: "Error: unexpected SPECIAL in "%>%"

# first_result and pipe or pass to the next function

# first part filter
stations_multcounty1 <- filter(stations, COUNTY_NAME %in%
                                 sep_counties)

# second part select
stations_multcounty2 <- select(stations_multcounty1, starts_with("W"), contains("NAME"), contains("ID"))

# third part rename a column: new name = old name
stations_multcounty3 <- rename(stations_multcounty2, station_id = STN_ID)

# filter, select, rename all in one:
stations_multcounties <- stations %>% 
  filter(COUNTY_NAME %in% sep_counties) %>% 
  select(starts_with("W"), contains("NAME"), contains("ID")) %>% 
  rename(station_id=STN_ID)

# CHALLENGE:

# filter to only Residential wells that have a WELL_DEPTH > 1000 feet.
# select only STN_ID, WELL_DEPTH, WELL_NAME, BASIN_NAME, and COUNTY_NAME columns in the dataframe.
# how many records are in Los Angeles?

# save to the environment
la_gw <- stations %>% 
  filter(WELL_USE == "Residential", WELL_DEPTH >= 1000) %>% 
  select(c(STN_ID, WELL_DEPTH, WELL_NAME, BASIN_NAME, COUNTY_NAME)) %>% 
  filter(COUNTY_NAME == "Los Angeles")
# write out:
write_csv(la_gw, file = "data_output/la_gw_depths_greater_than_1000.csv")

# write out to csv
stations %>% 
  filter(WELL_USE == "Residential", WELL_DEPTH >= 1000) %>% 
  select(c(STN_ID, WELL_DEPTH, WELL_NAME, BASIN_NAME, COUNTY_NAME)) %>% 
  filter(COUNTY_NAME == "Los Angeles") %>% 
  write_csv(file = "data_output/la_gw_depths_greater_than_1000.csv")


# MUTATE ------------------------------------------------------------------

# add new columns to a dataframe

stations_mutate1 <- stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048)

stations_mutate1 %>% 
  # passing the stations above to ggplot below
  ggplot() + 
  geom_point(aes(x = STN_ID, y = WELL_DEPTH), color = "cyan4", alpha=0.5) +
  geom_point(aes(x = STN_ID, y = WELL_DEPTH_m), color = "salmon", pch=21, alpha=0.5)


# GROUP BY  ---------------------------------------------------------------


n_by_county <- stations %>% 
  group_by(COUNTY_NAME) %>% 
  count() %>% 
  rename(no_gw_stations = n)

head(n_by_county)


# SUMMARIZE DATA (need to use group_by)

stations %>% 
  group_by(COUNTY_NAME) %>% # grouped by COUNTY
  summarize(mean_well_depth = mean(WELL_DEPTH, na.rm=TRUE)) %>% # mean depth
  arrange(desc(mean_well_depth)) %>% # sorted the data by mean well depth
  head(10) %>% # took the largest 10 rows
  ggplot() + # plot
  geom_col(aes(x= COUNTY_NAME, y= mean_well_depth)) +
  labs(title = "Mean Well Depth (ft)", 
       subtitle = "Top 10 Counties with Deepest Wells")

# why use na.rm?
summary(stations) # a way to evaluate data 

# Filtering Out NAs
stations %>% 
  group_by(COUNTY_NAME) %>% # grouped by COUNTY
  summarize(mean_well_depth = mean(WELL_DEPTH, na.rm=TRUE)) %>% # mean depth
  filter(!is.na(mean_well_depth)) %>% 
  # pass to ggplot
  ggplot() + 
  geom_col(aes(x = fct_reorder(COUNTY_NAME, mean_well_depth), 
               y = mean_well_depth)) +
  labs(title = "Mean well depth", 
       subtitle = "Periodic groundwater level database", 
       y = "Mean well depth (ft)",
       x = "") +
  coord_flip()



# BOXPLOT WITH SUMMARIZE --------------------------------------------------

stations %>% 
  group_by(COUNTY_NAME, WELL_USE) %>% 
  summarize(mean_well_depth = mean(WELL_DEPTH, na.rm=TRUE),
            total_records = n() ) %>% 
  filter(total_records > 10, !is.na(mean_well_depth)) %>% 
  ggplot() + 
  geom_boxplot(aes(x = WELL_USE, y = mean_well_depth), 
               fill = "seagreen", alpha = 0.5) +
  labs(title = "Well Depth for Groundwater Stations in CA",
       subtitle = "For groups with >10 stations",
       x = "Well Use", 
       y = "Mean Well Depth (ft)")







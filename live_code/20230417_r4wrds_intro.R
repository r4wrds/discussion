# 2023-04-17 and 2023-04-19

# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Welcome to the Intro Course! (2023-04-17)

print("Good morning!")

# A comment to test!

# to run code in a script: 
# we can Ctrl + Enter on the line or highlighted lines
# or use the button "Run" at top right

# Setting up a Project ----------------------------------------------------

# to make a new section, you can type the # some stuff and then at least 4 hyphens

# the other shortcut is Ctrl + Shift + R

# Making our Directories --------------------------------------------------

# we can use the click button option to create a new directory (folder)
# or we can use some code

dir.create(path = "data") # warning is ok if already exists!

# note this uses a "RELATIVE PATH" which is relative to the .Rproj file
# a hard path would the full path to our file or directory
# getwd()
# setwd("SOME/LONG/PATH/THAT/YOU/DONTHAVE")


# Download Data -----------------------------------------------------------


# downloads the file (data.zip) to your data directory
download.file(url = "https://github.com/r4wrds/r4wrds-data-intro/raw/main/data.zip", destfile = "data/data.zip")
# check help for default arguments
?download.file # then hit Ctrl + Enter on this line

# unzip the data we need for the course:
unzip(zipfile = "data/data.zip")


# Saving environment or data ----------------------------------------------
#pebble <- 5
#rock <- 10
#save(list = c(pebble, rock), file = "myfile.rda")



# module 4: importing and exporting data ----------------------------------

# an example of a function
sum(1, 4)

seq(from = 0, to = 10, by = 1)
0:10


# read in files

library(readr)
stations <- read_csv(file = 'data/gwl/stations.csv')
head(stations)
print(stations, width = Inf) # this is so you can see all of the columns

# oriented with the data frame
nrow(stations)
ncol(stations)
dim(stations)
class(stations)

# there are multiple ways to view your dataframe
View(stations) # in code
# go to environments tab and select the spreadsheet icon
# can also look at files in the file pane


# you wantt o look at acolumn
stations$WELL_TYPE
stations$WELL_TYPE[1] #just the first value in that vector
stations$WELL_TYPE[1:10] 
table(stations$WELL_TYPE) # count the number of categories

library(readxl)
ces <- read_xlsx(path = 'data/calenviroscreen/ces3results.xlsx')
ces

# to read in a different sheet, need to specify
metadata <- read_xlsx('data/calenviroscreen/ces3results.xlsx', sheet = 2, skip = 6)



# exporting data ----------------------------------------------------------
# create a new folder called data_output
dir.create('data_output')

write_csv(x = stations, file = 'data_output/my_stations.csv')
write_csv(stations, 'data_output/my_stations.csv')
# function: a "verb" that changes your inputs into outputs
# argument: within a function, defines the inputs
# data.frame: like a spreadsheet, a type of class in R
# vector: like a column, located in a data.frame. e.g. stations$WELL_TYPE
# element: like a cell, a value in a vector


# to read in the shapefile
unzip('data/shp/sac_county.zip', exdir = 'data/shp/sac')

library(sf)
sac_county <- st_read(dsn = 'data/shp/sac/sac_county.shp')
plot(sac_county)
mapview::mapview(sac_county) # to get a specific function with a package, can use ::
head(sac_county)

# I can export. should actaully export to data_output!
st_write(obj = sac_county, dsn = 'data/shp/sac/sac_county_export.shp')




# module 5: data visualziation --------------------------------------------

library(tidyverse) # loads ggplot along with others
gwl <- read_csv('data/gwl/gwl.csv')

# if you read in with read.csv
gwl_dataframe <- read.csv('data/gwl/gwl.csv')
head(gwl_dataframe$MSMT_DATE)
class(gwl_dataframe$MSMT_DATE) # this is a character... uh-oh
class(gwl$MSMT_DATE) # this is a date. great!

head(gwl)
gwl
names(gwl)
unique(gwl$SITE_CODE) # pulls up unique vales in a vector

ggplot(data = gwl) + # baseplot is created, dataset is defined up here
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE)) #x, y

# we can make a scatterplot
ggplot(data = gwl) + # data gets defined up here
  geom_point(aes(MSMT_DATE, GSE_WSE), alpha = .2) + # x and y defined
  geom_line(aes(MSMT_DATE, GSE_WSE), color = 'red') # need to define x and y again here


# this is the same thing, but actually less verbose
ggplot(data = gwl, aes(x = MSMT_DATE, y = GSE_WSE)) +
  geom_point(alpha = .2) +
  geom_line(color = 'blue', alpha = .5)

# can easily flip x and y...if your data beckon it!
ggplot(data = gwl, aes(y = MSMT_DATE, x = GSE_WSE)) +
  geom_point(alpha = .2) +
  geom_line(color = 'blue', alpha = .5)

# can plot a histogram
ggplot(data = gwl) +
  geom_histogram(aes(GSE_WSE), bins = 100)


# challenge: 
# 1) make the histogram blue (hint: fill = 'blue')
ggplot(data = gwl) +
  geom_histogram(aes(GSE_WSE), bins = 100, color = 'yellow', fill = 'blue')


# aesthetics
gwl_10 <- read_csv('data/gwl/gwl_10.csv')
gwl_10

# scatterplot!
ggplot(data = gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE))

# add in color. to make each site a different color, make sure 'color' is included within aes()
ggplot(data = gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE, color = SITE_CODE))

# can color a plot by a continuous variable
ggplot(data = gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE, color = WELL_DEPTH))

# boxplots
ggplot(data = gwl_10) +
  geom_boxplot(aes(x = SITE_CODE, y = GSE_WSE))
 
# swith the axes to make axis labels more visible
ggplot(data = gwl_10) +
  geom_boxplot(aes(y = SITE_CODE, x = GSE_WSE))

# the differenr ways to vew a dataframe
# view(df)
# in evironments tab
# in files tab

# to see all of the packages in your session, `sessionInfo()`




# load the readr package
library(readr)
gwl_10 <- read_csv('data/gwl/gwl_10.csv')
View(gwl_10)

# load tidyverse
library(tidyverse)

# look at the dataframe
head(gwl_10)
gwl_10
names(gwl_10)
colnames(gwl_10)


# plot gse_wse vs time for all of the stations
unique(gwl_10$SITE_CODE) # these are the 10 different sitecodes
ggplot(data = gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE), color = 'blue')

# why sitecode doesn't need to be in quotes
ggplot(data = gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE, color = 'SITE_CODE'))

# this works bc sitecode is being referred to as a vector inside of gwl_10
# variables inside of aes() don't need to be in quotes
ggplot(data = gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE, color = SITE_CODE))

# can map color ot continuous variable
ggplot(data = gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE, color = WELL_DEPTH))

# boxplots
ggplot(data = gwl_10) +
  geom_boxplot(aes(x = GSE_WSE, y = SITE_CODE, color = WELL_USE)) +
  labs(x = 'Depth to groundwater (ft)',
       y = '',
       title = 'Depth to groundwater at 10 monitoring sites',
       subtitle = 'Sacramento and Placer county (1960 - present)',
       caption = 'Source: periodic groundwater level database, CA-DWR',
       color = 'Well type')

# a way to change the site code values
# easiest way is to create a new column in gwl_10
gwl_10$SITE_CODE # this can be changed in the dataframe using mutate. Rich will cover that. 

# faceting
ggplot(data = gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE)) +
  facet_wrap(~ SITE_CODE, nrow = 5)

# scales can change fixed or free
ggplot(data = gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE)) +
  facet_wrap(~ SITE_CODE, ncol = 5, scales = 'free_y')

# not that useful of a figure. has blank facets
ggplot(data = gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE)) +
  facet_grid(COUNTY_NAME ~ SITE_CODE, scales = 'free_y')

ggplot(data = gwl_10) +
  geom_point(aes(MSMT_DATE, GSE_WSE, color = SITE_CODE)) +
  facet_grid(COUNTY_NAME ~ WELL_USE, scales = 'free_y')

# challenge
# 1) modify the plot above to color by well use (hint: change color = WELL_USE inside of aes())
ggplot(data = gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE)) + # modify this line
  facet_grid(COUNTY_NAME ~ WELL_USE, scales = 'free_y')

# 2) how does the plot change if you add group = SITE_CODE inside of aes()?
ggplot(data = gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE, group = SITE_CODE)) + # modify this line
  facet_grid(COUNTY_NAME ~ WELL_USE, scales = 'free_y')

# omit group
ggplot(data = gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE)) + # modify this line
  facet_grid(COUNTY_NAME ~ WELL_USE, scales = 'free_y')



# 3) can you explain what group does?
# group will group the lines by the variable 




# save plots --------------------------------------------------------------

myplot <- ggplot(data = gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE, group = SITE_CODE)) + # modify this line
  facet_grid(COUNTY_NAME ~ WELL_USE, scales = 'free_y')


# one way to save a plot
dir.create('figures')

pdf('figures/my_plot.pdf')
myplot
dev.off()

# can plot multiple plots in a single pdf
pdf('figures/my_plot_duplicated.pdf') 
myplot
myplot
dev.off()

# if i want to overwrite, can just do it again
pdf('figures/my_plot_duplicated.pdf') 
myplot
myplot
myplot
dev.off()

# can use this abovve method to plot png
# png('mypath.png')

# using ggsave
ggsave(filename = 'figures/my_plot_ggsave_2.pdf', plot = myplot)

# can define the dimensions
ggsave(filename = 'figures/my_plot_ggsave_2.pdf', plot = myplot, width = 6, height = 4)

# can chang the color pallete using scale_
myplot +
  scale_color_viridis_d()

ggplot(data = gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = MSMT_DATE, group = SITE_CODE)) +
  scale_color_viridis_c(option = 'A', end = .95) # option changes the palette, end changes the end range of the palette


ggplot(data = gwl_10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = MSMT_DATE, group = SITE_CODE)) +
  scale_color_viridis_c(option = 'A', end = .95) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank()) # removes minor grid lines


# to restart your R session, and clear the environemnt
# go to Session > Restart R

install.packages("cowsay")
library(cowsay)
names(cowsay::animals)
say(what = "Hello r4wrds, welcome to data structures!", 
    by = "mushroom")




# Module 6: Data Structures -----------------------------------------------
library(tidyverse)

# assignment is how we bind variable names to data

# an example of assignment
stations <- read_csv("data/gwl/stations.csv")

# let's inspect the structure of the stations dataframe
str(stations)

summary(stations) 
median(stations$WELL_DEPTH, na.rm = TRUE)


# Basic Object Classes ----------------------------------------------------

# basic object classes in R are: numeric, character, logical, factor

# check the class of an object
class(stations)

# check the classes of cols
class(stations$SITE_CODE)
class(stations$LATITUDE)

# create vectors and check their classes

# data.frames are a COMBINATION of vectors
stations$STN_ID # is a vector
stations$SITE_CODE # is a vector

# and so on...
# this implies that each column of a data.frame is a
# vector, and a data.frame is a combination of vectors

# each vector is a set of **one** object class()

# to create vectors, we use the c() function, which
# stands for "concatenate"
dry <- c(TRUE, FALSE, TRUE)
flow <- c(0, 57, 128)
date <- c("July", "January", "February")

# combine these vectors into a data.frame
df <- data.frame(dry, flow, date)



# Module 7: Wrangling Data with dplyr -------------------------------------

library(tidyverse)

# dpyr verbs (functions) work on data.frames. we focus on:
# select(): select a column
# filter(): keep/remove rows based on a condition
# rename(): rename a column
# arrange(): sort a column by ascending/descending order
# group_by() and summarize(): taking grouped summaries

# load in stations
stations <- read_csv("data/gwl/stations.csv")

# filter() rows: find all stations in Sacramento County
sort(unique(stations$COUNTY_NAME))

# == means "is equal to", single = means assignment
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento")
unique(stations_sac$COUNTY_NAME)

# an aside on assignment ------------------------------
# what's the deal with <- vs = for assignment
# <- is to bind an object to a variable name
# = is to assign argument names in a function 

# logical operators:
# == means "is equal to"
# ! means "not", e.g., != means "not equal to"
# %in% means "is in"
# the | means "or"
# & means "and"

# stations in Sacramento, Placer, or El Dorado Counties

# slow way with ==
df1 <- filter(stations, 
              COUNTY_NAME == "Sacramento" | 
              COUNTY_NAME == "Placer" |
              COUNTY_NAME == "El Dorado")
unique(df1$COUNTY_NAME) # test
nrow(df1) # 723 rows in stations that meet conditions above
nrow(stations)

# logical tests
nrow(df1) == nrow(stations)
ncol(df1) == ncol(stations)
nrow(df1) != nrow(stations)

# pro way with %in%
sep_counties <- c("Sacramento", "Placer", "El Dorado")
df2 <- filter(stations, COUNTY_NAME %in% sep_counties)
unique(df2$COUNTY_NAME)
nrow(df2) == nrow(df1)



# select columns ----------------------------------------------------------

# we want to keep only STN_ID, LATITUDE, LONGITUDE

stations_sel <- select(stations, STN_ID, LATITUDE, LONGITUDE)
ncol(stations_sel)

# can also use select to drop columns with the - operator
stations_sel2 <- select(stations, -c(SITE_CODE:WELL_NAME))
stations_sel3 <- select(stations, -c(SITE_CODE:WELL_NAME, BASIN_CODE))

# warning advanced material: this may be confusing. logical test. 
colnames(stations)[ ! colnames(stations) %in% colnames(stations_sel3) ]


# the pipe ----------------------------------------------------------------

# use starts_with() vs writing multiple colnames
# rename(dataframe, new_colname = old_colname)

# the slow way
df1 <- filter(stations, COUNTY_NAME == "Sacramento")
df2 <- select(df1, starts_with("WELL"), COUNTY_NAME) 
df3 <- rename(df2, name = WELL_NAME) 

# the pro way (with the pipe, %>%) - pipe shortcut "Control + Shift + M"
# the pipe moves the LHS into the function as the first argument
# x %>% function() is the same as function(x)
nrow(stations)
stations %>% nrow()

length(unique(stations$COUNTY_NAME))
stations$COUNTY_NAME %>% unique() %>% length()

# the pro way
df4 <- stations %>% 
  filter(COUNTY_NAME == "Sacramento") %>% 
  select(starts_with("WELL"), COUNTY_NAME) %>% 
  rename(name = WELL_NAME) 
  


# mutate adds new columns -------------------------------------------------

stations %>% 
  mutate(well_depth_m = WELL_DEPTH * 0.3048) %>% 
  select(STN_ID, well_depth_m, WELL_DEPTH)
  

# pipe into a ggplot
stations %>% 
  mutate(well_depth_m = WELL_DEPTH * 0.3048) %>% 
  select(STN_ID, well_depth_m, WELL_DEPTH) %>% 
  ggplot() +
  geom_point(aes(STN_ID, WELL_DEPTH), 
             color = "cyan4", alpha = 0.5) +
  geom_point(aes(STN_ID, well_depth_m),
             color = "maroon", alpha = 0.5)

# Live Code for 2023-01-23 R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Welcome to the Intro Course! (2023-01-25)

# Check your working directory with
getwd()#

# load in our packages
library(readr)
library(ggplot2)

# read in data
gwl10 <- read_csv("data/gwl/gwl_10.csv")
# dimensions
dim(gwl10)
colnames(gwl10)
# index columns with $, index = extract one item
gwl10$SITE_CODE
# MSMT DATE: measurement date
# WSE: water surface elevation
# GSE_WSE - depth to groundwater

# indexing = extracting an item, $ for column, [#]
gwl10$WELL_USE[100:150]
# second indexing approach: dataframe[row,column]
# columns 1-5, rows 1-10
gwl10[1:10,1:5]
# why index? work with more specific parts of the data
indexed_data <- gwl10[1:10, 1:5]

# 3 core of a ggplot: 1) data, 2) geometric shape, 3) aesthetic mapping

ggplot(data = gwl10) +
  geom_point(mapping = aes(x = MSMT_DATE, y = GSE_WSE), alpha = 0.5, color = "violet")

# geom_point: continuous x continuous (same with geom_line)
# histograms: 1 cont.
ggplot(gwl10) +
  geom_histogram(aes(x = GSE_WSE))
# default is 30 bins
ggplot(gwl10) +
  geom_histogram(aes(x = GSE_WSE), bins = 100)

# Our trend is not quite clear: why?
ggplot(gwl10) +
  # put data attributes inside aes argument
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE, color = SITE_CODE), alpha = 0.5)

# boxplots: distributions categorical x cont.
# load in tidyverse to get a wider set of packages and functions
library(tidyverse)
# use fct_order() function WITHIN ggplot to reorder variables by the median
ggplot(gwl10) +
  #boxplots show us the median (center line), the inter-quartile range within the box, and the whiskers (<1.5x IQ range), with the outliers as points (>1.5x IQ range)
  geom_boxplot(aes(x = GSE_WSE, y = fct_reorder(SITE_CODE, GSE_WSE), color = WELL_USE)) +
  # add labels
  labs(x = "Depth to groundwater (ft)",
       y = "Site", color = "Well type",
       title = "Depth to groundwater at 10 sites",
       subtitle = "Sacramento and Placer county (1960-present")

# Sub-plots: facets: facet_wrap
ggplot(gwl10) +
  geom_line(aes(MSMT_DATE, GSE_WSE)) +
  facet_wrap(~SITE_CODE, ncol = 2, scales = "free")
?facet_wrap

# Color
library(viridis)
ggplot(gwl10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE)) +
  facet_wrap(~SITE_CODE, ncol = 2, scales = "free") +
  scale_color_viridis_d() # _d discrete/categorical

colnames(gwl10)
my_plot <- ggplot(gwl10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_DEPTH)) +
  facet_wrap(~SITE_CODE, ncol = 2, scales = "free") +
  # If you mismatch _d with a continuous color variable, you will get an error
  scale_color_viridis_c() # _c continuous

# Notes: 
## warnings are okay (usually), they just tell you something is up
# errors are more serious (something went wrong and cannot run)
## Tidyverse warning of conflicts: this is okay, but it tells you about how some packages have function with the same names

dir.create("figures")
# defaults a width and height that is the size of your viewer pane
ggsave("figures/my_plot_ggsave.pdf", my_plot)
# specify dimensions we want
?ggsave
ggsave("figures/my_plot_ggsave.pdf", my_plot, height = 10, width = 6)

# rows ~ column
?facet_grid
# facet_grid: facet by two variables
ggplot(gwl10) +
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE, group = SITE_CODE)) +
  facet_grid(COUNTY_NAME~WELL_USE, scales = "free") +
  theme_minimal() +
  # theme function allows you to change positions of just about anything in the plot
  theme(legend.position = "top")



# 7. dplyr ----------------------------------------------------------------

install.packages("cowsay")
cowsay::say("hello")

library(tidyverse)

# tidyverse, by default, loads dplyr

# load groundwater stations data
stations <- read_csv("data/gwl/stations.csv")


# filter() rows -----------------------------------------------------------

# how many rows are in the stations dataframe
nrow(stations)

# sorted vector of county names
sort(unique(stations$COUNTY_NAME))

# return the rows in Sacramento county
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento")
nrow(stations_sac)

# tabulate (count) all the unique values of a column
# in a dataframe with the table() function
table(stations_sac$WELL_USE)

# quick aside on $ operator: dollar sign operator allows
# access to a **column** in a **dataframe**
class(stations_sac)           # dataframe
class(stations_sac$WELL_USE)  # column in a dataframe
head(stations_sac$WELL_USE)   # column in a dataframe

# we anticipate 76 rows in the resulting dataframe after
# filtering by County Sacramento and well use Residential
stations_sac <- filter(stations, COUNTY_NAME == "Sacramento", WELL_USE == "Residential")

# let's further convince ourselves this works:
unique(stations_sac$COUNTY_NAME)
unique(stations_sac$WELL_USE)

# yay filter() works!

# filter by multiple values with %in% operator
# (because the == operator is for one value)
# first create a string vector with the c() operator
sep_counties <- c("Sacramento", "El Dorado", "Placer")
sep_multcounties <- filter(stations, COUNTY_NAME %in% sep_counties)

# convince ourselevs it works
unique(sep_multcounties$COUNTY_NAME)
table(sep_multcounties$COUNTY_NAME)

# to negate, use the ! operator (means not equal to)
stations_trim <- filter(stations, COUNTY_NAME != "Yolo")


# select() columns from a dataframe ---------------------------------------

# select 4 columns from the dataframe by colname
stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME))
ncol(stations_sel1)

# remove columns using the - operator (subtract those cols)
stations_sel2 <- select(stations, -c(LATITUDE:BASIN_NAME, COUNTY_NAME))
ncol(stations_sel2)

# demo helpers starts_with() and contains()
stations_sel3 <- select(stations, starts_with("WELL"), contains("NAME"))
colnames(stations_sel3)



# the pipe operator: chaining dplyr verbs together ------------------------

# pipe means: take left hand side object, and put it into
# the function on the right hand side as the 1st argument
stations$WELL_USE %>% table()
# same thing as
table(stations$WELL_USE)

# practical example: avoid creating intermedaite variables and reduce code -- SIMPLIFY

# filter
stations_multcounty1 <- filter(stations, COUNTY_NAME %in% 
                                 c("Sacramento", "Placer"))

# select
stations_multcounty2 <- select(stations_multcounty1, 
                               starts_with("W"), 
                               contains("NAME"), 
                               contains("ID"))

# rename the STN_ID to station_id: rename(new_col_name, old_col_name)
stations_multcounty3 <- rename(stations_multcounty2, station_id = STN_ID)

# do the same thing above with the pipe
stations_mutcounty <- stations %>% 
  filter(COUNTY_NAME %in% c("Sacramento", "Placer")) %>% 
  select(starts_with("W"), contains("NAME"), contains("ID")) %>% 
  rename(station_id = STN_ID) # rename(new_col_name = old_col_name)

# same as above, extremely hard to read
rename(select(filter(stations, COUNTY_NAME %in% c("Sacramento", "Placer")), starts_with("W"), contains("NAME"), contains("ID")), station_id = STN_ID)

# sanity check: same dataframe
nrow(stations_multcounty3) == nrow(stations_mutcounty)


# mutate() creates a new column -------------------------------------------

# convert well depth to meters with mutate() and
# piping into a ggplot
stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048) %>% 
  ggplot() +
  geom_point(aes(x = STN_ID, y = WELL_DEPTH), color = "cyan4", alpha = 0.5) +
  geom_point(aes(x = STN_ID, y = WELL_DEPTH_m), color = "maroon", alpha = 0.5)



# group by and summarize --------------------------------------------------

# count observations per group
stations %>% 
  count(COUNTY_NAME, sort = TRUE)

# plot mean (avg) well depth for the top 10 deepest well counties
stations %>% 
  group_by(COUNTY_NAME) %>% 
  summarise(mean_well_depth = mean(WELL_DEPTH, na.rm = TRUE)) %>% 
  arrange(desc(mean_well_depth)) %>% 
  head(10) %>% # take top 10 rows
  ggplot() +
  geom_col(aes(x = COUNTY_NAME, y = mean_well_depth))


# make it nicer and add all counties
stations %>% 
  # group by the county name
  group_by(COUNTY_NAME) %>% 
  # calculate the mean well depth
  summarize(mean_well_depth = mean(WELL_DEPTH, na.rm = TRUE)) %>% 
  # remove two counties that have NA values
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

# to write tables
stations %>% 
  # group by the county name
  group_by(COUNTY_NAME) %>% 
  # calculate the mean well depth
  summarize(mean_well_depth = mean(WELL_DEPTH, na.rm = TRUE)) %>% 
  write_csv("data_output/my_summarized_data.csv")

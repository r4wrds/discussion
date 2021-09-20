# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

print("hellooooo everyone!")


# Intro to RStudio Panels -------------------------------------------------

# this is a comment
89 * 100
# to evaluate code, we can use Ctrl + Enter from a script

# assignment  "<-"
stream <- 4
pebble <- 8

# can use these variable
stream + pebble


library(tidyverse)

# to use a single function that might have a conflict
# we use explicitly library::function
dplyr::filter()


# Project Management ------------------------------------------------------

# working dir
getwd()

# change this to your working directory
setwd("C:/MyUserName/My_Documents/A_Folder_You_May_Have/But_This_One_You_Definitely_Dont/")

# relative links:
"data/"

# use relative paths with the here package
#install.packages("here")
library(here)

# use relative paths with tab autocomplete
read.csv(here("data/calenviroscreen/ces3results_data.csv"))

read.csv(here("data", "calenviroscreen/ces3results_data.csv"))

# always starting with a clean slate
# explicit import of data
# and export/saving of data in your code


# module 4: import/export -------------------------------------------------

# what's a function?

# install.packages() is a function
# functions take arugments

# a function called sum
sum(1, 2)

# create a sequence from 0 to 10 by 1
seq(0, 10, 1)

seq(1, 100000, 5)

# creating sequences is so common we have a sepcial
# operator :
0:10

# read a csv with tidyverse's readr package, using read_csv()
library(here)
library(readr)

# read a csv of groundwater level monitoring stations
stations <- read_csv(here("data/gwl/stations.csv"))

# read.csv() is different than read_csv(), that latter being
# a bit faster and with better printing methods

# head views the first n rows of the dataframe
head(stations) # first 6 rows
head(stations, 10) # first 10 rows

# let's read a csv from a url
url <- "https://raw.githubusercontent.com/r4wrds/r4wrds-data/main/intro/data/gwl/stations.csv"

# overwrite stations with a new dataframe
stations <- read_csv(url)

# number of columns, rows, and both
ncol(stations)
nrow(stations)
dim(stations)

# ask what the class of the stations object is
class(stations)

# view the stations
View(stations)

# properties of data.frames

# data frames are made of vectors of the same length
# each vector is a column

# access column of the dataframe with the $ operator
stations$WELL_TYPE

# subset the column by position
stations$WELL_TYPE[1] # first entry in column
stations$WELL_TYPE[1:10] # first 10 entries in column

# what are the counts of all the unique well types?
table(stations$WELL_TYPE)


# read excel files
install.packages("readxl") # only need to install a package once
library(readxl)
ces <- read_xlsx(here("data/calenviroscreen/ces3results.xlsx"))

head(ces, 10)
View(ces)

# by default, read_xlsx() only reads the first sheet in a file
metadata <- read_xlsx(here("data/calenviroscreen/ces3results.xlsx"),
                      sheet = 2, skip = 6)

View(metadata)

# read in watershed data and skip some rows
watershed <- read_xlsx(here("data",
                            "healthy_watersheds",
                            "CA_PHWA_TabularResults_170518.xlsx"),
                       sheet = 2, skip = 4)


# read a shapefile of sacramento county
library(sf)
sac_county <- st_read(here("data/shp/sac/sac_county.shp"))

# plot sac county
library(ggplot2)
ggplot(sac_county) + geom_sf()


# write, or export data

# need a place to write data: create directory from within R!
dir.create(here("data_output"))
write_csv(stations,
          here("data_output", "my_stations.csv"))


# write a shapefile
st_write(sac_county, here("data_output",
                          "sac_county.shp"))

# challenge 2: create, write, and read an rds file
breakfast <- "bagel with avocado and egg"
write_rds(breakfast, here("data_output/breakfast.rds"))
read_rds(here("data_output/breakfast.rds"))



# module 5: data visualization with ggplot2 --------------------------------
library(tidyverse)
library(here)

# groundwater level from a single station in Sac County
gwl <- read_csv(here("data", "gwl", "gwl.csv"))

# inspect the data
View(gwl)

# column names of data frame
colnames(gwl)

# make our first ggplot()
# GSE_WSE is the depth to groundwater below land surface
ggplot(data = gwl) +
  geom_line(mapping = aes(x = MSMT_DATE, y = GSE_WSE))

# # anatomy of a gpplot
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(gwl) +
  geom_line(aes(MSMT_DATE, GSE_WSE))

# use a geom_point()
ggplot(gwl) +
  geom_point(aes(MSMT_DATE, GSE_WSE), 
             alpha = 0.3)

# use geom_area()
ggplot(gwl) +
  geom_area(aes(MSMT_DATE, GSE_WSE))

# a slightly more complex plot
ggplot(gwl) +
  geom_point(aes(MSMT_DATE, -GSE_WSE), 
             alpha = 0.3) +
  geom_line(aes(MSMT_DATE, -GSE_WSE), color = "red") 
  
# let's make a histogram
ggplot(gwl) +
  geom_histogram(aes(GSE_WSE), bins = 100)

# challenge 1: 
# colors using strings and hex codes
# google "colors in R"
ggplot(gwl) +
  geom_histogram(aes(GSE_WSE), bins = 100, 
                 fill = "#FF5733")

# a boxplot
ggplot(gwl) + geom_boxplot(aes(GSE_WSE))


# let's now look at 10 groundwater stations
gwl_10 <- read_csv(here("data/gwl/gwl_10.csv"))

# plot
ggplot(gwl_10) +
  geom_point(aes(x = MSMT_DATE, y = GSE_WSE))

# how many stations are we lookin' at? - aside on data
# class and structure
length(unique(gwl_10$SITE_CODE))
class(gwl_10$SITE_CODE)
dim(gwl_10$SITE_CODE)
dim(gwl_10) # dataframes have a property dim
length(gwl_10) # length is number of columns in dataframe
table(gwl_10$SITE_CODE)

# use a color aesthetic
ggplot(gwl_10) +
  geom_point(aes(x = MSMT_DATE, 
                 y = GSE_WSE, 
                 color = SITE_CODE),
             alpha = 0.3)

# can also color by a continuous variable, 
# like the total completed depth
ggplot(gwl_10) +
  geom_point(aes(x = MSMT_DATE, 
                 y = GSE_WSE, 
                 color = WELL_DEPTH),
             alpha = 0.3)

# ggplot allows us to ask question of the data 
# and perform EDA: exploratory data analysis

# what's the depth to water distribution of the 
# 10 sites in our dataset?

# hard to read x axix text because they're long
ggplot(data = gwl_10) +
  geom_boxplot(aes(x = SITE_CODE, y = GSE_WSE))

# use a coord_flip()
ggplot(data = gwl_10) +
  geom_boxplot(aes(x = SITE_CODE, y = GSE_WSE)) +
  coord_flip()

# make more complex
range(gwl_10$MSMT_DATE) # range of the dates

ggplot(data = gwl_10) +
  geom_boxplot(aes(y = SITE_CODE, x = GSE_WSE,
                   fill = WELL_USE)) +
  labs(
    y = "",
    x = "Depth to groundwater (ft)",
    fill = "Well type",
    title = "Depth to groundwater at 10 sites",
    subtitle = "Sacramento and Placer County (1961-present)",
    caption = "Source: Periodic Groundwater level database, CA-DWR."
  )


# facet makes subplots - make a subplot for each
# SITE_ID in the data
ggplot(gwl_10) + 
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE)) +
  facet_wrap(~SITE_CODE) # don't forget tilde!
  


# save some plots
my_plot <- ggplot(gwl_10) + 
  geom_line(aes(MSMT_DATE, GSE_WSE, color = WELL_USE)) +
  facet_wrap(~SITE_CODE) + # don't forget tilde!
  scale_color_viridis_d()
my_plot

# save to pdf by creating device
pdf(here("data_output", "my_plot.pdf")) # open a PDF connection
my_plot # print to the plot to the PDF
dev.off() # close the connection, i.e., save plot

# save with ggsave - 1 step
ggsave(here("data_output", "my_plot_ggsave.png"), 
       my_plot, width = 10, height = 7)

# bespoke plots are a google away!


# module 6: data structures -----------------------------------------------

## skipped


# random fun --------------------------------------------------------------

# some random fun with the "cowsay" package
install.packages("cowsay")
cowsay::say("Why did the fungi move from his studio apartment?\n
            Because there wasn't mushroom.")

# module 7: data wrangling with dplyr -------------------------------------

# load the functions that are in the packages (libraries)
library(here)
library(tidyverse)

# now import some data that we're going to wrangle

# import stations.csv
stations <- read_csv(here("data/gwl/stations.csv"))


## Filtering --------------------------------------------------------------

# let's filter to just stations in Sacramento
unique(stations$COUNTY_NAME)
# match a vector of length 1 (need ==)
stations_sac <- dplyr::filter(stations, COUNTY_NAME == "Sacramento" )

table(stations_sac$WELL_USE)

#stations_sac = dplyr::filter(.data = )
#do something then -> assign the variable name

# we can mix and match and combine things inside filter, comma is like AND
stations_sac <- filter(stations, 
                       COUNTY_NAME == "Sacramento", 
                       WELL_USE == "Residential"
)


# use with multiples or lists
#sep_counties <- c(124, "names", "A") # can't mix datatypes
sep_counties <- c("Sacramento", "Placer", "El Dorado")

# use the %in% to filter with multiples greater than 1
stations_multcounties <- filter(stations, COUNTY_NAME %in% sep_counties)
table(stations_multcounties$COUNTY_NAME)

# exclude everything but Yolo County
stations_trim <- filter(stations, !COUNTY_NAME == "Yolo")
stations_trim <- filter(stations, COUNTY_NAME != "Yolo")
stations_trim <- filter(stations, !COUNTY_NAME %in% c("Yolo", "Placer"))

## dplyr SELECT -----------------------------------------------------------

stations_sel1 <- select(stations, c(STN_ID, LATITUDE, LONGITUDE, COUNTY_NAME))
names(stations_sel1)

# can use : to indicate from something through something (1:4)
stations_sel2 <- select(stations, -c(LATITUDE:BASIN_NAME, WELL_USE))

# select by conditions, like starts_with or contains
stations_sel3 <- select(stations, starts_with("W"), contains("NAME"))


# The Pipe (%>%) ----------------------------------------------------------

# pass data from left side, to the right side of the pipe
# shortcut %>% = Ctrl + Shift + M

# filter and select and rename
stations_multcounty <- stations %>% 
  filter(COUNTY_NAME %in% c("Sacramento", "Placer")) %>% 
  select(starts_with("W"), contains("NAME"), contains("ID")) %>% 
  rename(station_id = STN_ID)


# Mutate ------------------------------------------------------------------

# modifies data
stations_mutate1 <- stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048 )

# specify where the new column goes with .after or .before
stations_mutate1 <- stations %>% 
  mutate(WELL_DEPTH_m = WELL_DEPTH * 0.3048, .after = "WELL_DEPTH")

stations_mutate1 %>% 
  ggplot() + 
  geom_point(aes(x=STN_ID, y=WELL_DEPTH), color="darkblue", alpha=0.5) +
  geom_point(aes(x=STN_ID, y=WELL_DEPTH_m), color="maroon", pch=21, alpha=0.8)


# Group by and Summarize  -------------------------------------------------

# generate an entirely new dataframe 
n_by_county <- stations %>% 
  group_by(COUNTY_NAME) %>% 
  count(sort = TRUE)
head(n_by_county)
class(n_by_county) # check the class type of data
# be aware of grouping
n_by_county %>% ungroup() %>% class # then do something, dropped grouped_df

# arrange
stations %>% 
  group_by(COUNTY_NAME) %>% 
  count() %>% 
  arrange(desc(COUNTY_NAME)) # arrange the data by whatever variable of interest


# summarize and get the top 10 avg depth wells by county
stations %>% 
  group_by(COUNTY_NAME) %>% 
  summarize(mean_well_depth = mean(WELL_DEPTH, na.rm=TRUE)) %>% 
  arrange(desc(mean_well_depth)) %>% # gets the deepest
  head(10) %>% # take top 10 records
  ggplot() + geom_col(aes(x=COUNTY_NAME, y=mean_well_depth)) +
  labs(title = "Mean well depth", 
       subtitle = "Top 10 Counties with deepest wells") + 
  coord_flip()


# module 14: strategies for troubleshooting -------------------------------

# generate an error message
mean[1]
gwl_10$SITE_CODE[1:10] # brackets subset a vector


# understanding class -----------------------------------------------------

# every object has a class
library(tidyverse)

# filter is a function
class(filter)

# class of a integer vector
class(1:10)
class("a string")

my_string <- c("apple", "pear")
class(my_string)
my_string[1]
select[1]  # returns an error because wrong class

# create a dataframe and check class
class(tibble(x = 1:2, y = my_string))



# minimal reprex -----------------------------------------------------

library(tidyverse)
library(here)

# dput() prints data in a nice copy/pastable format
# take first 3 rows of dataframe and first 4 columns
dput(starwars[1:3, 1:4]) # starwars is a built-in dataframe for examples

my_minimal_data <- structure(list(name = c("Luke Skywalker", "C-3PO", "R2-D2"), 
                                  height = c(172L, 167L, 96L), mass = c(77, 75, 32), hair_color = c("blond", 
                                                                                                    NA, NA)), row.names = c(NA, -3L), class = c("tbl_df", "tbl", 
                                                                                                                                                "data.frame"))
my_minimal_data

# sessionInfo returns information about your R session, 
# computer, packages
sessionInfo()
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.7
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] sf_1.0-0        here_1.0.1      forcats_0.5.1   stringr_1.4.0  
# [5] dplyr_1.0.7     purrr_0.3.4     readr_1.4.0     tidyr_1.1.3    
# [9] tibble_3.1.2    ggplot2_3.3.5   tidyverse_1.3.1
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.1.1   haven_2.4.1        colorspace_2.0-2  
# [4] vctrs_0.3.8        generics_0.1.0     utf8_1.2.1        
# [7] rlang_0.4.11       e1071_1.7-7        pillar_1.6.1      
# [10] glue_1.4.2         withr_2.4.2        DBI_1.1.1         
# [13] dbplyr_2.1.1       modelr_0.1.8       readxl_1.3.1      
# [16] fortunes_1.5-4     lifecycle_1.0.0    munsell_0.5.0     
# [19] gtable_0.3.0       cellranger_1.1.0   rvest_1.0.0       
# [22] class_7.3-19       fansi_0.5.0        broom_0.7.8       
# [25] Rcpp_1.0.7         KernSmooth_2.23-20 scales_1.1.1      
# [28] backports_1.2.1    classInt_0.4-3     jsonlite_1.7.2    
# [31] fs_1.5.0           rmsfact_0.0.3      hms_1.1.0         
# [34] stringi_1.7.3      grid_4.1.0         rprojroot_2.0.2   
# [37] cli_3.0.1          tools_4.1.0        magrittr_2.0.1    
# [40] proxy_0.4-26       cowsay_0.8.0       crayon_1.4.1      
# [43] pkgconfig_2.0.3    ellipsis_0.3.2     xml2_1.3.2        
# [46] reprex_2.0.0       lubridate_1.7.10   assertthat_0.2.1  
# [49] httr_1.4.2         rstudioapi_0.13    R6_2.5.0          
# [52] units_0.7-2        compiler_4.1.0    


# Spreadsheets and Pivoting -----------------------------------------------


library(tidyverse)
library(readxl)
library(here)

# get some data:
ces <- read_xlsx(here("data/calenviroscreen/ces3results.xlsx"))

str(ces)
names(ces)

ces_clean <- ces %>% # convert columns incorrectly classed to numeric
  mutate(across(c(`Ozone`:`Pop. Char. Pctl`), as.numeric))

# clean column names
library(janitor)
ces_janitor <- ces %>% clean_names()
names(ces_janitor)

# PIVOTING
# from wide to long
ces_long <- pivot_longer(data = ces_clean, 
                         # pivot on some columns
                         cols = c(`Ozone`:`Pop. Char. Pctl`),
                         names_to = "CES_variable",
                         # name of the column for pivoted values
                         values_to = "CES_values"
                         )

# plot data more easily in 
ces_long %>% filter(`California County` %in% c("Sacramento", "Placer") ) %>% 
  ggplot() +
  geom_col(aes(x=CES_variable, y=CES_values)) +
  facet_wrap(~`California County`) + 
  coord_flip()
  
# PIVOTING BACK TO WIDE
ces_wide <- pivot_wider(data = ces_long, 
                        names_from = "CES_variable",
                        values_from = "CES_values")



# Mapmaking in R ----------------------------------------------------------

# load some libraries
library(sf)
library(here)
library(tidyverse)


# Read in shapefile
sac <- st_read(here("data/shp/sac/sac_county.shp"), quiet=TRUE)

# what about a csv with lat/lon
# groundwater level station data in Sac county
stations <- read_csv(here("data/gwl/stations_sac.csv"))

# convert the simple dataframe to a sf dataframe
# functions in sf almost all start with "st_"
stations <- st_as_sf(stations, 
                     coords = c("LONGITUDE", "LATITUDE"), # X goes first
                     crs = 4269, # projections 
                     remove = FALSE)


# check projection:
st_crs(sac)$epsg
st_crs(stations)$epsg

# transform data to match EPSG (projections)
sac <- st_transform(sac, crs = st_crs(stations))

# check they are the same:
identical(st_crs(stations), st_crs(sac))

# Mapview
library(mapview)

mapview(sac) # plot interactive map
mapview(sac) + mapview(stations)

# if layers don't show up in web browser, change this default setting:
mapviewOptions(fgb = FALSE)

# spatial joins in R

# all stations in CA
all_gw_stations <- read_csv(here("data/gwl/stations.csv")) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), # X goes first
           crs = 4269, # projections 
           remove = FALSE)

plot(all_gw_stations$geometry)

# let's filter to Sacramento:
stations_sac <- stations %>% filter(COUNTY_NAME == "Sacramento")

plot(stations_sac$geometry)

# what if we want to crop or clip data by other spatial data
# best to do this in a projected CRS (coordinate ref system)
all_gw_stations_3310 <- st_transform(all_gw_stations, 3310)
st_crs(all_gw_stations_3310)$epsg

# same projection for our county layer
sac_3310 <- st_transform(sac, 3310)


# perform the intersection/crop
stations_sac_3310 <- st_intersection(
  # first part x is thing you want to crop
  x=all_gw_stations_3310,
  # thing you are cropping by
  y=sac_3310
)

table(stations_sac_3310$COUNTY_NAME)

mapview(stations_sac_3310, zcol="COUNTY_NAME") + mapview(sac)
mapview(stations_sac_3310, zcol="COUNTY_NAME", burst=TRUE) + mapview(sac)

# methods to do spatial joins:
methods(class = "sfc")

# Making a Map

# static maps
plot(stations_sac_3310$geometry)
# interactive with mapview

# another package to check out is {tmap}

# let's make a simple ggplot
(p <- ggplot() + 
    geom_sf(data = sac) +
    geom_sf(data = stations_sac, color="blue", alpha=0.4))

# more complex
(p <- ggplot() + 
  geom_sf(data = sac) +
  geom_sf(data = stations_sac, aes(color=WELL_DEPTH), alpha=0.4) +
  scale_color_viridis_c("Well Depth (ft)"))
p # can wrap in parenthesis to print at same time

#install.packages("ggspatial")
library(ggspatial)

p + 
  # north arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl") +
  ggspatial::annotation_scale(location = "br") +
  labs( x = "Longitude", y= "Latitude", 
        title = "Groundwater monitoring stations",
        subtitle = "Sacramento County",
        caption = "R is great.") + 
  theme_minimal()

dir.create("figures") # create the folder
ggsave(filename = "figures/sac_gw_map.pdf", width = 8, height = 8,
       dpi = 300, units = "in")



# random starwars fun -----------------------------------------------------

starwars %>% 
  sample_n(15) %>%   
  ggplot(aes(height, mass)) + 
  geom_point(aes(color = species)) + 
  ggrepel::geom_label_repel(aes(label = name, fill = species), 
                            alpha = 0.5, max.overlaps = 100) + 
  scale_y_log10() + 
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  # facet_wrap(~species) + 
  labs(y = "Height (cm)", 
       x = "Weight (kg)", 
       color = "Species") + 
  guides(fill = "none") +
  theme_bw() 



# EDA ---------------------------------------------------------------------

# answer just question 1
library(here)
library(tidyverse)
library(sf)
library(mapview)

# read in groundwater level measurements, stations, perforations, and 
# CES data pre-joined 
gwl <- read_rds(here("data/sacramento_gw_data_w_calenviro.rds"))

# What well uses are most common? and where are they located?
gwl %>% # pipe takes everything to the left of the pipe (gwl)
  count(WELL_USE, sort = TRUE) # and inserts it as first arguemnt of next function
  
# pipe above is the same as:
count(gwl, WELL_USE, sort = TRUE)

# visualize well counts with a plot and clean up the plot
p1 <- gwl %>% 
  count(WELL_USE, sort = TRUE) %>% 
  # remove NA well uses
  filter(!is.na(WELL_USE)) %>% 
  ggplot(aes(fct_reorder(WELL_USE, n), n)) +
  geom_col(aes(fill = WELL_USE)) +
  coord_flip() + 
  guides(fill = "none") +
  labs(title = "Monitoring well use", x = "", y = "Count")

# converting groundwater levels to sf object
gwl <- st_as_sf(gwl, 
                coords = c("LONGITUDE", "LATITUDE"),
                crs = 4269, 
                remove = FALSE)

# sacramento county spatial data
sac <- st_read(here("data", "shp", "sac", "sac_county.shp")) %>% 
  st_transform(st_crs(gwl))

# plot of groundwater stations
gwl_stations <- gwl %>% 
  filter(is.na(WELL_USE)) %>% 
  group_by(SITE_CODE) %>% 
  slice(1) # pull the first observation

# out of time, see course website for full example and other questions
# https://www.r4wrds.com/intro/m_exploratory_da#question-1-well-use-and-location
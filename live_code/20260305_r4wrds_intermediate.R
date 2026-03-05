# Live Code for R4WRDS Course: March 3/5th 2026

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.


# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_live_code.R?dl=0

# Live Code ------------------------------------------

# test test test


# https://r4wrds.netlify.app/intermediate/m_interactive_viz

# install.packages('tidygeocoder') # this installs the package
library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!

mapviewOptions(fgb = FALSE)


paste0('I', 'like', 'apples')

form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                    "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")

# take this data from the internet and read it in
dat <- readr::read_csv(form_data) %>% # ctrl shift M for the pipe
  clean_names() %>% 
  rename(dining_name = 3, dining_address = 4)
head(dat)

# these strings need to be geocoded, i.e. strings turned into spatial data
dat$dining_address

dat_geo <- dat %>% 
  geocode(dining_address, method = 'osm')

# look at dat geo
head(dat_geo)
dat_geo$lat

# sf is a vector package in R
dat_geo <- dat_geo %>% 
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c('lat', 'long'), crs = 4326, remove = F)

# oops! coords need to be long, lat!
dat_geo = dat_geo %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326, remove = F) 
dat_geo$geometry  

# map this!
mapview(dat_geo, zcol = 'comfort_using_r', cex = 10)


# mapview is a wrapper package of the package "leaflet"
# if you want to customize your maps more, check out leaflet.


# plotly is another interactive plotting package and allows you to make interactive ggplot figures
ces3_sac <- readRDS('data/ces3_sac.rds')
mapview(ces3_sac, zcol = 'CIscoreP')

head(ces3_sac)


# plot groundwater threats vs. CES score
my_plot <- ces3_sac %>% 
  ggplot( aes(x = gwthreatsP, y = CIscoreP, label = tract)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(x = 'Groundwater Threats (percentiles)', 
       y = 'CI scores', 
       title = 'CES scores vs. Groundwater Threats (Sac county)') + 
  theme_bw()

my_plot

library(plotly)
# install it
install.packages('plotly')
# load it again
library(plotly)

# the plot is now interactive
plotly::ggplotly(my_plot)



# writing function --------------------------------------------------------

# a function is like a verb

cms_to_cfs <- function(discharge){
  res <- discharge * 35.3146662
  return(res)
}

# the function in action
my_value = 10
cms_to_cfs(my_value)

# this function can be applied to a vector of values
discharge_values <- c(1:100)
discharge_values
cms_to_cfs(discharge = discharge_values)

# laod packages
library(tidyverse)

# more info on parameters in functions and when to specify the parms
read_csv('data/nwis_sites_american_river.csv', trim_ws = FALSE)


# other names
# cms_to_cfs (my prefernces)
# CMS_TO_CFS (ok)
# CmsToCfs (ok)
# # avoid: spacing functions with periods, avoid mixing naming conventions
# CMS_toCfs (too crazy)

# setting a default value
cms_to_cfs <- function(cms = 1){
  res <- cms * 35.3146662
  return(res)
}
cms_to_cfs()

# you can have multiple parameters
cms_to_cfs <- function(cms = 1, print = F){
  if(print == T){
    print(cms)
  }
  res <- cms * 35.3146662
  return(res)
}
cms_to_cfs(cms = 10, print = T)


# iterations --------------------------------------------------------------


# https://r4wrds.netlify.app/intermediate/m_iteration

# iterations = doing a function many many tiimes
# for-loops, map()

library(tidyverse)

# load up a few files
eldorado <- read_csv('data/gwl/county/El Dorado.csv')
placer <- read_csv('data/gwl/county/Placer.csv')
sac <- read_csv('data/gwl/county/Sacramento.csv')

# read all of these at once with read_csv
# define paths in a vector
paths <- c('data/gwl/county/El Dorado.csv', 
  'data/gwl/county/Placer.csv',
  'data/gwl/county/Sacramento.csv')
paths
df <- read_csv(paths)
df

# shape of eldorado is smaller than df
dim(eldorado)
dim(df)

# the for-loop way
l <- vector('list', length = length(paths))
l[[1]] # this should say NULL bc it hasnt been assigned

# our first for loop
# start with "for()"
1:length(paths) # 1:3

for(index in 1:length(paths)){ # for i in c(1, 2, 3)
  print(paste("index:", index))
  print(paste("path value:", paths[index]))
  l[[index]] <- read_csv(paths[index]) # the list gets populated with a df each loop
}

# look at l
l # a list

# l can be a df too
bind_rows(l)

# lets make the for loop more complicated by manipulate the data and writing it out as something else

# for actions that should occur once, dont put in the for loop
# create the folder to store the data
fs::dir_create('data/gwl/site_code')

# for loop reads in the data
for(index in 1:length(paths)){ # for i in c(1, 2, 3)
  print(paste("index:", index))
  print(paste("path value:", paths[index]))
  l[[index]] <- read_csv(paths[index]) 
}

# combine all the data, split the data by site code
df <- bind_rows(l)
df_split <- split(df, df$SITE_CODE)
  
# write the dfs out as csv files
glue::glue('data/gwl/site_code/{names(df_split)}.csv')
outpaths = glue::glue('data/gwl/site_code/{names(df_split)}.csv')
# ^^ outpaths = a vector of all the paths to write the data to

# another for loop to write the csv data
for(i in 1:length(outpaths)){
  print(i)
  write_csv(df_split[[i]], outpaths[i])
}



# For loops again ---------------------------------------------------------

library(tidyverse)

# 1) for loop to read in data
files_in <- fs::dir_ls('data/gwl/county/')
files_in
l <- vector('list', length = length(files_in))
l
length(l) == length(files_in) # list container has the same length as the vector files_in

for(i in 1:length(l)){
  print(paste('i:', i))
  print(paste('path:', files_in[i]))
  # read the csv
  l[[i]] <- read_csv(files_in[i], show_col_types=FALSE)
}
l

# 2) outside the for loop, we combined the data, split it
df <- bind_rows(l)
df
# split this by sitecode
df_split <- df %>% 
  split(df$SITE_CODE)
df_split

# 3) created a vector of paths to write out the data
fs::dir_create('data/gwl/site_code')
names(df_split)
files_out <- glue::glue("data/gwl/site_code/{names(df_split)}.csv")
files_out[2]

another_way <- paste0("data/gwl/site_code/", names(df_split), '.csv')
another_way

# 4) for loop we wrote out the data
for(i in 1:length(files_out)){
  print(i)
  # write csv
  write_csv(df_split[[i]], files_out[i])
}


# files_out = vector. to access elements in a vector, use []
files_out[1]

# to access the elements in a list, use [[]]
df_split[[1]]
l[[1]]
class(l[1]) # still a list
class(l[[1]]) # a df bc we accessed the df (the "element" in the list)


# redo that workflow with map ---------------------------------------------

# 1) read in the csvs
files_in
l <- map(.x = files_in, .f = ~read_csv(.x))
l

# 2) bind df and split by site code
l_df <- bind_rows(l)
l_df <- group_split(l_df, SITE_CODE)
l_df

# 3) create the directory, write out the out paths
# already done above

# 4) write out the data
# write_csv(df, path)
# ~ is analogous to function(.x, .y){}
map2(.x = l_df, .y = files_out, ~write_csv(.x, .y))

# the same thing, but EVEN shorter
map(.x = files_in, .f = ~read_csv(.x)) %>% # read in data
  bind_rows() %>% 
  group_split(SITE_CODE) %>% 
  map2(files_out, ~write_csv(.x, .y))

# Parameterized Reporting -------------------------------------------------

# shifting to qmd live code but need these packages:
install.packages("maps")
install.packages("quarto")
install.package("DT")

# create a new quarto document in "docs" folder...may need to create docs
# should have "r4wrds_report.qmd" in docs

# create "quarto_render_all.R" and save in scripts

## Libraries ---------------------------------------------------------------

library(quarto) # for reports
library(tidyverse) # for all the things
library(glue) # for pasting stuff together
library(fs) # for files and paths
library(here) # for paths and directories


# Set Paths ---------------------------------------------------------------

# set paths
userhome <- fs::path_home() # on PC something like C:/username
projhome <- here::here()

# to connect to external drive or OneDrive
# use this r'( )' as a way to paste Windows paths safely (make sure to remove quotes inside the parantheses)

# check path:  only works on my computer
#fullpath <- r'(/Users/rapeek/Documents/RPROJECTS/positron_modeling)'
#fs::file_exists(fullpath)  # should be TRUE if full path

# so to link to external data or folder via R using a relative path:

# link to just our one drive piece, then paste it all together to get full path
# onedrive <- r"(OneDrive - CDFW\)" # copy partial path to JUST the onedrive

# should represent a full path to a folder:
#onedrive_path <- glue("{userhome}/{onedrive}/Terrestrial/Data")
#fs::file_exists(onedrive_path) # should get TRUE
#read_csv(glue("{onedrive_path}/my_data_i_need_from_onedrive.csv"))


## Parameterizing the Reports ---------------------------------------------------

# replace this with whatever you named your quarto report (r4wrds_report.qmd)
report_path <- glue("{projhome}/docs/r4wrds_report.qmd")
report_path
fs::file_exists(report_path)

# data
gwl <- read_rds("data/sac_yolo_sj.rds")
counties  <- unique(gwl$COUNTY_NAME)
class(counties)
class(gwl)
class(gwl$COUNTY_NAME) # why not use this as index?
length(counties)
length(gwl$COUNTY_NAME)

# loop through the vector counties
for (i in counties){
  glue("making report for {i}")
  quarto::quarto_render(
    input = report_path,
    output_file = glue("county_report_{i}.html"),
    execute_params = list(county = i)
  )
}










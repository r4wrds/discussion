# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github here: https://github.com/r4wrds/discussion/tree/main/live_code 

# live code link: https://dl.dropboxusercontent.com/s/tuonv1w7ehw3gkn/r4wrds_r_live_code.R?dl=0

# Welcome to the Intermediate Course!
print("Welcome!")


# Hey Ryan!
print("Hope everyone is doing well!")


# Updating Packages Regularly ---------------------------------------------

# to update packages, we should check regularly (bi-weekly)
# using RStudio, easiest option is Tools > Check for Package Updates


# Downloading Data --------------------------------------------------------

# create a folder or directory
dir.create("data")

# downloads the data.zip file to the `data` directory
download.file(
  "https://github.com/r4wrds/r4wrds-data-intermediate/raw/main/data.zip", 
  destfile = "data/data.zip")

unzip(zipfile = "data/data.zip")

# if __MACOSX folder is created during unzip, delete that directory.
unlink("__MACOSX", recursive = TRUE)


# edit our project-level .RProfile ----------------------------------------

# RProfile is just code that's run when you start up R within a project
# install.packages("usethis") # you may need to install the usethis pkg
usethis::edit_r_profile(scope = "project")

# let's demonstrate how the cowsay package, which is cool, but also
# useless, works
cowsay::say(what = "R is fun!", by = "cow")
cowsay::say(what = "R is fun!")


# build an inspiration quote generator for RProfile -----------------------

library(cowsay) # animal art
library(glue)   # pasting things together with {}

# vector of all animal names exported by the cowsay package
names(cowsay::animals)

# get pieces to make link
repo <- "JakubPetriska/060958fd744ca34f099e947cd080b540"
csv <- "raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"

# get dataframe of inspirational quotes
quotes  <- readr::read_csv(glue("https://gist.githubusercontent.com/{repo}/{csv}"))  

# make a full quote with glue as a new column in the quotes dataframe
quotes$full_quote <- glue("{quotes$Quote} - {quotes$Author}")

# have a random animal say a random full quote 
say(sample(quotes$full_quote, 1), sample(names(cowsay::animals), 1))



# how to install a package ------------------------------------------------

# whenever you have a "missing package" error, the way to fix is with
# install.packages("package_name")
install.packages("usethis")
install.packages("cowsay")
install.packages(c("usethis", "cowsay"))



# .Renviron ---------------------------------------------------------------

# get all environmental variables in R right now
Sys.getenv()

# get a specific environmental variable by name
Sys.getenv("USER")

# set an environmental variable by editing a .Renviron file
usethis::edit_r_environ(scope = "project")

# put this line of code in your .Renviron file and save
ANIMAL = "cow"

# check that we have a ANIMAL env variable
Sys.getenv("ANIMAL")

# apply this to a real-ish example
inspire_me <- function(animal){
  
  # get pieces to make link
  repo <- "JakubPetriska/060958fd744ca34f099e947cd080b540"
  csv  <- "raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"
  
  # silently read dataframe
  suppressMessages(
    quotes  <- readr::read_csv(
      glue::glue("https://gist.githubusercontent.com/{repo}/{csv}")
    )  
  )
  
  # paste together the full quote
  quotes$full_quote  <- paste0(quotes$Quote, " -", quotes$Author)
  
  # make a user-specified animal say the quote
  cowsay::say(sample(quotes$full_quote, 1), by = animal)
  
}

# use the function with an environmental variable
inspire_me(Sys.getenv("ANIMAL"))


# Interactive Visualization -----------------------------------------------

# install.packages("tidygeocoder") 
# if can't install because version not available for 4.2
# try from github repository. Often requires 
# devtools or remotes packages
# install.packages("devtools")
# devtools::install_github("jessecambon/tidygeocoder")
library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)


# the url for the Form data 
form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                   "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")

# read in url and clean the data
dat <- read_csv(form_data) %>% 
  janitor::clean_names() %>% 
  dplyr::rename(dining_name = 3, dining_address = 4)

# geocode to get the lat lon data and add to our data frame
dat_geo <- dat %>%
  tidygeocoder::geocode(dining_address, method = 'osm', lat = latitude , long = longitude)


## now make spatial: we can't have any missing lats/longs
# we also need to transform the data into a spatial "R" format
dat_geo <- dat_geo %>% 
  dplyr::filter(!is.na(latitude) & !is.na(longitude)) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, 
               remove = FALSE)
# check class of data
class(dat)
class(dat_geo)

# Let's make a map!!
mapview(dat_geo, zcol = "comfort_using_r",
        layer.name = "R comfort level",
        cex = 6.5)


# Leaflet Maps ------------------------------------------------------------

# these packages are all for interactive mapping 
library(leafpm)
library(leaflet)
library(leaflet.extras)
library(htmltools)

# set up our map
m <- leaflet() %>%
  # add tiles or the "basemaps"
  addTiles(group = "OSM") %>% # defaults to Open Street Maps
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>% 
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # then we add a single Circle marker for Sacramento
  addCircleMarkers(
    lng = -121.4944, lat = 38.5816, fillColor = "red", 
    color = "black",
    popup = "Sacramento!", group = "Home",
  ) %>% 
  # then add our full dataset (dat_geo)
  addCircleMarkers(
    data = dat_geo, 
    group = "Food & Drink",
    label = ~htmlEscape(first_name),
    popup = glue(
      "<b>Name:</b> {dat_geo$first_name}<br>
      <b>Food_Name:</b> {dat_geo$dining_name}<br>
      <b>Food_Address:</b> {dat_geo$dining_address}<br>
      <b>R comfort (1-10):</b> {dat_geo$comfort_using_r}"
    )
  )  %>% 
  # add controls for the legend and layer names
  addLayersControl(
    baseGroups = c("Toner Lite", "Positron", "OSM"),
    overlayGroups = c("Home", "Food & Drink"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  # add a measure bar
  addMeasure()

# print with 
m

# to save a map, use the "Export" button in the Viewer tab:
# Export > Save as Web Page...

# after you save as an html, best practice to zip or compress the file, and then you can email to collaborators/colleagues. 
# Chrome and Safari tend to do best with these interactive webmaps

# can also save with
# mapview::mapshot()
# htmlwidgets::saveWidget()



# D3 Viz ------------------------------------------------------------------

# Libraries
library(igraph)
library(networkD3)

# create a dataset:
data <- tibble(
  from = c(
    "Dam","Dam","Dam", "Dam",
    "River","River","River", "River","River",
    "Canal", "Canal", 
    "Diversion","Diversion", 
    "Reservoir", "Reservoir","Reservoir",
    "Lake","Lake","Lake", "Lake", 
    "Road","Road","Road",
    "Culvert", "Culvert",
    "Fish", "Fish","Fish",
    "Frog","Frog","Frog",
    "MacroInvertebrates","MacroInvertebrates"
  ),
  to = c(
    "River","Reservoir","Canal","Diversion",
    "Lake","Reservoir","Frog","Fish","MacroInvertebrates",
    "Diversion", "Reservoir",
    "Dam", "River",
    "River","Dam","Fish",
    "Fish","Dam","Frog","MacroInvertebrates",
    "Fish","Dam", "Canal",
    "Road", "Dam",
    "Frog", "River","MacroInvertebrates",
    "Fish", "River", "Lake",
    "River", "Lake"
  )
)

# make a interactive network 
(p <- simpleNetwork(data, height = "600px", width = "600px", 
                    fontSize = 16, fontFamily = "serif",
                    nodeColour = "darkblue", linkColour = "steelblue",
                    opacity = 0.9, zoom = FALSE, charge = -500))

# save the widget
library(htmlwidgets)
saveWidget(p, file = "docs/network_interactive.html")



# Plotly ------------------------------------------------------------------

library(tidyverse)
library(mapview)
mapviewOptions(fgb = FALSE)

# get the data
ces3_sac <- readRDS("data/ces3_sac.rds")

# make a quick mapview map:
mapview(ces3_sac, zcol = "CIscoreP")

# make a ggplot map (STATIC)
ces_plot <- ces3_sac %>% 
  ggplot(aes(x = gwthreatsP, y = CIscoreP, label=tract)) +
  geom_point() +
  geom_smooth(method = "gam") +
  theme_bw(base_size = 12) +
  labs(title = "CES Score vs. Groundwater Threats in Sacramento County",
       subtitle = "Higher CI Score indicates higher threat/impact",
       x = "Groundwater Threats (percentile)", 
       y = "CI Score (percentile)")

ces_plot

# so if we want to explore the data and see what each
# actual point might be (or groundwater station)
# can use the plotly package

# install.packages("plotly")
library(plotly)

ggplotly(ces_plot)

# can click on "Show in New Window" to view fullscreen in your browser


# -------------------------------------------------------------------------
  
# day 2 starts here -------------------------------------------------------

# -------------------------------------------------------------------------
  
# read and write a bunch of files at once with minimal effort

# load some standard data science packages
library(tidyverse)
library(here)
library(fs)

# read in some groundwater level stations with a non-iterative approach
sac <- read_csv(here("data/gwl/county/Sacramento.csv"))
placer <- read_csv(here("data/gwl/county/Placer.csv"))
eldorado <- read_csv(here("data/gwl/county/El Dorado.csv"))

# aside: review ggplot
# ggplot(sac) + geom_point(aes(LONGITUDE, LATITUDE)) 

# do the same thing as above, but with a for loop and R's list() object

# create a vector of file names to read in
files_in <- dir_ls("data/gwl/county/")

# create a "hotel" (list) to store dataframes within, and we need to specify
# the number of "rooms" (elements) in the hotel
l <- vector("list", length = length(files_in))

# inspect the list
length(l)

# subset a list in with [[ , for example, the nth element is l[[n]]
l[[2]]

# loop over all files in data/gwl/county/ (files_in) and 
# read each csv file (read_csv) as an element in the list we 
# just created, (l)
for(i in seq_along(files_in)){
  # each time we see "i" in the loop, it's replaced with the sequence
  l[[i]] <- read_csv(files_in[i])
}

# this is what the loop is doing behind the scenes
# l[[1]] <- read_csv(files_in[1])
# l[[2]] <- read_csv(files_in[2])
# l[[3]] <- read_csv(files_in[3])

# check that the loop worked by inspecting each element of the list
l[[1]] 
l[[2]]
l[[3]]

# another explanation of a for loop - printing numbers 1 to a million
for(i in 1:1000000){
  print(i)
}

# use Control + C in the console to interrupt a running function
# Use Control + L to clear the console 


# combine list elements into ONE BIG dataframe ----------------------------

# use a function from dyplr called bind_rows() to combine a list of 
# dataframes into one big dataframe
ldf <- bind_rows(l)

# we want to write a csv for each unique SITE_CODE
unique(ldf$SITE_CODE) # 723 unique site codes b/c each site is a station

# split the dataframe by the site code, and write them to csv
# split turns a dataframe into a list of dataframes, one for each split var
ldf <- split(ldf, ldf$SITE_CODE) # split by unique SITE_CODE
class(ldf)
length(ldf)

# create a folder to hold output files
dir_create("data/gwl/site_code")

# vector of output files paths
files_out <- glue::glue("{here('data/gwl/site_code')}/{names(ldf)}.csv")

# write site code csvs to the directory we just created with a loop
# in each step of the loop, "i" is replaced by 
# the ith element of the counter, in this case 1:723
for(i in seq_along(files_out)){
  write_csv(ldf[[i]], files_out[i])
  cat("Wrote csv #:", i, "\n")
}
print("DONE!")



# base R functional programming with lapply -------------------------------

# read in eldorado, sac, placer county station dataframes again
l <- lapply(files_in, read_csv) # return a list

# combine the list elements of "l" above into one dataframe
ldf <- bind_rows(l)
ldf <- split(ldf, ldf$SITE_CODE)

# write each site code dataframe to a csv, but with an apply (no loop)
mapply(function(x, y) write_csv(x, y), ldf, files_out)

# we can make the above more clear by naming the function
my_function <- function(df, fp){
  write_csv(df, fp)
}

# apply the named function to write all csvs
mapply(my_function, ldf, files_out)



# Function junction lesson https://www.r4wrds.com/intro/m_function --------

# a function has a name, takes input(s) (argument(s)), and returns something
# write a function that converts ft^3/s to m^3/s, & uses a default conversion factor
cms_to_cfs <- function(discharge_cms, conversion_factor = 35.31){
  # something to do
  discharge_cfs = discharge_cms * conversion_factor
  # something to return
  return(discharge_cfs)
}

# use the function by calling it by name
cms_to_cfs(discharge_cms = 100)

# use a different conversion factor
cms_to_cfs(discharge_cms = 100, conversion_factor = 100)

# argument order matters if unnamed (non-explicit)
cms_to_cfs(60, 7)
cms_to_cfs(conversion_factor = 80, discharge_cms = 7)

# convert a bunch of ft^3/ to m^3/s

# here is our input - let's convert this to cfs
q_cms <- c(24, 2134, 234, 346, 7543, 234, 34634) # think of a column in excel

q_cfs <- cms_to_cfs(q_cms)


# load some functions from a package to read USGS gage data ---------------

library(tidyverse)
library(dataRetrieval)
# install.packages("dataRetrieval")

# pull flow data from a site with a function from the {dataRetrieval} package
flow_site <- readNWISdata(site = "11427000",          # USGS Station ID
                          service = "iv",             # instantaneous measurements
                          parameterCd = "00060",      # discharge in cfs
                          startDate = "2019-10-01",
                          endDate = Sys.Date(),
                          tz = "America/Los_Angeles")

# check that it worked
summary(flow_site)
head(flow_site)

# rename columns to something more inutitive and user-friendly using another
# function from the dataRetrieval package
colnames(flow_site)
flow_site <- renameNWISColumns(flow_site)
colnames(flow_site)

# plot the discharge with ggplot
ggplot(flow_site) +
  geom_line(aes(dateTime, Flow_Inst), color = "darkblue") +
  labs(
    x = "Date", y = "Flow (cfs)", 
    caption = paste("Data acquired on", Sys.Date(), "by {dataRetrieval} package")
  )


# challenge 1  ------------------------------------------------------------

# grab some new data
flow_huc <- readNWISdata(huc = "18020111",
                         service = "dv",
                         parameterCd = "00060", 
                         startDate = "2019-10-01",
                         endDate = "2021-09-30") %>% 
  renameNWISColumns() %>% 
  addWaterYear()

# check the number of unique site numbers
length(unique(flow_huc$site_no))

# same as above, using the pipe to read the functions from left to right
flow_huc$site_no %>% unique() %>% length() # 2 unique site codes

# plot the sites and color the lines by the site_no column
ggplot(flow_huc) +
  geom_line(aes(dateTime, Flow, color = site_no)) + 
  scale_color_manual(values = c("pink", "limegreen")) # set cols manually
  # scale_color_viridis_d(option = "B") # colorblind friendly palette



# create a new function to convert cfs to cms -----------------------------

cfs_to_cms <- function(discharge){
  cms_out = discharge * 0.028316847
  return(cms_out)
}

# apply this function to the findNWISdata() function above, with a pipe
flow_site %>%
  mutate(flow_cms = cfs_to_cms(Flow_Inst)) %>% 
  ggplot() +
  geom_line(aes(dateTime, flow_cms), color = "magenta") +
  labs(
    x = "Date", y = "Flow (cms)", 
    caption = paste("Data acquired on", Sys.Date(), "by {dataRetrieval} package.")
  ) +
  theme_minimal()



# grab some water quality data stations and plot --------------------------

library(sf)

# get NWIS and flowline data 50km downstream of the USGS gage
am <- findNLDI(
  nwis = "11446500", # Below Nimbus Dam
  nav  = c("UM","DM"), 
  find = c("nwis", "flowlines"),
  distance_km = 50)

class(am)
length(am)
names(am)

# plot these interactively with mapview()
library(mapview)
mapview(am[[2]], layer.name = "UM sites", col.regions = "blue") +
  mapview(am[[3]], layer.name = "UM flowlines") +
  mapview(am[[4]], layer.name = "DM sites", col.regions = "green") +
  mapview(am[[5]], layer.name = "DM flowlines") +
  mapview(am[[1]], layer.name = "Origin (Nimbus dam)", col.regions = "red")
  


# joins and binds: https://www.r4wrds.com/intro/m_joins.html --------------

# if you only know one join, it should be the left join

library(tidyverse)
library(here)

# read in 3 dataframes that we can join together on a common key (shared column)
gw_stations <- read_csv("data/gwl/stations.csv")
gw_meas     <- read_csv("data/gwl/measurements.csv")
gw_perf     <- read_csv("data/gwl/perforations.csv")

library(janitor)

# find what columns to merge on
compare_df_cols(gw_stations, gw_meas, gw_perf) %>%
  filter(!is.na(gw_stations) & !is.na(gw_meas) & !is.na(gw_perf))

colnames(gw_stations)
colnames(gw_meas)
colnames(gw_perf)

# join 3 dataframes into 1 massive dataframe on the SITE_CODE column
join_df <- left_join(gw_meas, gw_perf) %>%
  left_join(gw_stations)

# write joined df to a csv for sharing
write_csv(join_df, "data/gwl/joined_gwl_df.csv")




# Live Code for R4WRDS Course

# As we type code, it will update here (each time we save).
# Refresh your browser to get most updated code.

# Live Code for previous courses can be found on Github:
# 2021-08-10: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210810_r4wrds_intro.R
# 2021-08-24: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210824_r4wrds_intro.R 

# ----------------------------------------------------------

# FILE PATHS
# use here(), if you don't have it:
# install.packages("here")

library(here)

getwd() # gives the absolute path
# this won't work on anyone else's computer
# setwd("/Users/rapeek/Dropbox/R_TEACHING/r4wrds_live")

here()

# here allows you to use relative paths
read.csv(here("data", "gwl", "gwl_sac.csv"))

# this is the same thing
read.csv(here("data/gwl/gwl_sac.csv"))



# .Rprofile ---------------------------------------------------------------

# use usethis to edit rprofile, you may need to 
# install.packages("usethis")

# step 1: open and edit your .Rprofile
usethis::edit_r_profile(scope = "project")

# step 2: copy/paste this into your .Rprofile
cowsay::say(what = "hello r4wrds intermediate course!",
            by = "cow")

# step 3: save .Rprofile and restart R 
# verify it works by viewing a cow in your console


# .Renviron ---------------------------------------------------------------

# think of Renviron as holding secrets, keys, and 
# other information specific to your computer/accounts

# let's check out an environmental variable
Sys.getenv("USER")
Sys.getenv() # displays all env variables

# set an environmental variable with {usethis}
usethis::edit_r_environ(scope = "project")

# add an environemntal variable ANIMAL = "cat", 
# save, and restart
Sys.getenv("ANIMAL")


# demo using env vars in a function
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

inspire_me("cat")
inspire_me(Sys.getenv("ANIMAL"))



# save this as scripts/functions.R ----------------------------------------

# list packages in a vector and load them all
pkgs <- c("readr", "cowsay")
purrr::walk(pkgs, require, character.only = TRUE)

# read quotes from a url
f_read_data <- function(url){
  suppressMessages(
    quotes  <- read_csv(url)  
  )
  return(quotes)
}

# paste the quote to the author
f_preprocess_data <- function(d){
  d$full_quote  <- paste0(d$Quote, " -", d$Author)
  return(d)
}

# print a random animal and a random quote
f_inspire_me <- function(d){
  animals <- names(animals)
  say(sample(d$full_quote, 1), by = sample(animals, 1))
}

# install.packages("tidyverse")


# source() ----------------------------------------------------------------

# source() allows us to run an R file from within R!
source(here::here("scripts/functions.R"))


# abstract code into functions --------------------------------------------
# https://www.r4wrds.com/intermediate/m_project_management.html#abstracting-functions-from-code

# make folder to hold functions
dir.create(here::here("scripts/functions"))

# make a new control.R file and paste contents from website

# run code, or source it
source(here::here("scripts/01_control.R"))



# Interactive Viz ---------------------------------------------------------

# if you don't have these, use install.packages()
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

# read in url and clean
dat <- read_csv(form_data) %>% 
  clean_names() %>% # janitor package great for cleaning colnames
  rename( dining_name = 3, dining_address = 4) # rename columns


# data cleaning of data
dat_clean <- dat %>% 
  mutate(dining_address = case_when(
    dining_name == "Midtown Sushi" ~ "2801 P St, Sacramento, CA 95816",
    TRUE ~ dining_address
  ))

# geocode using Open Street Map (osm) API because it's free
dat_geo <- dat_clean %>%
  geocode(dining_address, method = 'osm', lat = latitude , long = longitude)

summary(dat_geo)

# make into sf object so we can map (this breaks if there are NAs)
dat_geo <- dat_geo %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# map!
mapview(
  dat_geo, 
  zcol = "comfort_using_r", 
  layer.name = "R comfort level", 
  cex = 6.5
)

## Leaflet ---------------------------------------------

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
  addCircleMarkers(
    lng = -121.4944, lat = 38.5816, fillColor = "red", color = "black",
    popup = "Sacramento!", group = "Home",
  ) %>% 
  addCircleMarkers(
    data = dat_geo, group = "Food & Drink",
    label = ~htmlEscape(first_name),
    popup = glue(
      "<b>Name:</b> {dat_geo$first_name}<br>
      <b>Food_Name:</b> {dat_geo$dining_name}<br>
      <b>Food_Address:</b> {dat_geo$dining_address}<br>
      <b>R comfort (1-10):</b>{dat_geo$comfort_using_r}"
    )
  )  %>% 
  addLayersControl(
    baseGroups = c("Toner Lite", "Positron", "OSM"),
    overlayGroups = c("Home", "Food & Drink"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addMeasure()

m  



# D3 Interactive Network --------------------------------------------------

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

# Plot
(p <- simpleNetwork(data, height = "600px", width = "600px", 
                    fontSize = 16, fontFamily = "serif",
                    nodeColour = "darkblue", linkColour = "steelblue",
                    opacity = 0.9, zoom = FALSE, charge = -500))



# Plotly ------------------------------------------------------------------

# load CES data for Sacramento county
ces3_sac <- readRDS(here::here("data", "ces3_sac.rds"))

mapview(ces3_sac, zcol = "CIscoreP")



# plot of Groundwater threats vs. CES score
library(cowplot)

(ces_plot <- ces3_sac %>% 
    ggplot(aes(x = gwthreatsP, y = CIscoreP, label = tract)) + 
    geom_point() +
    geom_smooth(method = "gam") +
    cowplot::theme_half_open(font_size = 12) +
    labs(
      title = "CES Score vs. Groundwater Threats in Sacramento County",
      subtitle = "Higher CI Score indicates higher threat/impact",
      x = "Groundwater Threats (percentile)", 
      y = "CI Score (percentile)"
    ))

(tst <- head(ces3_sac))


# how to interact with these points
library(plotly)
ggplotly(ces_plot)



# for loops ---------------------------------------------------------------

library(tidyverse)
library(here)

eldorado <- read_csv(here("data", "gwl", "county", "El Dorado.csv"))
placer   <- read_csv(here("data", "gwl", "county", "Placer.csv"))
sac      <- read_csv(here("data", "gwl", "county", "Sacramento.csv"))

# list all files we want to read in
files_in <- list.files(here("data", "gwl", "county"), full.names = TRUE)

# initialize a list of defined length
l <- vector("list", length = length(files_in))

# loop over all files and read them into each element of the list
for(i in seq_along(l)){
  l[[i]] <- read_csv(files_in[i])
}

# list indexing

# first element of l with double bracket notation
l[[1]]
l[[2]]
l[[3]]


# split a list and loop over it -------------------------------------------

# initialize a list of defined length
l <- vector("list", length = length(files_in))

# loop over all files and read them into each element of the list
for(i in seq_along(l)){
  l[[i]] <- read_csv(files_in[i])
}

# combine all list elements into a single dataframe
# then split into another list by SITE_CODE.
ldf <- bind_rows(l)
ldf <- split(ldf, ldf$SITE_CODE)

# use list indexing to explore
ldf[[1]]
ldf[[2]]
ldf[[length(ldf)]]

# loop over each list element and write a csv file
dir.create(here("data", "gwl", "site_code"))
# here we make a list of files names ( names(ldf) is a vector )
files_out <- glue::glue("{here('data', 'gwl', 'site_code', names(ldf))}.csv")

# check the length and names of ldf
length(ldf)
names(ldf)

# access a list element by name, e.g., SITE_ID 393079N1202452W001
ldf[["393079N1202452W001"]] 

# write 723 files in one fell swoop
for(i in seq_along(ldf)){
  write_csv(ldf[[i]], files_out[i])
  # cat("Wrote", files_out[i], "\n")
  cat("Evaluted index", i, "\n")
}

# hard code the loop:

# extract and run the first iteration of the loop with i=1
write_csv(ldf[[1]], files_out[1])

# second loop statement is with i=2
write_csv(ldf[[2]], files_out[2])

# second loop statement is with i=3
write_csv(ldf[[3]], files_out[3])

# .......

# second loop statement is with i=length(seq_along(ldf))
write_csv(ldf[[723]], files_out[723])


# lapply ------------------------------------------------------------------

library(tidyverse)
library(here)

# create a vector of files to read in
# list.files is the same as fs::dir_ls
files_in <- fs::dir_ls(here("data/gwl/county/"))

# sanity check the basename
basename(files_in)
path_file(files_in)

# read
l <- lapply(files_in, read_csv)

# check the class of the output
class(l)
length(l)
l[[1]]

# bind and split by SITE_CODE
ldf <- bind_rows(l)
ldf <- split(ldf, ldf$SITE_CODE)

# vector of output file names
files_out <- glue::glue("{here('data', 'gwl', 'site_code', names(ldf))}.csv")

# error checking - if the output directory doesn't exist, create it
if(! fs::dir_exists(here("data/gwl/site_code/"))){
  fs::dir_create(here("data/gwl/site_code/"))
}

# write out: requires function (write_csv) 
# with 2 args (x = ldf & y = files_out)
mapply(function(x, y) write_csv(x, y), ldf, files_out) 


# map ---------------------------------------------------------------------

# tilde (verb) and .x, .y (nouns) are new syntax
# .x and .y are placeholders for input vectors or lists
# tilde specifies the start of the function
map(vector_or_list, ~some_function(.x))

# read
l <- map(files_in, 
         ~read_csv(.x) %>% 
           mutate(filename = .x) %>% 
           janitor::clean_names())

# bind and split by SITE_CODE
ldf <- bind_rows(l)
ldf <- group_split(ldf, site_code)

# walk is like map but silent

# write - very similar to mapply in base R
# takes 2 arguments, a .x (ldf) and .y (files_out)
walk2(ldf, files_out, ~write_csv(.x, .y)) 



# map_df ------------------------------------------------------------------

library(tidyverse)
library(here)

# map_df is shorthand for map() %>% bind_rows()

# read and bind, split, and write
map_df(files_in, ~read_csv(.x)) %>% 
  group_split(SITE_CODE) %>% 
  walk2(files_out, ~write_csv(.x, .y))

# map_df with read_csv: read in all of the csvs we just wrote in
# one big dataframe
df <- map_df(fs::dir_ls(here("data/gwl/site_code")),
             ~suppressMessages(read_csv(.x)))

# debug in the wild
col_classes <-
  read_csv(fs::dir_ls(here("data/gwl/site_code"))[1], 
           id = "filename") %>% 
  map(~class(.x)) %>% 
  unlist()

# new readr package makes this even easier!
df <- fs::dir_ls(here("data/gwl/site_code")) %>% 
  map_df(~read_csv(.x, col_types = col_classes))

df


# map functions abstracted from code --------------------------------------

library(here)
library(tidyverse)

source(here::here("scripts/functions/f_import_clean_export.R"))

# create a directory to store results
dir.create(here("results"))

# vectors with function args: input (.x) & output (.y) files
files_in  <- list.files(here("data", "gwl", "county"), full.names = TRUE)
files_out <- here("results", str_replace_all(basename(files_in), ".csv", ".shp"))

walk2(files_in, files_out, ~f_import_clean_export(.x, .y))



# shiny apps --------------------------------------------------------------

# example to explore data: https://richpauloo.shinyapps.io/cig_nlp/

# start with an EDA on groundwater level in Sac Co.
library(tidyverse)
library(here)
library(sf)

# read groundwater level data pre-filtered to Sacramento county
gwl <- read_csv(here("data", "gwl", "gwl_sac.csv")) %>% 
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4269)

# read sacramento county shapefile and reproject
sac <- st_read(here("data", "shp", "sac", "sac_county.shp"), quiet=TRUE) %>% 
  st_transform(4269)

# plot the groundwater levels at each monitoring site 
# for more on dates, read: https://r4ds.had.co.nz/dates-and-times.html
gwl %>% 
  ggplot() +
  geom_line(aes(MSMT_DATE, WSE, color = SITE_CODE), 
            alpha = 0.5) +
  guides(color = "none")

# make a map
# slice the first station per group of groundwater level observations
# (see comment aside)
gwl_min <- gwl %>% 
  group_by(SITE_CODE) %>% 
  slice(1) %>% 
  ungroup() 

# visualize sites on a map
ggplot() +
  geom_sf(data = sac) +
  geom_sf(data = gwl_min, alpha = 0.5, 
          aes(color = WELL_USE)) +
  rcartocolor::scale_color_carto_d(palette = "Pastel") +
  theme_void()


# function that takes a site code and generates a plot
f_make_hydrograph <- function(data, site_code){
  p <- data %>% 
    filter(SITE_CODE == site_code) %>% 
    ggplot(aes(MSMT_DATE, WSE)) +
    geom_line(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = glue::glue("Groundwater level (ft AMSL) at: {site_code}"))
  return(p)
}

# use the function 
f_make_hydrograph(gwl, gwl$SITE_CODE[1])

# make a lot of plots to wow your friends and peers
walk(unique(gwl$SITE_CODE),
     ~f_make_hydrograph(gwl, .x) %>% 
       ggsave(filename = glue::glue("{here('figures/gwl')}/{.x}.png"),
              plot = .))



# shiny app structure -----------------------------------------------------

# 3 parts: ui, sever, runApp(ui, server) call
# ui = user interface (what the user sees)
# server = hidden to the user, and defines how to interpret
# changes the user makes in terms of how they affect the data
# and ui

# for every inputId in the ui, there should be a corresponding 
# input$inputId in the server, and for every output$outputId in the
# server, there should be a corresponding outputId in the ui




# final shiny app with leaflet --------------------------------------------

library(shiny)
library(tidyverse)
library(leaflet)

# load sac county groundwater data and sac county polygon
gwl <- read_csv(here("data", "gwl", "gwl_sac.csv"))

# groundwater locations
gwl_min <- gwl %>%
  group_by(SITE_CODE) %>%
  slice(1) %>%
  ungroup()


# ------------------------------------------------------------------------
# user interface
ui <- fluidPage(
  
  # change default theme to "united"
  theme = shinythemes::shinytheme("united"),
  
  # application title
  titlePanel("Sacramento County Groundwater Level Data"),
  
  # sidebar with a dropdown input for site_code
  sidebarLayout(
    sidebarPanel(
      selectInput("site_code",
                  "Select a site code:",
                  choices = unique(gwl$SITE_CODE))
    ),
    
    # tabs with hydrograph and data table
    mainPanel(
      # tabsetPanel(
      plotly::plotlyOutput("gwl_plot"),
      leafletOutput("leaflet")
      # tabPanel("Data", DT::dataTableOutput("gwl_data"))
      # )
    )
  )
)

# ------------------------------------------------------------------------
# define server logic to plot a hydrograph and create data table
server <- function(input, output) {
  
  # --------------------------------------------------
  # create hydrograph
  output$gwl_plot <- plotly::renderPlotly({
    
    # draw the ggplot based on the "site_code" user input
    p <- filter(gwl, SITE_CODE == input$site_code) %>%
      ggplot(aes(MSMT_DATE, WSE)) +
      geom_line(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = input$site_code,
           x = "", y = "Groundwater level (ft AMSL)")
    
    # render the plotly object
    plotly::ggplotly(p)
  })
  
  # create data table ----------------------------------
  output$gwl_data <- DT::renderDataTable({
    
    # draw the plot based on the "site_code" user input
    DT::datatable(
      filter(gwl, SITE_CODE == input$site_code),
      extensions = 'Buttons',
      options =
        list(dom = 'Bfrtip',
             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
    )
  })
  
  
  # create leaflet output ---------------------------
  output$leaflet <- renderLeaflet({
    selected_site <- gwl_min %>% filter(SITE_CODE == input$site_code)
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = selected_site$LONGITUDE,
                 lat = selected_site$LATITUDE) %>% 
      setView(lat = 38.495042, lng = -121.387303, zoom = 9)
    
  })
  
  
}

# ------------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)
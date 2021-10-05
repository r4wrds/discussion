# Live Code for 2021-10-05 R4WRDS Course

# Live Code for previous courses can be found on Github:
# 2021-08-10: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210810_r4wrds_intro.R
# 2021-08-24: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210824_r4wrds_intro.R 
# 2021-09-21: https://raw.githubusercontent.com/r4wrds/discussion/main/live_code/20210921_r4wrds_intermediate.R 

R.version # check your R version


# using here() ------------------------------------------------------------


library(here)   #install.packages("here")
here()

# here gives the path (relative) of a file or dir
here("data/major_rivers_ca.rds") # this allows tab autocomplete
here("data", "major_rivers_ca.rds") 

rivers <- readRDS(here("data/major_rivers_ca.rds"))

# please don't do this:
# rm()
# set Global Options to not save .Rdata by default or load by default
# we always want to start from a blank slate.
# to restart your session (which you can/should do often):
# Session > Restart R


# rainbow ()
(((((((((())))))))))


# .Rprofile ---------------------------------------------------------------

# runs R code at the start of a session (upon restart)

# install the {usethis} package if necessary
# install.packages("usethis")

usethis::edit_r_profile(scope = "project")

# paste this into .RProfile
cowsay::say(what = "hello r4wrds participants!", by = "cow")

# clear console with Cmd + L, Crtl + L and restart R



# .Renviron ---------------------------------------------------------------

# loads variables you want to keep hidden, or allow to be configurable per user

# access env vars with:
Sys.getenv() # returns all env vars on your machine

# access an existing env var by quoted name
Sys.getenv("USER")

# let's specify our own env vars (custom) and add ANIMAL = "cat"
usethis::edit_r_environ(scope = "project")

# access the variable you just set
Sys.getenv("ANIMAL")

# use env var in a function
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

# use our function with our env var
inspire_me(Sys.getenv("ANIMAL"))


# modularization of functions ---------------------------------------------

# sourcing other R scripts by file location
source(here::here("scripts/functions.R"))

# source a control script (see website for 01_control.R)



# Interactive Visualization -----------------------------------------------


# load libraries for map
# use install.packages("packagename")
library(tidygeocoder) # geocode our addresses
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)


# let's get our data
# the url for the Form data 
form_data <- glue("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                    "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")


# read in url and clean
dat <- readr::read_csv(form_data) %>% 
  janitor::clean_names() %>% 
  dplyr::rename( dining_name = 3, dining_address = 4)

# geocode
# geocode using Open Street Map (osm) API because it's free
dat_geo <- dat %>%
  geocode(.tbl = . , address = dining_address, method = 'osm', lat = latitude , long = longitude) 

# take this data and make it into a spatial sf object
# make into sf object so we can map
dat_geo <- dat_geo %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = 4326, # this is spatial projection 
               remove = FALSE) # keep the lat lon columns


# map!
mapview::mapview(
  dat_geo, # data
  zcol = "comfort_using_r", # color the points by this column
  layer.name = "R comfort level", 
  cex = 6.5
)

## Leaflet Maps

library(leafpm)
library(leaflet) # interactive map
library(leaflet.extras) # for scale bars and things
library(htmltools) # helpful for formatting html attributes

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
      <b>R comfort (1-10):</b> {dat_geo$comfort_using_r}"
    )
  )  %>% 
  addLayersControl(
    baseGroups = c("Toner Lite", "Positron", "OSM"),
    overlayGroups = c("Home", "Food & Drink"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addMeasure()

m  # Print the map

# d3 network example

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

# Plot
(p <- simpleNetwork(data, height = "600px", width = "600px", 
                    fontSize = 16, fontFamily = "serif",
                    nodeColour = "darkblue", linkColour = "steelblue",
                    opacity = 0.9, zoom = FALSE, charge = -500))




# Plotly: CalEnviroscreen -------------------------------------------------

# load CES data for Sacramento county
ces3_sac <- readRDS(here::here("data/ces3_sac.rds"))
class(ces3_sac)
mapview(ces3_sac, zcol="CIscoreP")

# using () around objects
example_ces3 <- head(ces3_sac)
example_ces3
(example_ces3 <- head(ces3_sac))


## Static
ces_plot <- ces3_sac %>% 
  ggplot(data = ., aes(x=gwthreatsP, y=CIscoreP, label=tract)) +
  geom_point() +
  geom_smooth(method = "gam") + 
  cowplot::theme_half_open(font_size = 12) +
  labs(
    title = "CES Score vs. Groundwater Threats in Sac Cnty",
    subtite = "Higher CI score indicates higher threat or impact",
    x = "Groundwater Threats (percentile)",
    y = "CI Score (percentile)"
  )


library(plotly)
ggplotly(ces_plot)



# quick detour on the pipe ------------------------------------------------

library(tidyverse)
# x %>% f is equivalent to f(x)
# x %>% f(y) is equivalent to f(x, y)
# x %>% f %>% g %>% h is equivalent to h(g(f(x)))
# x %>% f(y, .) is equivalent to f(y, x)
# x %>% f(y, z = .) is equivalent to f(y, z = x)

# insert a pipe operator with keyboard shortcut Shit + Ctrl + M
x <- 1:5
x %>% mean() # same as mean(x)
mean(x)      # same as x %>% mean()
x %>% mean(.)     # same as above
x %>% mean(x = .) # same as above

# common operations
data %>% 
  ggplot() +
  geom_smooth()

# left_join(data_1, data_2)
data_1 %>% 
  left_join(data_2)

# helpful for chaining together multiple functions
data %>% 
  mutate() %>% 
  select() %>% 
  write_csv()

# piping prevents us from nesting hard to read function calls
write_csv(select(mutate(data)))

# piping prevents us from creating unnecessary intermediate objects
# that clutter the workspace
data_2 <- mutate(data)
data_3 <- select(data_2)
write_csv(data_3)


# iteration and functional programming ------------------------------------

# for loops: make iteration very explicit by using a counter (i)

library(tidyverse)
library(here)

eldorado <- read_csv(here("data", "gwl", "county", "El Dorado.csv"))
placer   <- read_csv(here("data", "gwl", "county", "Placer.csv"))
sac      <- read_csv(here("data", "gwl", "county", "Sacramento.csv"))

# let's iterate over files and read them in with a for loop

# list all files we want to read in
files_in <- fs::dir_ls(here("data/gwl/county/"))

# initialize a list of defined length
l <- vector("list", length = length(files_in))
class(l)

# use double bracket notation to access list elements by index
l[[1]]

# loop over all files in files_in, and read them into each element of the list
for(i in seq_along(l)){ # for each i in the vector provided
  cat("Reading file:", files_in[i], "\n") # prints some useful info
  l[[i]] <- read_csv(files_in[i]) 
  cat("\nComplete.\n\n")
}

# above means we run the loop for i = 1, then i = 2, then i = 3
# replace i with 1 everywhere in the loop and evaluate, then repeat for 2 and 3

# inspect l after the loop
l[[1]] # this is the result of read_csv(files_in[1])

# let's make a silly loop that prints what index of the loop we're on
for(index in 1:100000){
  cat("We are on index:", index, "of the loop!\n")
}

# a list can hold any R object
my_list <- list(a = 1:3, b = data.frame(x=1, y=2), c = "apples")

# combine all list elements into a single dataframe
ldf <- bind_rows(l)

# split this into another list by a column
ldf <- split(ldf, ldf$SITE_CODE) # returns a named list, named by the splitting col
length(ldf)
ldf[[1]]
names(ldf) # this is a named list

# loop over each list element and write a csv file
fs::dir_create(here("data/gwl/site_code"))

# create a vector of file names to write out
files_out <- glue::glue("{here('data/gwl/site_code', names(ldf))}.csv")

# now we can write each split dataframe of ldf as a separate csv file
for(i in seq_along(ldf)){
  write_csv(ldf[[i]], files_out[i])
}


# lapply ------------------------------------------------------------------

# lapply is part of base R and enables functional programming

# read in some files with lapply and no loops, no initialization!
files_in
l <- lapply(files_in, read_csv) # same as the for loop above!
l[[3]]

# let's also write files with mapply
ldf <- bind_rows(l)
ldf <- split(ldf, ldf$SITE_CODE)
length(ldf)

# write out each of these files, and demonstrate an anonymous function
files_out # these are the output paths from above
mapply(function(x, y){ write_csv(x, y) }, ldf, files_out)

# extract the anoymous function
my_function <- function(x, y){
  write_csv(x, y)
} 

# same as above
mapply(my_function, ldf, files_out) 

# can't do this because write_csv is not set up for working with lists
my_function(ldf, files_out)
class(ldf)
length(files_out)


# map ---------------------------------------------------------------------

# redo what we've already seen - read in county csvs
# like lapply, the first argument to map is the thing you want to map over
# then the ~ (tilde) is where you *start* the function you want to map
# .x is a placeholder for where you put each element you want to map
l <- map(files_in, ~read_csv(.x))
l[[1]]

# just as before let's bind_rows, split into a dataframes by site code and write
ldf <- bind_rows(l)
ldf <- group_split(ldf, SITE_CODE) # like split, but doesn't name the list
names(ldf)

# write
# .x is placeholder for the first argument (ldf)
# and .y for the second argument (files_out)
walk2(ldf, files_out, ~write_csv(.x, .y))


# map_df ------------------------------------------------------------------

# a common task is to read in dataframes and combine them into one
files_in

map_df(files_in, ~read_csv(.x)) %>% # read in dataframes and combine into 1
  group_split(SITE_CODE) %>% # split by the SITE_CODE column into many dfs
  walk2(files_out, ~write_csv(.x, .y)) # walked over the dataframes above and files_out and wrote csvs



# map: putting it all together with a modular function --------------------

library(tidyverse)
library(here)
library(purrr)

source(here("scripts/functions/f_import_clean_export.R"))

# create a directory to store the results
fs::dir_create(here("results"))

# files and files out
files_in <- fs::dir_ls(here("data/gwl/county/"))
basename(files_in) # good to know
files_out <- here("results", str_replace_all(basename(files_in), 
                                             ".csv", ".shp"))

walk2(files_in, files_out, ~f_import_clean_export(.x, .y))



# shiny -------------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(tidylog)

# read groundwater level data pre-filtered to Sacramento county
gwl <- read_csv(here("data", "gwl", "gwl_sac.csv")) %>% 
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4269) 

# read sacramento county shapefile and reproject
sac <- st_read(here("data", "shp", "sac", "sac_county.shp"), quiet=TRUE) %>% 
  st_transform(4269)

# plot the groundwater levels at each monitoring site 
gwl %>% 
  ggplot() +
  geom_line(aes(MSMT_DATE, WSE, group = SITE_CODE), alpha = 0.5)

# slice the first station per group of groundwater level observations
# (see comment aside)
gwl_min <- gwl %>% 
  group_by(SITE_CODE) %>% 
  slice(1) %>% # slice pulls a row by index
  ungroup() 

# aside on dplyr::slice()
tibble(x = 1:3, y = letters[1:3]) %>% 
  slice(1)

# visualize sites on a map
ggplot() +
  geom_sf(data = sac) +
  geom_sf(data = gwl_min, alpha = 0.5, color = "blue") +
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

# make a hydrograph for the first SITE_CODE
f_make_hydrograph(gwl, gwl$SITE_CODE[1])



# adhoc shiny app input output example ------------------------------------

# copy/paste into an app.R and "Run App"
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot2"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'red', border = 'white')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



# working final shiny app -------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)

# load sac county groundwater data and sac county polygon
gwl <- read_csv(here::here("data", "gwl", "gwl_sac.csv"))

# groundwater locations
gwl_min <- gwl %>%
  group_by(SITE_CODE) %>%
  slice(1) %>%
  ungroup()

# ------------------------------------------------------------------------
# user interface
ui <- fluidPage(
  
  # change default theme to "united"
  theme = shinytheme("united"),
  
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
      tabsetPanel(
        tabPanel("Hydrograph", 
                 plotly::plotlyOutput("gwl_plot"),
                 leafletOutput("gwl_leaflet")),
        tabPanel("Data", DT::dataTableOutput("gwl_data"))
      )
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
  
  # --------------------------------------------------
  # create data table
  output$gwl_data <- DT::renderDataTable({
    
    # draw the plot based on the "site_code" user input
    DT::datatable(
      filter(gwl, SITE_CODE == input$site_code)
    )
  })
  
  # create leaflet output ---------------------------
  output$gwl_leaflet <- renderLeaflet({
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


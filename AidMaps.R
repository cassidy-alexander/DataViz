# load packages
{ library(maps)
  library(mapdata)
  library(tidyverse)
  library(stringr)
  library(viridis)
  library(tibble)
  library(plyr)
  library(ggplot2)
  library(dplyr)
  library(sf)
  library(RColorBrewer)
  library(ggmap)
  library(maps)
  library(tmap)
  library(htmlwidgets)
  library(leaflet)
}

# create list of colors orange to purple
mycols4 <- c("#F69543", "#F5664D", "#D44368", "#782F4A", "#351A2F")

# read shape file for mapping
states <- st_read("acs_2012_2016_county_us_B27001/acs_2012_2016_county_us_B27001.shp",
                  stringAsFactors = FALSE)
    # alternative shape file
    # states <- st_read("cb_2017_us_state_20m/cb_2017_us_state_20m.shp")

colnames(states) # display column names

# read variable data to be plotted
D <- read.csv("stateaid.csv", header = TRUE)
D <- data.frame(D) # convert data to data frame
colnames(D) # display column names

# manipulate D data
Aid <- as.numeric(D$GrantperUGFTE) # create vector of GrantperUGFTE
NAME <- as.character(D$State) # create vector of State names
test <- data.frame(Aid, NAME) # create data frame of aid and state names

# join shape data and variable data to one table
map_and_data <- inner_join(states, test) 

# default map of U.S. by undergrad aid
defaultMap <- tm_shape(map_and_data, projection = 2163) + # standard view of U.S.
  tm_polygons("Aid", id = "NAME", # color states by aid amount
              palette = mycols4, # set gradient colors
              border.col = "white", # outlines color
              border.alpha = .5, # outlines thickness
              title = "Aid (dollars)") + # rename legend
  tm_legend(legend.position = c("right", "top")) + # set legend in top right corner
  tm_layout(title = "State Undergraduate Aid per Student", 
            title.size = 1.1, # title text size
            title.position = c("left", "top"), # title position
            inner.margins = c(0.06, 0.10, 0.10, 0.08)) # set margins from bounding box
defaultMap

# interactive leaflet map of average state aid per undergrad
interactiveMap <- tmap_leaflet(defaultMap) %>%
  setView(-96, 37.8, zoom = 3.5)
interactiveMap

# export interactive map as html widget
saveWidget(interactiveMap, "mymap.html", selfcontained=TRUE)

# create subset of continental U.S.
continentalUS <- subset(states, NAME != "Alaska" & NAME != "Hawaii") # all rows except AK and HI
contMapData <- inner_join(continentalUS, test) # join data

# map of continental U.S. state aid per undergrad
contMap <- tm_shape(contMapData, projection = 2163) +
  tm_polygons("Aid", id = "NAME", # color states by aid amount
              palette = mycols4, # set gradient colors
              border.col = "white", # outlines color
              border.alpha = .5, # outlines thickness
              title = "Aid (dollars)") + # rename legend
  tm_legend(legend.position = c("right", "top")) +
  tm_layout(#title = "State Undergraduate Aid per Student",
            title.size = 1.1, # title text size
            title.position = c("right", "top"), # title position
            inner.margins = c(0.06, 0.10, 0.10, 0.08), # set margins from bounding box
            frame = FALSE) # remove bounding box
contMap

# create subset of Alaska data
AlaskaMapData <- (subset(map_and_data, NAME == "Alaska"))

# map of Alaska aid
AlaskaMap <- tm_shape(AlaskaMapData, projection = 2163) + # standard view
  tm_polygons("Aid", id = "NAME", # color states by aid amount
              palette = mycols4, # set gradient colors
              border.col = "white", # outlines color
              border.alpha = .25) + # outlines thickness
  tm_legend(legend.show = FALSE) + # remove legent
  tm_layout(frame = FALSE) # remove bounding box

# set new dimensions and placement for Alaska
viewAlaska <- viewport(x = 0.15, y = 0.22, width = 0.3, height = 0.3)

# create subset of Hawaii data
HawaiiMapData <- (subset(map_and_data, NAME == "Hawaii"))

# map of Hawaii aid
HawaiiMap <- tm_shape(HawaiiMapData, projection = 2163) + # standard view
  tm_polygons("Aid", id = "NAME", # color states by aid amount
              palette = mycols4, # set gradient colors
              border.col = "white", # outlines color
              border.alpha = .25) + # outlines thickness
  tm_legend(legend.show = FALSE) + # remove legent
  tm_layout(frame = FALSE) # remove bounding box
  
# set new dimensions and placement for Alaska
viewHawaii <- viewport(x = 0.4, y = 0.13, width = 0.2, height = 0.1)

# Plot Alaska and Hawaii on top of continental U.S. map
tmap_mode("plot") # change to plot mode
contMap # plot continental U.S. 
print(AlaskaMap, vp = viewAlaska) # add Alaska
print(HawaiiMap, vp = viewHawaii) # add Hawaii

# export map as .png to desktop
save_tmap(m_cont, "Aid2.png", scale = 0.7, width = 6.125, 
          insets_tm = list(US_AK, US_HI), 
          insets_vp = list(vp_AK, vp_HI))

#library(rgdal) 
library(move) 
library(dplyr)
library(leaflet)
library(units)
library(sf)
library(viridis)
library(kableExtra)
library(lubridate)
## Load packages for google drive ----
library(googledrive)
library(purrr)
library(readxl)
library(geosphere)
library(foreach)
library(maptools)
library(leaflet.opacity)
library(leaflet.extras)
library(htmlwidgets)

#options(googledrive_quiet = TRUE)

cams_1 <- st_read("data/Cam Trap Grid 2018.shp")
cams_2 <- st_read("data/Mega_Survey_set_-_Final_March_2020 waypoints.shp")
kids <- st_read("data/kids club partners.shp")
oc <- st_read("data/oc_propertiers_clean.shp")
oc <- st_zm(oc)
rest <- st_read("data/restoration partners.shp")
road <- st_read("data/Road Study 2021.shp")
road <- road[st_coordinates(road)[,1] < (-80),]


m <- leaflet() %>%
  # Add a satellite image layer
  addProviderTiles(providers$CartoDB.VoyagerNoLabels, 
                   options = providerTileOptions(minZoom = 10, maxZoom = 13))     

m <- m %>%
  addPolygons(data = oc, color = "pink", group = "property", weight=3,fillOpacity=1,stroke=F, popup="property") %>%
  addCircleMarkers(data = cams_1, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="cameratraps") %>%
  addCircleMarkers(data = cams_2, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="cameratraps") %>%
  addCircleMarkers(data = kids, color = "yellow", group = "kids club", weight=1,opacity=1, radius=1, popup="kids clubs")   %>%
  addCircleMarkers(data = road, color = "purple", group = "road survey", weight=1,opacity=1, radius=1, popup="road survey")   %>%
  addCircleMarkers(data = rest, color = "orange", group = "restoration", weight=1,opacity=1, radius=1, popup="restoration")    %>% 
  addLayersControl(
    overlayGroups = c("property","restoration","camera trap","kids club", "road survey", "restoration"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addFullscreenControl() %>% 
  addLegend(colors=c("pink", "blue", "yellow", "purple", "orange"), labels=c("OC property","camera trap","kids club", "road survey", "restoration partners"), opacity=1) %>% 
  suspendScroll(hoverToWake = TRUE, wakeTime = 2000)

m

saveWidget(m, "index.html" , selfcontained = TRUE, libdir = NULL,
           background = "white", knitrOptions = list())

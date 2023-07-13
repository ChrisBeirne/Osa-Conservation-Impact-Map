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

# Import animal locations
# Import passcodes
MOVE_PASS <- Sys.getenv("MOVEBANK_PASSWORD")
MOVE_USE  <- Sys.getenv("MOVEBANK_USERNAME")

loginStored <- movebankLogin(username=MOVE_USE, 
                             password=MOVE_PASS)

# Get animals
# Vultures
animals <-getMovebankAnimals(study=1573471517,login=loginStored)
# Ocelot
tmp <-getMovebankAnimals(study=2526574641,login=loginStored)
# Tapir
tmp2 <- getMovebankAnimals(study=1954804459,login=loginStored)
animals <- rbind(animals, tmp,tmp2)

# For some reason they are duplicated
animals[duplicated(animals)==F,]
# They vary by the field "sensor_type_id"
animals <- animals[animals$sensor_type_id==653 & is.na(animals$sensor_type_id)==F,]

# Clean up the name
animals$animalName <- paste0(sub('\\_.*', '', animals$animalName), "_", sub('\\ .*', '', animals$taxon_canonical_name))
animals$name <- sub('\\_.*', '', animals$animalName)

# Sort date objects
animals$timestamp_start <- ymd_hms(animals$timestamp_start)
animals$timestamp_end <- ymd_hms(animals$timestamp_end)

# Get last 2 weeks
t <- now("America/Costa_Rica")
start_t <- t-as.difftime(5,units='days')
start_tapir <- t-as.difftime(48,units='days')

# Vultures
mov_dat <- getMovebankData(study=1573471517, login=loginStored,  removeDuplicatedTimestamps=TRUE,
                           timestamp_start=start_t)
#OCelot
tmp <- getMovebankData(study=2526574641, login=loginStored,  removeDuplicatedTimestamps=TRUE,
                       timestamp_start=start_tapir)

#Tapir
tmp2 <- getMovebankData(study=1954804459, login=loginStored,  removeDuplicatedTimestamps=TRUE,
                        timestamp_start=start_tapir)

# Remove the obvious outlier
tmp2 <- tmp2[tmp2$location_lat>8,]

mov_dat <- moveStack(mov_dat, tmp, tmp2)



#Add the names
mov_dat$name <- trackId(mov_dat)


# Convert timezone
mov_dat$timestamp <- with_tz(timestamps(mov_dat), tz="America/Costa_Rica")

# Convery move stack to dataframe
dat <- as.data.frame(mov_dat)

# Convert dat to costa rica time
dat$timestamp <- with_tz(dat$timestamp, tzone = "America/Costa_Rica")

# Add the location data
dat <- left_join(dat, animals[, c("tag_id", "animalName")])
# Sort the names out

dat$animalName <- sub('\\_.*', '', dat$animalName)
# Add in the taxonomic group
dat$animalName <- paste0(dat$animalName, "_", sub('\\ .*', '', dat$taxon_canonical_name))




# Get Icons
papa <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/king_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 39)

aura <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/turk_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 39)

mela <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/yhv_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 39)


atra <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/bhv_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 39)


pardalis <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/ocelot.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 39)

bairdii <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/tapir.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 22, iconAnchorY = 39)

iconSet <- iconList(aura= aura,
                    papa =papa,
                    melambrotus = mela,
                    atratus = atra,
                    pardalis = pardalis,
                    bairdii = bairdii)

dat$icon <- sub(".*? ", "", dat$taxon_canonical_name)

# Add a country column
dat$country <- dat$location_lat<0
dat$country[dat$country==T] <- "peru"
dat$country[dat$country==F] <- "costa_rica"
tmp_cr <- dat[dat$country=="costa_rica",]


lastloc <- tmp_cr %>%
  group_by(name) %>%
  slice(n()) %>%
  ungroup

ids <- unique(tmp_cr$name)


m <- leaflet() %>%
  # Add a satellite image layer
  addProviderTiles(providers$CartoDB.VoyagerNoLabels, 
                   options = providerTileOptions(minZoom = 10, maxZoom = 13)) %>% 
                   setView(lng=-83.26358816666858, lat=8.708281742832918, zoom = 10)

m <- m %>%
  addPolygons(data = oc, color = "pink", group = "property", weight=3,fillOpacity=1,stroke=F, popup="property") %>%
  addCircleMarkers(data = cams_1, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="cameratraps") %>%
  addCircleMarkers(data = cams_2, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="cameratraps") %>%
  addCircleMarkers(data = kids, color = "yellow", group = "kids club", weight=1,opacity=1, radius=1, popup="kids clubs")   %>%
  addCircleMarkers(data = road, color = "purple", group = "road survey", weight=1,opacity=1, radius=1, popup="road survey")   %>%
  addCircleMarkers(data = rest, color = "orange", group = "restoration", weight=1,opacity=1, radius=1, popup="restoration") %>% 
  # Add the last location point for each animal
  addMarkers(lng=lastloc$location_long,
             lat=lastloc$location_lat, 
             popup=lastloc$local_identifier,
             icon = iconSet[lastloc$icon], group="animals")   %>% 
  addLayersControl(
    overlayGroups = c("property","restoration","camera trap","kids club", "road survey", "restoration", "animals"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addFullscreenControl() %>% 
  addLegend(colors=c("pink", "blue", "yellow", "purple", "orange"), labels=c("OC property","camera trap","kids club", "road survey", "restoration partners"), opacity=1) %>% 
  suspendScroll(hoverToWake = TRUE, wakeTime = 2000)

m
 
saveWidget(m, "index.html" , selfcontained = TRUE, libdir = NULL,
           background = "white", knitrOptions = list())

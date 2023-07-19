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

# Corridors
corridors <- st_read("data/CB_amistosa.shp")
corridors<- st_transform(corridors, 4326)

corridors2 <- st_read("data/Corredores_Biológicos.shp")
corridors2<- st_transform(corridors2, 4326)

# PA's
# Import WPDA protected area and filter to this area
# tmp <-readRDS("C:/Users/cwbei/Dropbox/GitHubProjects/Osa-Conservation-Connectivity-Project/data/input/WDPA_protected_areas/all_area_pa_shp.RDS") 
# focal_countries <- st_read("data/focal_countries.shp")
# cr <- focal_countries[focal_countries$name=="Costa Rica",]
# cr<- st_make_valid(cr)
# tmp<- st_make_valid(tmp)
# tmp <- st_intersection(cr, tmp)
#st_write(tmp, "data/cr_protected.shp")

pa <- st_read("data/cr_protected.shp")
np <- pa[pa$DESIG_E=="National Park",] 
pa <- pa[pa$DESIG_E!="National Park",] 

# Create OSa impact zone
# All points, buffered and interesected with land area

t1<- st_buffer(cams_1,7000)
t2<- st_buffer(cams_2,7000)
t3<- st_buffer(kids,7000)
t4<- st_buffer(oc,7000)
t5<- st_buffer(rest,7000)
t6<- st_buffer(road,7000)

test<- c(st_geometry(t1), st_geometry(t2), st_geometry(t3), st_geometry(t4), st_geometry(t5), st_geometry(t6))
tmp <- st_combine(test)
#plot(test)
tmp <- st_union(test)
#plot(tmp)
tmp2 <- nngeo::st_remove_holes(tmp)
#plot(tmp2)
tmp2<-st_make_valid(tmp2)
tmp3 <- st_simplify(tmp2, dTolerance = 2000)
#plot(tmp3)
tmp4<- st_cast(tmp3, "POLYGON")
tmp4$area <- st_area(tmp4)
tmp5 <- tmp4[order(tmp4$area)]
#plot(tmp5[3])
aoi <- tmp5[3]
plot(aoi)
aoi <- st_as_sf(aoi)
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
animals$animalName <- paste0(sub('//_.*', '', animals$animalName), "_", sub('// .*', '', animals$taxon_canonical_name))
animals$name <- sub('//_.*', '', animals$animalName)

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

dat$animalName <- sub('//_.*', '', dat$animalName)
# Add in the taxonomic group
dat$animalName <- paste0(dat$animalName, "_", sub('// .*', '', dat$taxon_canonical_name))




# Get Icons
papa <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/king_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10)

aura <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/turk_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10)

mela <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/yhv_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10)


atra <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/bhv_small.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10)


pardalis <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/ocelot.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10)

bairdii <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/tapir.png",
  iconWidth = 19, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10)

iconSet <- iconList(aura= aura,
                    papa =papa,
                    melambrotus = mela,
                    atratus = atra,
                    pardalis = pardalis,
                    bairdii = bairdii)
# Create a legend for the animals
html_legend <- "<img src='https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/king_small.png'style='width:19px;height:20px; '> King vulture<br/>
                <img src='https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/turk_small.png'style='width:19px;height:20px; '> Turkey vulture<br/>
                <img src='https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/bhv_small.png'style='width:19px;height:20px;  '> Black vulture<br/>
                <img src='https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/ocelot.png'style='width:19px;height:20px;     '> Ocelot<br/>
                <img src='https://raw.githubusercontent.com/ChrisBeirne/Osa-Conservation-Movement-Ecology-Daily-Report/main/icons/tapir.png'style='width:19px;height:20px;      '> Baird's tapir"
                

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
  addProviderTiles(providers$Esri.WorldImagery, 
                   options = providerTileOptions(minZoom = 10, maxZoom = 13)) %>% 
                   setView(lng=-83.26358816666858, lat=8.708281742832918, zoom = 10)

m <- m %>%
  addPolygons(data = np, color = "#236049AA", group = "National parks", weight=3,fillOpacity=1,stroke=F, popup=np$NAME_1) %>%
  addPolygons(data = pa, color = "#23604950", group = "Protected areas", weight=3,fillOpacity=1,stroke=F, popup=pa$NAME_1) %>%
  addPolygons(data = corridors2, color = "#90604950", group = "Biological corridors", weight=3,fillOpacity=1,stroke=F, popup=corridors2$nombre_cb) %>%
  addPolygons(data = corridors, color = "#90604950", group = "Biological corridors", weight=3,fillOpacity=1,stroke=F, popup=corridors$Nombre) %>%
  addPolygons(data = oc, color = "#e60f0f", group = "property", weight=3,fillOpacity=1,stroke=F, popup="Osa Conservation Private Wildlife Refuge") %>%
  addPolygons(data = aoi, fill=F, color = "yellow", group = "Osa Conservation's Impact Zone", weight=5,fillOpacity=1,stroke=T) %>%
  addCircleMarkers(data = cams_1, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="Wildlife Monitoring Devices") %>%
  addCircleMarkers(data = cams_2, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="Wildlife Monitoring Devices") %>%
  addCircleMarkers(data = kids, color = "purple", group = "kids club", weight=1,opacity=1, radius=1, popup="Youth Nature Club Chapters")   %>%
  addCircleMarkers(data = road, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="Wildlife Monitoring Devices")   %>%
  addCircleMarkers(data = rest, color = "orange", group = "restoration", weight=1,opacity=1, radius=1, popup="Restoration Network Members") %>% 
  # Add the last location point for each animal
  addMarkers(lng=lastloc$location_long,
             lat=lastloc$location_lat, 
             popup=paste0(lastloc$local_identifier,"<br>" ,substr(lastloc$timestamp,1,16)),
             icon = iconSet[lastloc$icon], group="animals")  %>%
  addFullscreenControl() %>% 
  addLegend(colors=c("#e60f0f", "blue", "purple", "orange","#236049AA","#23604950", "#90604950", "yellow"), 
            labels=c("Osa Conservation Private Wildlife Refuge","Wildlife Monitoring Devices","Youth Nature Club Chapters", "Restoration Network Members", "National parks", "Protected areas", "Biological corridors", "Impact Zone"),
            opacity=1) %>% 
  suspendScroll(hoverToWake = TRUE, wakeTime = 2000)  %>%
  addControl(html = html_legend, position = "topright")  %>% 
  addLayersControl(
    overlayGroups = c("Osa Conservation's Impact Zone","National parks", "Protected areas", "Biological corridors" ),
    options = layersControlOptions(collapsed = FALSE)
  ) 

m
 ?addPolygons
saveWidget(m, "index.html" , selfcontained = TRUE, libdir = NULL,
           background = "white", knitrOptions = list())

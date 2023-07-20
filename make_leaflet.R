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
zone <- data.frame(lon= c(-82.70765465758336, -84.00351630486277),
                   lat= c(7.893950842276212,  9.44561248318058))
      

zone <- zone %>% 
          st_as_sf(coords = c("lon", "lat"), 
                   crs = 4326) %>% 
          st_bbox() %>% 
          st_as_sfc()        
        
      
cams_1 <- st_read("data/Cam Trap Grid 2018.shp")
cams_2 <- st_read("data/Mega_Survey_set_-_Final_March_2020 waypoints.shp")
kids <- st_read("data/kids club partners.shp")
oc <- st_read("data/oc_propertiers_clean.shp")
oc <- st_zm(oc)
rest <- st_read("data/restoration partners.shp")
rest2 <- st_read("data/rest3.shp")
rest2 <- st_zm(rest2, drop = T, what = "ZM")

road <- st_read("data/Road Study 2021.shp")
road <- road[st_coordinates(road)[,1] < (-80),]

# Corridors
corridors <- st_read("data/CB_amistosa.shp")
corridors<- st_transform(corridors, 4326)

corridors2 <- st_read("data/Corredores_Biológicos.shp")
corridors2<- st_transform(corridors2, 4326)

# Create other elements
plant <- data.frame(x=c(-83.34874,-83.295492), y=c(8.410424,8.764304))
plant <- st_as_sf(plant, coords = c("x", "y"), crs = 4326)

arboreal <- read.csv("data/arboreal_bridges_csv.csv", header=T)
arboreal <- st_as_sf(arboreal, coords = c("x", "y"), crs = 4326)

hatchery <- data.frame(x=c(-83.334338), y=c(8.393745))
hatchery <- st_as_sf(hatchery, coords = c("x", "y"), crs = 4326)


# PA's
# Import WPDA protected area and filter to this area
# tmp <-readRDS("C:/Users/cwbei/Dropbox/GitHubProjects/Osa-Conservation-Connectivity-Project/data/input/WDPA_protected_areas/all_area_pa_shp.RDS") 
 focal_countries <- st_read("data/focal_countries.shp")

# cr <- st_crop(focal_countries, zone)
# cr<- st_make_valid(cr)
# tmp<- st_make_valid(tmp)
# tmp <- st_intersection(cr, tmp)
# st_write(tmp, "data/cr_protected.shp", append=F)

pa <- st_read("data/cr_protected.shp")
np <- pa[pa$DESIG_E=="National Park",] 
pa <- pa[pa$DESIG_E!="National Park",] 

# Clean up the national park file
np_labels <- np[np$NAME_1 %in% c("Corcovado", "Piedras Blancas"),]
np_labels <- rbind(np_labels, np[np$NAME_1=="Internacional La Amistad",][2,])
tmp <- np[np$NAME_1=="Internacional La Amistad",][1,]
np<- np[!(np$NAME_1 %in% np_labels$NAME_1),]
np <- rbind(np, tmp)

# Create OSa impact zone
# All points, buffered and interesected with land area

t1<- st_buffer(cams_1,7000)
t2<- st_buffer(cams_2,7000)
t3<- st_buffer(kids,7000)
t4<- st_buffer(oc,7000)
t5<- st_buffer(rest,7000)
t6<- st_buffer(road,7000)
t7<- st_buffer(rest2,7000)
t7 <- st_cast(t7, "POLYGON")
t8<- st_buffer(plant,7000)
t9<- st_buffer(arboreal,7000)
test<- c(st_geometry(t1), st_geometry(t2), st_geometry(t3), st_geometry(t4), st_geometry(t5), st_geometry(t6),st_geometry(t7),st_geometry(t8),st_geometry(t9) )
tmp <- st_combine(test)
#plot(test)
tmp <- st_union(test)
#plot(tmp)
tmp <- tmp[st_geometry_type(tmp)=="MULTIPOLYGON"]

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
#plot(aoi)
aoi <- st_as_sf(aoi)


# Crop relevant files to the zone
corridors2<- st_make_valid(corridors2)
corridors2 <- st_crop(corridors2, zone)



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

# Make labels for national parks
np_labs <- data.frame(labels=paste0(np_labels$NAME_1), x=st_coordinates(st_centroid(np_labels))[,1],y=st_coordinates(st_centroid(np_labels))[,2])
np_labs$labels[np_labs$labels=="Internacional La Amistad"] <- "Parque Internacional La Amistad"
np_labs$labels[np_labs$labels=="Corcovado"] <- "Corcovado National Park"
np_labs$labels[np_labs$labels=="Piedras Blancas"] <- "Piedras Blancas National Park"


rest2 <- st_as_sf(rest2)
# cOMMON NAMES FOR THE ANIMALS
lastloc$taxon_canonical_name
lastloc$common_name <- NA
lastloc$common_name[lastloc$taxon_canonical_name=="Cathartes aura"] <- "Turkey vulture"
lastloc$common_name[lastloc$taxon_canonical_name=="Coragyps atratus"] <- "Black vulture"
lastloc$common_name[lastloc$taxon_canonical_name=="Leopardus pardalis"] <- "Ocelot"
lastloc$common_name[lastloc$taxon_canonical_name=="Sarcoramphus papa"] <- "King vulture"
lastloc$common_name[lastloc$taxon_canonical_name=="Tapirus bairdii"] <- "Baird's tapir"

#table(lastloc$taxon_canonical_name)
m <- leaflet() %>%
  # Add a satellite image layer
  addProviderTiles(providers$Esri.WorldImagery, 
                   options = providerTileOptions(minZoom = 9, maxZoom = 13)) %>% 
                   setView(lng=-83.26358816666858, lat=8.708281742832918, zoom = 10) 

m <- m %>%
  addPolygons(data = np_labels, color = "#487f16AA", group = "National parks"                       , weight=3,fillOpacity=1,stroke=F) %>%
  addPolygons(data = np, color = "#487f16AA", group = "National parks"                       , weight=3,fillOpacity=1,stroke=F) %>%
  addPolygons(data = pa, color = "#d4fa37AA", group = "Protected areas"                      , weight=3,fillOpacity=1,stroke=F) %>%
  addPolygons(data = corridors2, color = "#f8cecc50", group = "Biological corridors"         , weight=3,fillOpacity=1,stroke=F) %>%
  addPolygons(data = corridors, color = "#f8cecc50", group = "Biological corridors"          , weight=3,fillOpacity=1,stroke=F) %>%
  addPolygons(data = oc, color = "#e60f0f", group = "property"                               , weight=3,fillOpacity=1,stroke=F, popup="Osa Conservation Private Wildlife Refuge") %>%
  addPolygons(data = aoi, fill=F, color = "#f29e21", group = "Osa Conservation's Impact Zone", weight=5,opacity=1,stroke=T) %>%
  addPolygons(data = zone, fill=F, color = "black", weight=3,opacity=1,stroke=T) %>%
  addPolygons(data = rest2, color = "#72f9ff", group = "restoration"                         , weight=1,fillOpacity=1,stroke=F, popup="Restoration Network Members") %>% 
  addCircleMarkers(data = cams_1, color = "blue", group = "camera trap"                      , weight=1,fillOpacity=1, radius=2,stroke=F, popup="Wildlife Monitoring Devices") %>%
  addCircleMarkers(data = cams_2, color = "blue", group = "camera trap"                      , weight=1,fillOpacity=1, radius=2,stroke=F, popup="Wildlife Monitoring Devices") %>%
  addCircleMarkers(data = kids, color = "#ec49c9", group = "kids club"                       , weight=1,fillOpacity=1, radius=2,stroke=F, popup="Youth Nature Club Chapters")   %>%
  addCircleMarkers(data = road, color = "blue", group = "camera trap"                        , weight=1,fillOpacity=1, radius=2,stroke=F, popup="Wildlife Monitoring Devices")   %>%
  addCircleMarkers(data = rest, color = "#72f9ff", group = "restoration"                     , weight=1,fillOpacity=1, radius=2,stroke=F, popup="Restoration Network Members") %>% 
  addCircleMarkers(data = plant, color = "#707070", group = "restoration"                     , weight=1,fillOpacity=1, radius=6,stroke=F, popup="Native Tree Nursery") %>% 
  addCircleMarkers(data = hatchery, color = "#ad3dfb", group = "restoration"                     , weight=1,fillOpacity=1, radius=6,stroke=F, popup="Protective Sea Turtle Hatchery") %>% 
  addCircleMarkers(data = arboreal, color = "#519eea", group = "restoration"                     , weight=1,fillOpacity=1, radius=2,stroke=F, popup="Treetop Bridges") %>% 
  addCircleMarkers(data = rest, color = "#72f9ff", group = "restoration"                     , weight=1,fillOpacity=1, radius=2,stroke=F, popup="Restoration Network Members") %>% 
  # Add centroids to make them visible
  addCircleMarkers(data = st_centroid(rest2), color = "#72f9ff", group = "restoration"       , weight=1,fillOpacity=1,radius=2, stroke=F,popup="Restoration Network Members") %>% 
  
    #
  # Add the last location point for each animal
  addMarkers(lng=lastloc$location_long,
             lat=lastloc$location_lat, 
             popup=paste0(lastloc$local_identifier,"<br>" ,substr(lastloc$timestamp,1,16), "<br>", lastloc$common_name),
             icon = iconSet[lastloc$icon], group="animals")  %>%
  addFullscreenControl() %>% 
  addLabelOnlyMarkers(data = np_labs,
                      lng = ~x, lat = ~y, label = ~labels,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', weight = 10,opacity=0.8,)) %>%
  addLegend(colors=c("#e60f0f", "blue", "#ec49c9", "#72f9ff","#ad3dfb", "#519eea", "#707070" ,"#487f16AA","#d4fa37AA", "#f8cecc50", "#f29e21"), 
            labels=c("Osa Conservation Private Wildlife Refuge","Wildlife Monitoring Devices","Youth Nature Club Chapters", "Restoration Network Sites/Members", "Protective Sea Turtle Hatchery","Treetop Bridges","Native Tree Nurseries", 
                     "National Parks", "Protected areas", "Biological corridors", "Impact Zone"),
            opacity=1) %>% 
  suspendScroll(hoverToWake = TRUE, wakeTime = 2000)  %>%
  #remove animal key to reduce clutter
  #addControl(html = html_legend, position = "topright")  %>% 
  addLayersControl(
    overlayGroups = c("Osa Conservation's Impact Zone","National parks", "Protected areas", "Biological corridors" ),
    options = layersControlOptions(collapsed = FALSE)
  ) 

m

saveWidget(m, "index.html" , selfcontained = TRUE, libdir = NULL,
           background = "white", knitrOptions = list())
?addLabelOnlyMarkers

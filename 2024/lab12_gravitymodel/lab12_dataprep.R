library(tidyverse)
library(sf)
library(tmap)
library(geojsonio)
library(sp)
library(reshape2)
library(stplanr)
library(leaflet)
library(broom)

library(rmapshaper)

#here is a geojson of Greater Capital City Statistical Areas, so let's read it in as an 'sp' object
Aus <- geojson_read("https://www.dropbox.com/s/0fg80nzcxcsybii/GCCSA_2016_AUST_New.geojson?raw=1", what = "sp")

#now let's extract the data
Ausdata <- Aus@data

#here is a geojson of Greater Capital City Statistical Areas, so let's read it in as a 'simple features' object and set the coordinate reference system at the same time in case the file doesn't have one.
AusSF <- st_as_sf(Aus) %>% 
  st_set_crs(4283) %>%
  st_make_valid()
#view the file
AusSF

# simplify
AusSF <- st_simplify(AusSF, preserveTopology = TRUE, dTolerance = 500)

# reproject
AusSF <- st_transform(AusSF, crs = 3112)

dist <- st_distance(st_centroid(AusSF))
## Returns units - drop these for the conversion
dist <- units::drop_units(dist)

#melt this matrix into a list of origin/destination pairs using melt. Melt in in the reshape2 package. Reshape2, dplyr and ggplot, together, are some of the best packages in R, so if you are not familiar with them, get googling and your life will be much better!
distPair <- melt(dist)
#convert metres into km
distPair$value <- distPair$value / 1000
head(distPair)

#read in your Australian Migration Data
mdata <- read_csv("https://www.dropbox.com/s/wi3zxlq5pff1yda/AusMig2011.csv?raw=1",col_names = TRUE)

head(mdata)

#First create a new total column which excludes intra-zone flow totals (well sets them to a very very small number for reasons you will see later...)
mdata$FlowNoIntra <- ifelse(mdata$Orig_code == mdata$Dest_code,0,mdata$Flow)
mdata$offset <- ifelse(mdata$Orig_code == mdata$Dest_code,0.0000000001,1)

#now we ordered our spatial data earlier so that our zones are in their code order. We can now easily join these data together with our flow data as they are in the correct order.
mdata$dist <- distPair$value 
#and while we are here, rather than setting the intra zonal distances to 0, we should set them to something small (most intrazonal moves won't occur over 0 distance)
mdata$dist <- ifelse(mdata$dist == 0,5,mdata$dist)

head(mdata)

#remove intra-zonal flows
mdatasub <- mdata[mdata$Orig_code!=mdata$Dest_code,]

#use the od2line function from RObin Lovelace's excellent stplanr package - remove all but the origin, destination and flow columns
mdatasub_skinny <- mdatasub[,c(2,4,5)]
travel_network <- od2line(flow = mdatasub_skinny, zones = AusSF)
#convert the flows to WGS84

w <- mdatasub_skinny$Flow / max(mdatasub_skinny$Flow) * 10
plot(st_geometry(AusSF))
plot(st_geometry(travel_network), lwd = w, add = TRUE)


save(mdata, mdatasub, travel_network, AusSF, file = "aussie_travel.RData")

write.csv(mdata, "../datafiles/aussie_flow.csv", row.names = FALSE)

st_write(AusSF, "../datafiles/aus_gccsa/aus_gccsa.shp")

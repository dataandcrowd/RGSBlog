mapboxAccessToken <- 'pk.eyJ1IjoiaHNoaW4xNyIsImEiOiJja2hoaTYxbnYwbmxmMnlvOW0xY2s2emYxIn0.ccMG4CeAL-kHoTRo45LvQg'
# https://statistics.ukdataservice.ac.uk/dataset/2011-census-geography-boundaries-lower-layer-super-output-areas-and-data-zones
library(tidyverse)
library(flowmapblue)
library(googlesheets4)
library(data.table)
library(sf)

##
longlat2015 <- 
  read_csv("GIS/longlat_od2015.csv") %>% 
  rename(lon = X, lat = Y, id = ID) 

longlat2015_edge <-  
  read_csv("GIS/longlat_edge2015.csv") %>%
  select(id, osm_name) %>%
  rename(name = osm_name)

setDT(longlat2015)
setDT(longlat2015_edge)

longlat2015_edge[longlat2015,on=.(id=id),roll="nearest"] -> lg

####
od_rollup_2015 <- read_csv("OD/glasgow_2015_ride_od_rollup_month_total.csv") 

od_rollup_2015 %>% 
  select(origin, destination, cmtcnt) %>% 
  rename(dest = destination,
         count = cmtcnt) %>% 
  group_by(origin, dest) %>% 
  summarise(count = sum(count)) -> fg

flowmapblue(lg, fg, mapboxAccessToken, clustering=TRUE, darkMode=TRUE, animation=F)


############################

longlat2017 <- 
  read_csv("GIS/longlat_od2017.csv") %>% 
  rename(lon = X, lat = Y, id = ID) 


longlat2017_edge <-  
  read_csv("GIS/longlat_edge2017.csv") %>%
  select(id, osm_name) %>%
  rename(name = osm_name)


setDT(longlat2017)
setDT(longlat2017_edge)

longlat2017_edge[longlat2017,on=.(id=id),roll="nearest"] -> lg17

####
od_rollup_2017 <- read_csv("OD/glasgow_2017_ride_od_rollup_month_total.csv") 

od_rollup_2017 %>% 
  select(origin, destination, cmtcnt) %>% 
  rename(dest = destination,
         count = cmtcnt) %>% 
  group_by(origin, dest) %>% 
  summarise(count = sum(count)) -> fg17

flowmapblue(lg17, fg17, mapboxAccessToken, clustering=TRUE, darkMode=TRUE, animation=F)



#####################
longlat2019 <- 
  read_csv("GIS/longlat_od2019.csv") %>% 
  rename(lon = X, lat = Y, id = ID) 

longlat2019_edge <-  
  read_csv("GIS/longlat_edge2019.csv") %>%
  select(OSM_ID, OSM_NAME) %>%
  rename(name = OSM_NAME,
         id = OSM_ID)

setDT(longlat2019)
setDT(longlat2019_edge)

longlat2019_edge[longlat2019,on=.(id=id),roll="nearest"] -> lg19

####
# Batch Import with the data.table package
files <-list.files(path="OD", pattern="^glasgow_2019", full.names=TRUE)
od_rollup_2019 <- rbindlist(lapply(files, fread), idcol = "month")
od_rollup_2019[, month := factor(month, labels = basename(files))]


od_rollup_2019 %>% 
  select(origin, destination, cmtcnt) %>% 
  rename(dest = destination,
         count = cmtcnt) %>% 
  group_by(origin, dest) %>% 
  summarise(count = sum(count)) -> fg19

flowmapblue(lg19, fg19, mapboxAccessToken, clustering=TRUE, darkMode=TRUE, animation=F)




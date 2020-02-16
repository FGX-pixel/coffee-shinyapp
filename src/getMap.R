## get map script
# needs outlet data loaded

library(OpenStreetMap)

# store locations
outletLocations <- as.data.frame(projectMercator( lat = outlets$store_latitude, long = outlets$store_longitude))
outletLocations <- data.frame(outlets$sales_outlet_id, outletLocations)
outletLocations <- outletLocations[outlets$sales_outlet_type=="retail",]

if(!exists("output/mapNyc.rds")) {
  nycMap <- readRDS("output/mapNyc.rds")
} else {
  # get map to avoid later calls
  
  # dynamic map selection
  mapMargin <- .05
  mapCornerUL <- c(max(outlets[outlets$sales_outlet_type=="retail",]$store_latitude) + mapMargin, 
                   min(outlets[outlets$sales_outlet_type=="retail",]$store_longitude) - mapMargin)
  mapCornerDR <- c(min(outlets[outlets$sales_outlet_type=="retail",]$store_latitude) - mapMargin,
                   max(outlets[outlets$sales_outlet_type=="retail",]$store_longitude) + mapMargin)
  mapp <- openmap(mapCornerUL, mapCornerDR, type = "osm")
  
  # save map
  saveRDS(mapp, "output/mapNyc.rds")
  nycMap <- readRDS("output/mapNyc.rds")
}

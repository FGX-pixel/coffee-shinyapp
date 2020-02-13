## get map script
# needs outlet data loaded

if(!exists("mapNyc.rds")) {
  nycMap <- readRDS("mapNyc.rds")
} else {
  # get map to avoid later calls
  library(OpenStreetMap)
  
  # store locations
  outletLocations <- as.data.frame(projectMercator( lat = outlets$store_latitude, long = outlets$store_longitude))
  outletLocations <- data.frame(outlets$sales_outlet_id, outletLocations)
  outletLocations <- outletLocations[outlets$sales_outlet_type=="retail",]
  
  # dynamic map selection
  mapMargin <- .05
  mapCornerUL <- c(max(outlets[outlets$sales_outlet_type=="retail",]$store_latitude) + mapMargin, 
                   min(outlets[outlets$sales_outlet_type=="retail",]$store_longitude) - mapMargin)
  mapCornerDR <- c(min(outlets[outlets$sales_outlet_type=="retail",]$store_latitude) - mapMargin,
                   max(outlets[outlets$sales_outlet_type=="retail",]$store_longitude) + mapMargin)
  mapp <- openmap(mapCornerUL, mapCornerDR, type = "osm")
  
  # save map
  saveRDS(mapp, "mapNyc.rds")
  nycMap <- readRDS("mapNyc.rds")
}

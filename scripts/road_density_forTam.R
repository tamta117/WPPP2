#set working directory
library(here)
library(raster) 
library(sf)
library(dplyr)
library(sp)
library(raster)
library(fasterize)
library(FedData)
library(sf)
library(velox)
library(dplyr)
library(rgdal)
library(units)

#####----- ROAD DENSITY  ----####

#read in all WA public roads from WA DOT
roads <- st_read(here("data/GIS/roads/roads.gdb"))
roads <- st_transform(roads, crs=32610)
roads <- st_zm(roads) #drops M and Z values
#roads_sp <- as(roads, "Spatial")

#create empty raster over WA and then give raster values the pixel number
raster_WA <- raster(extent(washington_UTM_sp), res=1000, crs="+init=epsg:32610")
raster_WA[] <- 1:ncell(rs) #giving values of raster the pixel number

#convert to polygon and make sf object
polygon_WA <- rasterToPolygons(raster_WA)
polygon_WA <- st_as_sf(polygon_WA)

#select only the column we need from WA state
washington_UTM <- washington_UTM %>% dplyr::select(postal)

#clip our new 1 km2 polygons to WA state & calculate area
polygon_WA_clip <- st_intersection(polygon_WA, washington_UTM)
polygon_WA_clip$area_km2 <- as.numeric(set_units(st_area(polygon_WA_clip), km^2))

#this is the step that takes forever, getting the roads in each polygon
roads_WA <- st_intersection(roads, polygon_WA_clip)

#will get length of each road that overlaps WA
roads_WA$length <- st_length(roads_WA)
#this step converts to km2 & then accounts for cells that are <1 km2
roads_WA$length_km <- (roads_WA$length/1000)/roads_WA$area_km2

#now we can summarize by length and grid cell of raster layer
final_km <- tapply(roads_WA$length_km, roads_WA$layer, sum)

#now let's assign those values to the raster

road_density <- raster(raster_WA)

road_density[as.integer(names(final_km))] <- final_km #assigning pixel values to the pixel names

plot(road_density, col = rainbow(4, start = 0.7, end = 0.9))

#saving prelim version of this so work doesn't get lost
writeRaster(road_density, "G:/My Drive/Data/Data/GIS/Transportation_Data/road_density_1km.tif", format="GTiff", overwrite=T, NAflag=-9999)

#now we have to disaggregate to 30 m, resample, and mask so it can be in the raster stack
#let's read in one of the final layers
canopy_crop <- raster("G:/My Drive/Data/Data/GIS/a_Final_Rasters/perccanopy.tif")
road_density_30m <- resample(road_density,canopy_crop,method='bilinear')

#fill in zeroes with NA
road_density_30m[is.na(road_density_30m[])] <- 0 
plot(road_density_30m)

#a final mask job
road_density_final <- mask(road_density_30m, washington_UTM_sp)
plot(road_density_final)

#see below for writing this out to file

#test to see if the file will stack with canopy_crop
stack <- stack(road_density_final, canopy_crop)
#preliminary map of data

library(mapview)
library(sf)

ele.m<-ele

#redefine coordinates
coordinates(ele.m) <- ele.m[, c('long', 'lat')]

#assign crs project
proj4string(ele.m) <- CRS('+proj=longlat +datum=WGS84')

#map
mapview(ele.m, zcol="study_area",map.types = c("Esri.WorldImagery"))
mapview(ele.m, zcol="elevation",map.types = c("Esri.WorldImagery"))

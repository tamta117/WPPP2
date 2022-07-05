# get elevation data

library(sp)
library(elevatr)

#define new dataframe
ele<-o

#make utm numeric
ele$utm.e<-as.numeric(ele$utm.e,replace=FALSE)
ele$utm.n<-as.numeric(ele$utm.n,replace=FALSE)

#subset utm zones
ele10<-ele%>%
  subset(utm.z=="10U")
ele11<-ele%>%
  subset(utm.z=="11U")

#define coordinates
coordinates(ele10) <- ele10[, c('utm.n', 'utm.e')]
coordinates(ele11) <- ele11[, c('utm.n', 'utm.e')]

#assign crs project
proj4string(ele10) <- CRS('+proj=utm +zone=10 +datum=WGS84')
proj4string(ele11) <- CRS('+proj=utm +zone=11 +datum=WGS84')

#transform to latitude/longtitude
ele10 <- spTransform(ele10, CRSobj = CRS('+proj=longlat +datum=WGS84'))
ele11 <- spTransform(ele11, CRSobj = CRS('+proj=longlat +datum=WGS84'))

#convert to data frame and bind
ele10 <- as.data.frame(ele10)
ele11 <- as.data.frame(ele11)
ele.b<-bind_rows(ele10,ele11)

#rename columns
names(ele.b)[(ncol(ele.b)-1):ncol(ele.b)] <- c('long', 'lat')

# fetch elevation data
ele.c<-ele.b%>%
  select(long,lat)
prj_dd="EPSG:4326"
ele.sp <- SpatialPoints(ele.c, proj4string = CRS(prj_dd))
ele.f <- get_elev_point(ele.sp, prj = prj_dd, src = "epqs")
ele.f <- as.data.frame(ele.f)
colnames(ele.f)<-c("elevation","unit","long","lat")

#join back with og data
ele<-left_join(ele.b,ele.f)%>%
  select(-unit)
write.csv(ele,here("data/dir.csv"))

#refomat og data

library(here)
library(tidyverse)
library(RODBC)
library(readxl)
library(dplyr)

#format data to resemble each other
oc<-read_excel(here("data/CameraTraps_AllDeadDeer.xlsx"))%>%
  select(Site_ID, Carcass_Type, Study_Area, UTM_Zone, UTM_E, 
         UTM_N, First_Photo_Date, Last_Photo_Date)
colnames(oc)<-c("cam", "carcass", "study_area", "utm.z", "utm.e",
                "utm.n", "first_photo", "last_photo")

o20<-read_excel(here("data/CougarClusterData_Winter2020.xlsx"))%>%
  subset(CameraPlacement=="CSU")%>%
  select(Cluster_ID, Season, HabitatType, 
         DistanceToTrailMeters)
o20$Cluster_ID<-gsub("-CSU", "", as.character(o20$Cluster_ID))
colnames(o20)<-c("cam", "season", "habitat", "trail.m")

o17<-read_excel(here("data/CougarClusterData_2017-2019.xlsx"))%>%
  subset(CameraPlacement=="CSU")%>%
  select(Cluster_ID, Season, HabitatType, 
         DistanceToTrailMeters)
o17$Cluster_ID<-gsub("-CSU", "", as.character(o17$Cluster_ID))
o17$Cluster_ID<-gsub("-A", "", as.character(o17$Cluster_ID))
colnames(o17)<-c("cam", "season", "habitat", "trail.m")

#bind all data into master dataset
o17.20<-bind_rows(o17,o20)
o<-left_join(oc,o17.20)
write.csv(o,here("data/dir.csv"))

#check with online repository
online<-read_excel(here("data/ImageProcessingLog.xlsx"))%>%
  mutate(cam=ClusterID)%>%
  select(cam)
check<-anti_join(o,online,by="cam") # 2 missing cameras

#bind distance data
ogdat<-read.csv("data/dir.csv")
distance<-read_excel(here("data/cam_distance.xlsx"))%>%
  select(cam,building_id,building_distance,road_id,road_distance)
cam_distance<-left_join(ogdat, distance, by="cam")

# #bind roads data
# road<-read_excel(here("data/roads.xlsx"))%>%
#   select(OBJECTID, ROAD_STATUS_LBL, ROAD_USGS_CLASS_LBL)
# colnames(road)<-c("road_id","road_status","road_class")
# cam_complete<-left_join(cam_distance, road, by="road_id")
# write.csv(cam_complete,here("data/dir_final.csv"))

#bind trails data
cam_revised<-read_excel(here("data/cam_trails_building_revised.xlsx"))%>%
  mutate(building_density=Join_Count)%>%
  select(cam, carcass, study_area, utm_z, utm_e, utm_n, long, lat, 
         first_photo, last_photo, season, habitat, elevation, 
         building_density, road_distance, road_class, 
         trail_distance)

#rub time bin function --> all_coug
#bind building distance data
all_coug<-read.csv("data/cam_processed.csv")
cam_distance<-read_excel("data/archive/cam_distance.xlsx")%>%
  select(cam, building_distance)
all_revised<-left_join(all_coug, cam_distance, by="cam")%>%
  filter(Folder == "Reconyx")
write.csv(all_revised, "data/cam_revised.csv")

#bind road density data
all_revised<-read.csv("data/cam_revised.csv")
road_den<-read_excel("data/road_density.xlsx")%>%
  dplyr::select(cam, SHAPE_Length)%>%
  group_by(cam)%>%
  summarize(road_length=sum(SHAPE_Length))%>%
  mutate(road_density=road_length/3281)%>%
  select(-road_length)
all_revised<-left_join(all_revised, road_den, by="cam")

#bind forest density data
forest_den<-read_excel("data/forest_density.xlsx")%>%
  mutate(forest_density=grid_code)%>%
  select(cam, forest_density)
all_revised<-left_join(all_revised, forest_den, by="cam")

#final revision
all<-all_revised%>%
  select(cam, Folder, carcass, study_area, season, elevation, building_density,
         building_distance, road_density, road_distance, trail_distance,
         forest_density, total.time, nobs)%>%
  separate(cam,
           into=c("del1", "sex", "del2"),
           sep=c(6,7), remove=FALSE)%>%
  select(-del1, -del2)
all[is.na(all)]<-0
write.csv(all, "data/all.csv")

#refomat og data

library(here)
library(tidyverse)
library(RODBC)
library(readxl)
library(dplyr)
library(lubridate)

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
# all_revised<-read.csv("data/GIS/cam_revised.csv")
# road_den<-read_excel("data/GIS/road_density.xlsx")%>%
#   dplyr::select(cam, SHAPE_Length)%>%
#   group_by(cam)%>%
#   summarize(road_length=sum(SHAPE_Length))%>%
#   mutate(road_density=road_length/3281)%>%
#   select(-road_length)
# all_revised<-left_join(all_revised, road_den, by="cam")

#bind forest cover data
forest_den<-read_excel("data/GIS/forest_cover.xlsx")%>%
  mutate(forest_density=RASTERVALU)%>%
  select(cam, forest_density)%>%
  distinct(cam, .keep_all = TRUE)
all_revised<-left_join(all_revised, forest_den, by="cam")

#bind trail density data
trail_den<-read_excel("data/GIS/trail_density.xlsx")%>%
  group_by(cam)%>%
  summarize(trail_density = sum(Shape_Length))%>%
  select(cam, trail_density)
all_revised<-left_join(all_revised, trail_den, by="cam")

#final revision
all<-all_revised%>%
  select(cam, Folder, carcass, study_area, season, elevation, building_density,
         building_distance, road_density, road_distance, trail_density, 
         trail_distance, forest_density, total.time, nobs)%>%
  separate(cam,
           into=c("id", "sex", "del2"),
           sep=c(6,7), remove=FALSE)%>%
  unite("coug_id", id:sex, remove=FALSE, sep="")%>%
  select(-id, -del2)
all[is.na(all)]<-0
write.csv(all, "data/all.csv")

#add carcass data
# weight<-read_excel("data/Taylor_carcass.xlsx")%>%
#   select(species, sex, date_capt, year_capt, age_capt, 
#          collar_type, weight_wcollar_kg, mort_date)%>%
#   drop_na()
# mortality<-weight%>%
#   mutate(collar_weight=case_when(
#     collar_type=="ATS-VHF"~0.072,
#     collar_type=="Vectronic-GPS"~0.3,
#     collar_type=="Vectronic-Survey"~0.063),
#     final_weight= weight_wcollar_kg - collar_weight)%>%
#   group_by(species, year_capt)%>%
#   summarise(mean_capt=mean(date_capt),
#             mean_mort=mean(mort_date),
#             mean_wt=mean(final_weight))
# mortality$mean_capt<-format(as.POSIXct(mortality$mean_capt,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
# mortality$mean_mort<-format(as.POSIXct(mortality$mean_mort,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
# mortality<-mortality%>%
#   mutate(time2death=as.numeric(difftime(mean_mort,mean_capt,
#                                         units=c("days"))))
# elk<-mortality%>%
#   subset(species=="elk")%>%
#   mutate(add_wt=time2death*0.8,
#          dead_wt=add_wt+mean_wt,
#          dead_wt=ifelse(dead_wt>96, 96, dead_wt))%>%
#   summarise(final_wt=mean(dead_wt))
# wtd<-mortality%>%
#   subset(species=="wtd")%>%
#   mutate(add_wt=time2death*0.16,
#          dead_wt=add_wt+mean_wt)%>%
#   summarise(final_wt=mean(dead_wt))
# all<-read.csv("data/all_updated.csv")%>%
#   mutate(prey_wt=case_when(
#     carcass=="DeerUnkSpp_Fawn"~20,
#     carcass=="Elk_Calf"~60,
#     carcass=="MD_Adult"~70,
#     carcass=="MD_AgeUnk"~70,
#     carcass=="MD_Fawn"~20,
#     carcass=="MD_Yearling"~70,
#     carcass=="Moose_AgeUnk"~150,
#     carcass=="Moose_Calf"~60,
#     carcass=="Moose_Yearling"~150,
#     carcass=="Other"~70,
#     carcass=="WTD_Adult"~53,
#     carcass=="WTD_Fawn"~20))
# carcass_summer<-read_excel("data/WolfCougarClusters.xlsx")%>%
#   filter(CameraPlacement!="CPU")%>%
#   select(Cluster_ID, PercentCarcassConsumed)%>%
#   filter(!is.na(PercentCarcassConsumed))
# carcass_summer$cam<-gsub("-CSU", "",as.character(carcass_summer$Cluster_ID))
# carcass_winter<-read_excel("data/Winter2020.xlsx")%>%
#   filter(CameraPlacement!="CPU")%>%
#   select(Cluster_ID, PercentCarcassConsumed)%>%
#   filter(!is.na(PercentCarcassConsumed))
# carcass_winter$cam<-gsub("-CSU", "",as.character(carcass_winter$Cluster_ID))
# carcass<-bind_rows(carcass_summer, carcass_winter)
# all_join<-left_join(all, carcass, by="cam")%>%
#   select(-Cluster_ID)
# all_join[is.na(all_join)]<-0
# all_join<-all_join%>%
#   mutate(percent=PercentCarcassConsumed/100,
#          percent_remain=1-percent,
#          wt_remain=percent_remain*prey_wt)%>%
#   select(-PercentCarcassConsumed, -percent, -percent_remain)
# write.csv(all_join, "data/all_updated2.csv")

#re-add road and trail density
all<-read.csv("data/all_updated2.csv")%>%
  select(-road_density, -trail_density)
road_den<-read_excel("data/GIS/cam_road_density.xlsx")%>%
  dplyr::select(cam, Shape_Length)%>%
  group_by(cam)%>%
  summarize(road_length=sum(Shape_Length))%>%
  mutate(road_density=road_length*0.0003048)%>%
  select(-road_length)
all<-left_join(all, road_den, by="cam")
trail_den<-read_excel("data/GIS/cam_trail_density.xlsx")%>%
  dplyr::select(cam, Shape_Length)%>%
  group_by(cam)%>%
  summarize(trail_length=sum(Shape_Length))%>%
  mutate(trail_density=trail_length*0.0003048)%>%
  select(-trail_length)
all<-left_join(all, trail_den, by="cam")
all[is.na(all)]<-0
write.csv(all, "data/all_updated3.csv")

#add carcass weight
w20<-read_excel("data/Winter2020.xlsx")
coug_cluster<-read_excel("data/WolfCougarClusters.xlsx")
database<-bind_rows(w20, coug_cluster)%>%
  mutate(start_date=DateTime_FirstLocation_LMT, 
         cam=Cluster_ID,
         prey_sex=Sex_Final)%>%
  filter(CameraPlacement=="CSU")%>%
  select(cam, prey_sex, start_date)
database$cam<-gsub("-CSU", "", as.character(database$cam))
database$cam<-gsub("-A", "", as.character(database$cam))
all<-read.csv("data/all_updated3.csv")
all2<-left_join(all, database, by="cam")%>%
  separate(carcass, c("species", "age"), "_", 
           remove=FALSE)
test<-all2%>%
  mutate(yr=year(start_date),
         mnth=month(start_date),
         d=day(start_date),
         jday=yday(start_date),
         date=ymd(paste(yr, mnth, d)),
         birth_m=6,
         birth_d=7,
         birthdate=ymd(paste(yr, birth_m, birth_d)),
         time2death=as.numeric(difftime(date, birthdate,
                                        units=c("days"))),
         weight=case_when(
           species=="WTD" & age=="Fawn"~time2death*0.16,
           species=="MD" & age=="Fawn"~time2death*0.21, #replace 0.20 with actual numbers
           age=="Adult"~100,
           species=="Moose"~100,
           species=="Elk"~100,
           species=="MD" & age=="Yearling" & season=="Winter"~100,
           species=="MD" & age=="Yearling" & season=="Fall"~100,
           cam=="NEC108M-2031"~time2death*0.21,
           cam=="MVC227F-4361"~38+time2death*0.07,
           cam=="MVC205F-729" | cam=="MVC237M-060-16-10C" | cam=="MVC228F-2274"~100,
           cam=="MVC235M-033-10-30C" | cam=="NEC144M-1321"~0,
           cam=="MVC235M-055-15-20C"~100), #mule deer, killed Feb, classified as juv or adult, metatarsal 11
         wt_class=case_when(
           weight<=20 ~ "small",
           weight>20 & weight<=40 ~ "medium",
           weight>40 ~ "large"))%>%
  select(cam, coug_id, sex, carcass, weight, wt_class, study_area, season, elevation, building_density, building_distance, road_density, road_distance, trail_density, trail_distance, forest_density, nobs)
write.csv(test, "data/all_with_weight.csv")

#cluster data
cluster<-read.csv("data/Carcass_AllClusters.csv")%>%
  filter(CarcassFound==1)%>%
  mutate(duration.hr=time.span..hours.,
         lat=centroid.latitude,
         long=centroid.longitude)%>%
  select(Cluster, lat, long, duration.hr)
write.csv(cluster, "data/cluster_total_time.csv")
cluster_building_den<-read_excel("data/GIS/cluster_building_density.xlsx")%>%
  group_by(Cluster)%>%
  summarize(building_density=n())
cluster_building_dis<-read_excel("data/GIS/cluster_building_distance.xlsx")%>%
  mutate(building_distance=NEAR_DIST/1000)%>%
  select(Cluster, building_distance)
cluster_road_den<-read_excel("data/GIS/cluster_road_density.xlsx")%>%
  group_by(Cluster)%>%
  summarize(road_density=n())
cluster_road_dis<-read_excel("data/GIS/cluster_road_distance.xlsx")%>%
  mutate(road_distance=NEAR_DIST/1000)%>%
  select(Cluster, road_distance)
cluster_trail_den<-read_excel("data/GIS/cluster_trail_density.xlsx")%>%
  group_by(Cluster)%>%
  summarize(trail_density=n())
cluster_trail_dis<-read_excel("data/GIS/cluster_trail_distance.xlsx")%>%
  mutate(trail_distance=NEAR_DIST/1000)%>%
  select(Cluster, trail_distance)
cluster_building<-merge(cluster_building_den, cluster_building_dis, all=TRUE)
cluster_trail<-merge(cluster_trail_den, cluster_trail_dis, all=TRUE)
cluster_road<-merge(cluster_road_den, cluster_road_dis, all=TRUE)
cluster_gis<-bind_cols(cluster_building, cluster_trail, cluster_road)%>%
  mutate(Cluster=Cluster...1)%>%
  select(Cluster, building_density, building_distance, road_density, road_distance, trail_density, trail_distance)
cluster_gis[is.na(cluster_gis)]<-0
cluster_merge<-left_join(cluster, cluster_gis, by="Cluster")%>%
  select(-lat, -long)
cluster_forest<-read_excel("data/GIS/cluster_forest_density.xlsx")%>%
  mutate(forest_density=RASTERVALU)%>%
  select(Cluster, forest_density)
cluster.f<-left_join(cluster_merge, cluster_forest, by="Cluster")
write.csv(cluster.f, "data/cluster_gis.csv")

#add season to cluster data
cluster_o<-read.csv("data/Carcass_AllClusters.csv")
cluster_o$date<-lubridate::parse_date_time(cluster_o$first.fix.LMT, orders = c('mdy HMS','dmy HMS'))
cluster_season<-cluster_o%>%  
  mutate(yr=year(date),
         mnth=month(date),
         d=day(date),
         jday=yday(date),
         start.date=ymd(paste(yr, mnth, d)),
         season=ifelse(jday>150 & jday<349, "summer", "winter"))%>%
  select(Cluster, season)
cluster_season_full<-left_join(cluster.f, cluster_season, by="Cluster")
write.csv(cluster_season_full, "data/cluster.csv")

#cluster data with weight
w20<-read_excel("data/Winter2020.xlsx")
coug_cluster<-read_excel("data/WolfCougarClusters.xlsx")
cluster<-read.csv("data/cluster.csv")

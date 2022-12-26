library(tidyverse)
library(lubridate)
library(data.table)

#create list of all processed csv file names
setwd("C:/Users/Jelly Oon/Documents/WPPP2/data/cam/")
setwd("C:/Users/Jelly Oon/Documents/WPPP2")
file.list <- dir(pattern = "\\.csv$")

#write function
deer_count<-function(filename){
  cam_file<-read.csv(filename)%>%
    mutate(across(everything(), as.character))
  deer<-cam_file%>%
    mutate(DEER=ifelse(Species1=="Mule Deer" | Species1=="Unknown Deer" | Species1=="White-tailed Deer",1,0),
           species=Species1)%>%
    filter(DEER==1)%>%
    select(ClusterID, species)
}

deer_count<-function(filename){
  cam_file<-read.csv(filename)%>%
    unite("date_time", Date:Time, remove=FALSE, sep=" ")
  cam_file$date_time2<-parse_date_time(cam_file$date_time, orders = c('ymd HMS','dmy HMS'))
  deer<-cam_file%>%
    mutate(time.bin=format(ceiling_date(date_time2, "30 mins"), "%Y%m%d %H%M"),
           DEER=ifelse(Species1=="Mule Deer" | Species1=="Unknown Deer" | Species1=="White-tailed Deer",1,0),
           species=Species1)%>%
    filter(DEER==1)%>%
    distinct(time.bin,.keep_all = TRUE)%>%
    select(ClusterID, species)
}

#test function
test<-deer_count("data/cam/MVC202F-3281-Reconyx_Checked.csv")

#run function
deer<-lapply(file.list, deer_count)
all_deer<-bind_rows(deer)
deer_count<-all_deer%>%
  group_by(ClusterID, species)%>%
  summarize(count=n())%>%
  mutate(cam=ClusterID)%>%
  ungroup()%>%
  select(-ClusterID)

#run function for zero cougars on all csv
nodeer<-lapply(file.list, coug_bin0)
nodeer_merge<-bind_rows(nodeer)%>%
  mutate(count=nobs)%>%
  select(-nobs)%>%
  distinct(cam,.keep_all = TRUE)

#find sites with zero cougars
true.nodeer<-anti_join(nodeer_merge, deer_count, by="cam")

#bind all sites and write csv
processed<-bind_rows(true.nodeer, deer_count)

#bind with all.csv and write csv
all<-read.csv("data/all_with_weight.csv")
all_with_deer<-left_join(all, processed, by="cam")%>%
  select(cam, species, count, building_density)

#graph deer count
deer_model<-glm(count~building_density, family=poisson(link = log), all_with_deer)
summary(deer_model)
ggplot(all_with_deer, aes(building_density, count))+
  geom_jitter()
#no pattern between building density and deer
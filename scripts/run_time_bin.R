#get cougar count for 30 minutes bin of each camera set
library(dplyr)
library(lubridate)
library(here)

#make empty table of just camera
all<-read.csv("data/all.csv")
cam<-all%>%
  distinct(cam, .keep_all = TRUE)%>%
  select(cam)

#make list of all csv
#make sure to reset working directory to the project after running
setwd("C:/Users/Jelly Oon/Documents/WPPP2/data/cam/")
setwd("C:/Users/Jelly Oon/Documents/WPPP2")
file.list <- dir(pattern = "\\.csv$")

#run function for no cougars
nocoug<-lapply(file.list, coug_bin0)
nocoug_merge<-left_join(cam, bind_rows(nocoug), by="cam")

#run function for yes cougars
yescoug <- lapply(file.list, coug_bin1)
yescoug_merge<-left_join(cam, bind_rows(yescoug), by="cam")%>%
  filter(!is.na(nobs))

#find true no cougars
true.nocoug<-anti_join(nocoug_merge, yescoug_merge, by=c("cam","Folder"))

#bind yes cougars with true no cougars
all_coug<-bind_rows(yescoug_merge, true.nocoug)

#write csv
write.csv(all_coug,"data/cam_processed.csv")

#bind with all file
all<-all%>%
  select(-total.time, -nobs)
all<-left_join(all, all_coug, by=c("cam", "Folder"))
write.csv(all,"data/all.csv")

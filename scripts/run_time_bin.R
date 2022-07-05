#get cougar count for 30 minutes bin of each camera set
library(dplyr)
library(lubridate)
library(here)

#make list of all csv
#make sure to reset working directory to the project after running
setwd("C:/Users/Jelly Oon/Documents/WPPP/data/cam/")
file.list <- dir(pattern = "\\.csv$")

#run function for no cougars
nocoug<-lapply(file.list, coug_bin0)
nocoug_merge<-left_join(cam_revised, bind_rows(nocoug), by="cam")%>%
  filter(!is.na(Folder))

#run function for yes cougars
yescoug <- lapply(file.list, coug_bin1)
yescoug_merge<-left_join(cam_revised, bind_rows(yescoug), by="cam")%>%
  filter(!is.na(Folder))

#find true no cougars
true.nocoug<-anti_join(nocoug_merge, yescoug_merge, by=c("cam","Folder"))

#bind yes cougars with true no cougars
all_coug<-bind_rows(yescoug_merge, true.nocoug)

#write csv
write.csv(all_coug,"data/cam_processed.csv")

#function for feeding duration of scavengers

library(dplyr)
library(lubridate)
library(tidyr) #unite
library(here)

#function to replace values larger than 0 with 1
replacesum <- function(x) {
  if_else(x>=1, 1, 0)
}

#function to find nobs for scavengers
sca_bin<-function(fname){
cam<-read.csv(fname)
cam = cam[-1,] #remove first row
cam_date<-cam%>%
  unite("date_time", Date:Time, remove=FALSE, sep=" ")
cam_date$date_time2<-parse_date_time(cam_date$date_time, orders = c('ymd HMS','dmy HMS'))
cam_nobs<-cam_date%>%
  mutate(cam=ClusterID,
         time.bin=format(ceiling_date(date_time2, "30 mins"), "%Y%m%d %H%M"), #split into 30 min bins
         end.date=cam_date[1,38]+2592000, #new column for one month from start date
         MAGP = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Magpie')),
         EAGL = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Eagle')),
         RAVE = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Raven')),
         VULT = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Vulture')),
         COWS = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Cattle')),
         BEAR = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Black Bear')),
         BOBC = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Bobcat')),
         COYO = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Coyote')),
         WOLF = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Wolf')),
         COUG = ifelse(Species1 == "Cougar" & Collar == "true", 1, 0))%>%
  subset(date_time2 <= end.date) #filter for one month from start date
cam_coug<-cam_nobs%>%
  filter(COUG==1)
last_coug = tail(cam_coug, n =1)
cam_fin<-cam_nobs%>%
  filter(date_time2<=last_coug[1,38])%>%
  select(cam, Folder, time.bin, MAGP, EAGL, RAVE, VULT, COWS, BEAR, BOBC, COYO, WOLF)
}

#test function
test<-sca_bin("data/cam/MVC237M-060-16-10C-Reconyx_Checked.csv")

#run nobs function for scavengers
#create list of all processed csv file names
setwd("C:/Users/Jelly Oon/Documents/WPPP2") 
file.list <- dir(path="data/cam", pattern = "\\.csv$")

#run function
setwd("C:/Users/Jelly Oon/Documents/WPPP2/data/cam/") #set working directory so it knows where to grab files
sca_list<-lapply(file.list, sca_bin)

#format data
sca_nobs<-bind_rows(sca_list)%>%
  group_by(cam, time.bin)%>%
  summarize(across(where(is.numeric), sum))%>% #total number of images of each species in each time bin
  mutate(across(where(is.numeric), replacesum))%>% #replace with binary 1/0
  summarize(across(where(is.numeric), sum)) #total number of time bin for each species

#merge with cougar dataset
setwd("C:/Users/Jelly Oon/Documents/WPPP2") 
all<-read.csv("data/all_with_weight.csv")
all_sca<-left_join(all, sca_nobs, by="cam")%>%
  mutate(COUG=nobs)%>%
  select(-nobs)
all_sca = all_sca[-36,]

#write data
write.csv(all_sca, "data/all_sca_1226.csv")

#find cameras where there was a cougar
sca_yescoug<-anti_join(sca_nobs, nocoug, by="cam")
sca_nocoug<-semi_join(all_sca, nocoug, by="cam")%>%
  select(cam, 19:27)
sca_bind<-bind_rows(sca_yescoug, sca_nocoug)
all_sca_fin<-left_join(all, sca_bind, by="cam")%>%
  mutate(COUG=nobs)%>%
  select(-nobs)
write.csv(all_sca_fin, "data/all_sca_0106.csv")

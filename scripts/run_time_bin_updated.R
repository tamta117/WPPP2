library(tidyverse)
library(lubridate)
library(data.table)

#create list of all processed csv file names
setwd("C:/Users/Jelly Oon/Documents/WPPP2/data/cam/")
setwd("C:/Users/Jelly Oon/Documents/WPPP2")
file.list <- dir(path="data/cam", pattern = "\\.csv$")

#write function
timebin<-function(filename){
test<-read.csv(filename)%>%
  mutate(across(everything(), as.character))
test<-test%>%
  unite("date_time", Date:Time, remove=FALSE, sep=" ")
test$date_time2<-parse_date_time(test$date_time, orders = c('ymd HMS','dmy HMS'))
test2<-test%>%
  mutate(time.bin=format(ceiling_date(date_time2, "30 mins"), "%Y%m%d %H%M"),
         end.date=test[1,38]+2592000,
         COUG=ifelse(Species1=="Cougar" & Collar=="true",1,0))%>%
  filter(date_time2 <= end.date)%>%
  filter(COUG==1)%>%
  distinct(time.bin,.keep_all = TRUE)
}

#test function
a<-timebin("data/cam/MVC237M-060-16-10C-Reconyx_Checked.csv")

#run function
all.csv<-lapply(file.list, timebin)
csv.merge<-bind_rows(all.csv)
time.bin<-csv.merge%>%
  group_by(ClusterID)%>%
  distinct(time.bin,.keep_all = TRUE)%>%
  summarize(nobs=n())%>%
  mutate(cam=ClusterID)%>%
  select(cam, nobs)

#run function for zero cougars on all csv
nocoug<-lapply(file.list, coug_bin0)
nocoug.merge<-bind_rows(nocoug)%>%
  distinct(cam,.keep_all = TRUE)

#find sites with zero cougars
true.nocoug<-anti_join(nocoug.merge, time.bin, by="cam")

#bind all sites and write csv
processed<-bind_rows(true.nocoug, time.bin)
write.csv(processed, "data/updated_nobs.csv")

#bind with all.csv and write csv
all<-read.csv("data/all.csv")%>%
  distinct(cam,.keep_all = TRUE)%>%
  select(-nobs, -total.time)
all2<-left_join(all, processed, by="cam")
write.csv(all2, "data/all_updated.csv")

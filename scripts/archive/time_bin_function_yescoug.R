#function to process checked camera trap files with cougars

library(dplyr)
library(lubridate)

coug_bin1<-function(fname){
  cam<-read.csv(fname)%>%
    unite("setup_datetime", SetUpDay:SetUpTime, remove=FALSE, sep=" ")%>%
    unite("date_time", Date:Time, remove=FALSE, sep=" ")
  cam$date_time2<-parse_date_time(cam$date_time, orders = c('dmy HMS','dmy HMS'))
  cam$setup_datetime2<-parse_date_time(cam$setup_datetime, orders = c('mdy HMS','dmy HMS'))
  cam2<-cam%>%
    mutate(time.bin=format(ceiling_date(date_time2, "30 mins"), "%Y%m%d %H%M"),
           end.date=cam[1,40]+2592000,
           COUG=ifelse(Species1=="Cougar" & Collar=="true",1,0))%>%
    subset(date_time2 <= end.date)%>%
    filter(COUG==1)
  cam.time<-cam2%>%
    mutate(total.time=as.numeric(difftime(cam2[nrow(cam2),39],cam2[1,39],
                                          units=c("hours"))),
           cam=ClusterID)%>%
    distinct(time.bin,.keep_all = TRUE)%>%
    group_by(cam, Folder, total.time, COUG)%>%
    summarize(nobs=n())%>%
    select(-COUG)
}

#test function
test3<-read.csv("data/cam/MVC205F-729-Moultrie_Checked.csv")
test<-read.csv("data/cam/MVC205F-729-Moultrie_Checked.csv")%>%
  unite("setup_datetime", SetUpDay:SetUpTime, remove=FALSE, sep=" ")%>%
  unite("date_time", Date:Time, remove=FALSE, sep=" ")
test$date_time2<-parse_date_time(test$date_time, orders = c('mdy HMS','dmy HMS'))
test$setup_datetime2<-parse_date_time(test$setup_datetime, orders = c('mdy HMS','dmy HMS'))
test2<-test%>%
  mutate(time.bin=format(ceiling_date(date_time2, "30 mins"), "%Y%m%d %H%M"),
         end.date=test[1,40]+2592000,
         COUG=ifelse(Species1=="Cougar" & Collar=="true",1,0))%>%
  subset(date_time2 <= end.date)%>%
  filter(COUG==1)
test.time<-test2%>%
  mutate(total.time=as.numeric(difftime(test2[nrow(test2),39],test2[1,39],
                                        units=c("hours"))),
         test=ClusterID)%>%
  distinct(time.bin,.keep_all = TRUE)%>%
  group_by(test, Folder, total.time, COUG)%>%
  summarize(nobs=n())%>%
  select(-COUG)
test<-coug_bin1("data/cam/NEC145M-1257-Reconyx_Checked.csv")
test3 = test3[-1,] 

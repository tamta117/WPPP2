#function to process checked camera trap files with cougars

library(dplyr)
library(lubridate)

coug_bin1<-function(fname){
  cam<-read.csv(fname)
  cam = cam[-1,]
  cam<-cam%>%
    unite("date_time", Date:Time, remove=FALSE, sep=" ")
  cam$date_time2<-parse_date_time(cam$date_time, orders = c('ymd HMS','dmy HMS'))
  cam2<-cam%>%
    mutate(time.bin=format(ceiling_date(date_time2, "30 mins"), "%Y%m%d %H%M"),
           end.date=cam[1,38]+2592000,
           COUG=ifelse(Species1=="Cougar" & Collar=="true",1,0))%>%
    subset(date_time2 <= end.date)%>%
    filter(COUG==1)
  cam.time<-cam2%>%
    mutate(total.time=as.numeric(difftime(cam2[nrow(cam2),38],cam2[1,38],
                                          units=c("hours"))),
           cam=ClusterID)%>%
    distinct(time.bin,.keep_all = TRUE)%>%
    group_by(cam, Folder, total.time, COUG)%>%
    summarize(nobs=n())%>%
    select(-COUG)
}

#test function
test<-coug_bin1("data/cam/MVC202F-3539-Moultrie_Checked.csv")

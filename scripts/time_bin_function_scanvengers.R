#function to process checked camera trap files for scavengers

library(dplyr)
library(lubridate)

sca_bin1<-function(fname){
  cam<-read.csv(fname)
  cam = cam[-1,]
  cam<-cam%>%
    unite("date_time", Date:Time, remove=FALSE, sep=" ")
  cam$date_time2<-parse_date_time(cam$date_time, orders = c('ymd HMS','dmy HMS'))
  cam.time<-cam%>%
    mutate(time.bin=format(ceiling_date(date_time2, "30 mins"), "%Y%m%d %H%M"),
           end.date=cam[1,38]+2592000)%>%
    subset(date_time2 <= end.date)
  cora<-cam.time%>%
    filter(Species1=="Raven" | Species2=="Raven" | Species3=="Raven")%>%
    mutate(CORA1=1)
  coyo<-cam.time%>%
    filter(Species1=="Coyote" | Species2=="Coyote" | Species3=="Coyote")%>%
    mutate(COYO1=1)
  bear<-cam.time%>%
    filter(Species1=="Black bear" | Species2=="Black bear" | Species3=="Black bear")%>%
    mutate(BEAR1=1)
  cam.merge<-bind_rows(cora, coyo, bear)
  cam.merge[is.na(cam.merge)]<-0
  cam.time<-cam.merge%>%
    mutate(total.time=as.numeric(difftime(cam.merge[nrow(cam.merge),38],cam.merge[1,38],
                                          units=c("hours"))),
           cam=ClusterID)%>%
    distinct(time.bin,.keep_all = TRUE)%>%
    group_by(cam, Folder)%>%
    summarize(CORA=sum(CORA1),COYO=sum(COYO1), BEAR=sum(BEAR1))
}

#test function
test<-read.csv("data/cam/MVC202F-2588-Moultrie_Checked.csv")%>%
  filter(Species1=="Raven" | Species2=="Raven" | Species3=="Raven")%>%
  mutate(CORA = 1)
test<-read.csv("data/cam/MVC202F-2588-Moultrie_Checked.csv")%>%
  mutate(CORA=ifelse(Species1=="Raven" | Species2=="Raven" | Species3=="Raven", 1, 0))%>%
  mutate(CORA = 1)
test<-sca_bin1("data/cam/MVC202F-2588-Moultrie_Checked.csv")

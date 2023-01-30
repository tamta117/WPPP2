#function for number of scavenger photos by day


library(dplyr)

#function
sca_day<-function(fname){
cam<-read.csv(fname)
cam$date2<-parse_date_time(cam$Date,  
                               orders = c('ymd','dmy'),
                               tz="")
cam1<-cam%>%
  mutate(start_date=cam[1,37],
         day_cluster=as.numeric(difftime(date2, start_date, units=c("days"))),
         day_cluster=day_cluster+1,
         day_round=round(day_cluster, 0),
         MAGP = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Magpie')),
         EAGL = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Eagle')),
         RAVE = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Raven')),
         VULT = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Vulture')),
         COWS = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Cattle')),
         BEAR = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Black Bear')),
         BOBC = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Bobcat')),
         COYO = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Coyote')),
         WOLF = +(if_any(c("Species1", "Species2", "Species3"), ~ . %in% 'Wolf')))%>%
  select(ClusterID, Folder, day_round, 40:49)%>%
  filter(day_round<31)
}

#test function
test<-sca_day("data/cam/MVC202F-2588-Moultrie_Checked.csv")

#write list of all processed csvs
setwd("C:/Users/Jelly Oon/Documents/WPPP2") 
file.list <- dir(path="data/cam", pattern = "\\.csv$")

#run function
setwd("C:/Users/Jelly Oon/Documents/WPPP2/data/cam/") #set working directory so it knows where to grab files
sca_list<-lapply(file.list, sca_day)

#format data
sca_nobs<-bind_rows(sca_list)%>%
  group_by(ClusterID, Folder, day_round)%>%
  summarize(across(1:9, sum))%>%
  filter(Folder=="Reconyx")
sca_nobs1<-sca_nobs
  pivot_longer(cols=2:10,
               names_to="scavengers",
               values_to="count")

#plot
ggplot(sca_nobs1, aes(x=day_round, y=count, fill=scavengers))+
  geom_bar(stat = "identity", position = position_dodge(0.9))
ggplot(sca_nobs, aes(x=day_round, y=RAVE, group=day_round))+
  geom_bar(stat = "identity", position = position_dodge(0.9))

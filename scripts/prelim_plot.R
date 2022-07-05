#preliminary plots

library(ggplot2)
library(ggpubr)

#read in data
all_revised<-read.csv("data/cam_revised.csv")

#plot elevation vs road distance and building density
ggplot(all_revised)+
  geom_point(aes(x=elevation,y=building_density),col="red")+
  geom_point(aes(x=elevation,y=road_distance),col="blue")+
  geom_smooth(aes(x=elevation,y=building_density),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="red")+
  geom_smooth(aes(x=elevation,y=road_distance),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="blue")+
  ylab("Distance (m)")+
  xlab("Elevation (m)")

#plot road distance 
#closer to road = higher total time
#closer to road = higher feeding duration
ggplot(all_revised)+
  geom_point(aes(x=road_distance, y=total.time))+
  geom_smooth(aes(x=road_distance,y=total.time),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")
ggplot(all_revised)+
  geom_point(aes(x=road_distance, y=nobs))+
  geom_smooth(aes(x=road_distance,y=nobs),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")

#plot building density
#less building density = higher total time
#less buliding density = ?????
ggplot(all_revised)+
  geom_point(aes(x=building_density, y=total.time))+
  geom_smooth(aes(x=building_density, y=total.time),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")
ggplot(all_revised)+
  geom_point(aes(x=building_density, y=nobs))+
  geom_smooth(aes(x=building_density, y=nobs),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")

#plot trail distance
#closer to trails = higher total time
#closer to trails = lower feeding duration
ggplot(all_revised)+
  geom_point(aes(x=trail_distance, y=total.time))+
  geom_smooth(aes(x=trail_distance, y=total.time),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")
ggplot(all_revised)+
  geom_point(aes(x=trail_distance, y=nobs))+
  geom_smooth(aes(x=trail_distance, y=nobs),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")

#plot building distance
ggplot(all_revised)+
  geom_point(aes(x=building_distance, y=total.time))+
  geom_smooth(aes(x=building_distance, y=total.time),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")
ggplot(all_revised)+
  geom_point(aes(x=building_distance, y=nobs))+
  geom_smooth(aes(x=building_distance, y=nobs),
              method=lm, se=FALSE, linetype="dashed", 
              size=0.5, color="black")

#plot camera brand
ggplot(all_revised)+
  geom_boxplot(aes(x=Folder, y=total.time))
ggplot(all_revised)+
  geom_boxplot(aes(x=Folder, y=nobs))

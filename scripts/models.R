#preliminary model

#load library
library(lme4)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(glmmTMB)
library(AICcmodavg)
library(MuMIn)

#load data
all<-read.csv("data/all_updated3.csv")%>%
  select(cam, wt_remain, nobs, building_density, building_distance,
         road_density, road_distance, trail_density, trail_distance, 
         forest_density)

#scale
all_scale <- scale(all[4:10])
all.mod <- data.frame("cam" = all$cam,"wt_remain" = all$wt_remain, "nobs" = all$nobs, all_scale)

#model
(null <- glmmTMB(nobs~1, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m1 <- glmmTMB(nobs~building_density, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m2 <- glmmTMB(nobs~building_distance, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m3 <- glmmTMB(nobs~road_density, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m4 <- glmmTMB(nobs~log(road_distance+1), 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m5 <- glmmTMB(nobs~trail_density, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m6 <- glmmTMB(nobs~trail_distance, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m7 <- glmmTMB(nobs~trail_distance+road_density+building_distance, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m8 <- glmmTMB(nobs~trail_distance+road_density+building_distance+forest_density, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m9 <- glmmTMB(nobs~trail_distance+road_density*road_distance+building_distance+forest_density, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m10 <- glmmTMB(nobs~trail_distance+road_density+building_distance+forest_density+road_distance, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m11 <- glmmTMB(nobs~trail_distance+road_density+building_distance+forest_density+road_distance+building_density, 
                zi=~1, 
                family=list(family="truncated_poisson", link="log"), all.mod))
(m12 <- glmmTMB(nobs~trail_distance+trail_density+road_density+building_distance+forest_density+road_distance, 
                zi=~1, 
                family=list(family="truncated_poisson", link="log"), all.mod))
(m13 <- glmmTMB(nobs~trail_distance*trail_density+road_density*road_distance+building_distance*building_density+forest_density, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m14 <- glmmTMB(nobs~trail_distance+trail_density+road_density+road_distance+building_distance+building_density+forest_density, 
                zi=~1, 
                family=list(family="truncated_poisson", link="log"), all.mod))
summary(m10)

#AIC testing
#use dredge on overall model
dredge(test, fixed = zi((Int)))

#AIC table
#the models you're comparing
model.set <- list(null, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14)
model.names<-list(c("null", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14"))

#makes the table
aic <- aictab(model.set, modnames=model.names)
aic

#histograms of predictors
p1<-ggplot(all, aes(x=log(building_distance+1)))+
  geom_histogram()
p2<-ggplot(all, aes(x=road_density))+
  geom_histogram()
p3<-ggplot(all, aes(x=trail_density))+
  geom_histogram()
p4<-ggplot(all, aes(x=forest_density))+
  geom_histogram()
p5<-ggplot(all, aes(x=elevation))+
  geom_histogram()
ggplot(all, aes(x=carcass, y=forest_density))+
  geom_boxplot()+
  scale_x_discrete(limits = c("MD_Fawn", "MD_Yearling","MD_Adult", "MD_AgeUnk","WTD_Fawn","WTD_Adult","Elk_Calf","Moose_Calf","Moose_Yearling","Moose_AgeUnk", "DeerUnkSpp_Fawn","Other"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))
p1
#combine the graphs
(p1 + p2) / (p3 + p4) / p5

#compare Moultrie vs Reconyx
ggplot(all, aes(x=nobs, color=Folder)) +
  geom_histogram(fill="white")
ggplot(all, aes(x=total.time, color=Folder)) +
  geom_histogram(fill="white")

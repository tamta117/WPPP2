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
all<-read.csv("data/all_with_weight.csv")%>%
  select(cam, sex, wt_class, nobs, building_density, building_distance,
         road_density, road_distance, trail_density, trail_distance, 
         forest_density)

#scale
all_scale <- scale(all[5:11])
all.mod <- data.frame("cam" = all$cam, "sex" = all$sex, "wt_class" = all$wt_class, "nobs" = all$nobs, all_scale)
all.mod[7,3]<-"large"

#model
(null <- glmmTMB(nobs~1, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
summary(null)

(m1 <- glmmTMB(nobs~building_density+road_density+trail_density+forest_density+wt_class, 
                zi=~1, 
                family=list(family="truncated_poisson", link="log"), all.mod))
summary(m1)
sadnobs<-dredge(m1)

(m2 <- glmmTMB(nobs~building_density+road_density+forest_density, 
               zi=~wt_class, 
               family=list(family="truncated_poisson", link="log"), all.mod))
summary(m2)

(m3 <- glmmTMB(nobs~1, 
               zi=~building_density+road_density+trail_density+forest_density+wt_class, 
               family=list(family="truncated_poisson", link="log"), all.mod))
sadnobs.zi<-dredge(m3)


(m4 <- glmmTMB(nobs~building_density+road_density+trail_density+forest_density, 
               zi=~1, 
               family=list(family="truncated_poisson", link="log"), all.mod))
summary(m4)

#correlation analysis
head(all.mod)
all.mod <- all.mod[,-c(1,2,3)] #remove first three columns
scaled <- scale(all.mod) 
dim(scaled)
M <- cor(scaled)
library(corrplot)
corrplot(M, method="number", type = "upper")

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

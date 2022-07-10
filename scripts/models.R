#preliminary model

#load library
library(lme4)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(MuMIn)

#load data
all<-read.csv("data/all.csv")

#scale
all_scale <- scale(all[10:17])
all.mod <- data.frame("cam" = all$cam, "coug_id" = all$coug_id, "sex" = all$sex, "Folder" = all$Folder,
                      "carcass" = all$carcass, "study_area" = all$study_area,
                      "season" = all$season, "total.time" = all$total.time,
                      "nobs" = all$nobs, "CORA" = all$CORA, all_scale)
#model
test<-lmer(nobs ~ building_density + road_density + trail_density +
             forest_density + elevation + season + (1|coug_id), 
           data = all.mod)
test<-lm(CORA ~ building_density + road_density + trail_density +
             forest_density + elevation + season, 
           data = all.mod, na.action = "na.fail")
test<-lm(total.time ~ building_density + road_density + trail_distance +
           forest_density + elevation + season, 
         data = all.mod)
summary(test)
dredge(test)

all<-all%>%
  filter(Folder=="Reconyx")
#histograms of predictors
p1<-ggplot(all, aes(x=building_density))+
  geom_histogram()
p2<-ggplot(all, aes(x=road_density))+
  geom_histogram()
p3<-ggplot(all, aes(x=trail_density))+
  geom_histogram()
p4<-ggplot(all, aes(x=forest_density))+
  geom_histogram()
p5<-ggplot(all, aes(x=elevation))+
  geom_histogram()

#combine the graphs
(p1 + p2) / (p3 + p4) / p5

#compare Moultrie vs Reconyx
ggplot(all, aes(x=nobs, color=Folder)) +
  geom_histogram(fill="white")
ggplot(all, aes(x=total.time, color=Folder)) +
  geom_histogram(fill="white")

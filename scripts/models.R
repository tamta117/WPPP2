#preliminary model

#load library
library(lme4)
library(tidyverse)

#load data
all<-read.csv("data/all.csv")

#scale
all_scale <- scale(all[8:16])
all.mod <- data.frame("cam" = all$cam, "sex" = all$sex, "Folder" = all$Folder,
                      "carcass" = all$carcass, "study_area" = all$study_area,
                      "season" = all$season,all_scale)
#model
test<-lmer(nobs ~ building_density + road_density + trail_distance +
             forest_density + elevation + season + (1|study_area), 
           data = all.mod)
test<-lm(nobs ~ building_density + road_density + trail_distance +
             forest_density + elevation + season, 
           data = all.mod)
test<-lm(total.time ~ building_density + road_density + trail_distance +
           forest_density + elevation + season, 
         data = all.mod)
summary(test)

#plot range of forest density
test<-all%>%
  subset(building_density <= 40)
range(all$forest_density)
range(all$building_density)
range(all$road_density)
hist(all$road_density)
hist(test$building_density)
hist(all$forest_density)
hist(all$elevation)
hist(test$trail_distance)
hist(all$season)

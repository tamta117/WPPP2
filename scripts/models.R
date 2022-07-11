#preliminary model

#load library
library(lme4)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(glmmTMB)

#load data
all<-read.csv("data/all_updated.csv")

#scale
all_scale <- scale(all[11:18])
all.mod <- data.frame("cam" = all$cam, "coug_id" = all$coug_id, "sex" = all$sex,
                      "carcass" = all$carcass, "study_area" = all$study_area,
                      "season" = all$season,"nobs" = all$nobs, all_scale)
#model
#zero-inflated: positive coeff = covariate increases probability of 0
#of zero cougars (nobs)
#higher building density increases chance of getting zero coug = less likely coug
#higher road density decreases chance of getting zero coug = more likely coug
#conditional: positive coeff = covariate increases probability of 1, removed 0
#for sites that had a cougar, both road and bulding density decreases chance of getting coug
#building_density has bigger effect
(m <- glmmTMB(nobs~road_density + building_density + forest_density + I(forest_density^2) + (1|cam) + (1|carcass), 
               zi=~road_density + building_density + forest_density+ I(forest_density^2), 
               family=list(family="truncated_poisson", link="log"), all.mod))
(m1 <- glmmTMB(nobs~road_density + (1|cam) + (1|carcass), 
              zi=~building_density, 
              family=list(family="truncated_poisson", link="log"), all.mod))

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

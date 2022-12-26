library(lme4)
library(tidyverse)

cluster<-read.csv("data/cluster.csv")%>%
  filter(duration.hr<263)%>%
  distinct(Cluster,.keep_all = TRUE)
cluster_scale<-scale(cluster[5:11])
cluster.mod<-data.frame("cam"=cluster$Cluster, "total.hr"=cluster$duration.hr,
                        "season"=cluster$season, cluster_scale)

null<-glm(total.hr~1, cluster.mod, family=poisson(link = log))
summary(null)

c1<-glm(total.hr~building_density, family=poisson(link = log), cluster.mod)
summary(c1)
c2<-glm(total.hr~road_density, family=poisson(link = log), cluster.mod)
summary(c2)
c3<-glm(total.hr~trail_density, family=poisson(link = log), cluster.mod)
summary(c3)
c4<-glm(total.hr~forest_density, family=poisson(link = log), cluster.mod)
summary(c4)
c5<-glm(total.hr~season, family=poisson(link = log), cluster.mod)
summary(c5)
#building and season are significant
c6<-glm(total.hr~building_density+season, data=cluster.mod,
       na.action = "na.fail", family = poisson(link = log))
c7<-glm(total.hr~building_density+road_density+trail_density+season, data=cluster.mod,
        na.action = "na.fail", family = poisson(link = log))
summary(c7)
sadcluster<-dredge(c7)
#building and season is best, season on its own is -25.47 AIC, 
#building on its own is -424.31 AIC
summary(c6)

#find residuals
qqnorm(resid(c6))
qqline(resid(c6))
plot(cluster.mod$building_density,resid(c6))

library(lme4)

cluster<-read.csv("data/cluster.csv")%>%
  filter(duration.hr<=263)%>%
  distinct(Cluster,.keep_all = TRUE)
cluster_scale<-scale(cluster[5:11])
cluster.mod<-data.frame("cam"=cluster$Cluster, "total.hr"=cluster$duration.hr,
                        "season"=cluster$season, cluster_scale)

null<-glm(total.hr~1, cluster.mod, family=poisson(link = log))
summary(null)

c2<-glm(total.hr~building_density+road_density+trail_density+forest_density+season, data=cluster.mod,
       na.action = "na.fail", family = poisson(link = log))
c3<-glm(total.hr~building_density+trail_density+forest_density+season, cluster.mod, 
        family=poisson(link = log))
summary(c2)
sadcluster<-dredge(c2)

hist(cluster.mod$total.hr)
qqnorm(resid(c5))
qqline(resid(c5))

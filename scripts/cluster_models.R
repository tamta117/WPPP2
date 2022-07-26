cluster<-read.csv("data/cluster.csv")
cluster_scale<-scale(cluster[4:10])
cluster.mod<-data.frame("cam"=cluster$Cluster, "total.hr"=cluster$duration.hr, cluster_scale)

null<-lm(total.hr~1, cluster.mod)
c1<-lm(total.hr~building_density, cluster.mod)
c2<-lm(total.hr~trail_density, cluster.mod)
c3<-lm(total.hr~road_density, cluster.mod)
c4<-lm(total.hr~forest_density, cluster.mod)
c5<-lm(total.hr~building_density+building_distance+road_density+road_distance+trail_density+trail_distance+forest_density, cluster.mod,
       na.action = "na.fail")
summary(c5)
dredge(c5)

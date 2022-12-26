library(glmmTMB)
library(lme4)
library(ggeffects)
library(ggplot2)
library(here)

#final nobs model
(m8 <- glmmTMB(nobs~road_density+forest_density+season, 
               zi=~wt_class, 
               family=list(family="truncated_poisson", link="log"), all.mod))
summary(m8)
confint(m8)

#final cluster model
c6<-glm(total.hr~building_density+season, data=cluster.mod,
        na.action = "na.fail", family = poisson(link = log))
summary(c6)
confint(c6)

#ggplot theme
mytheme<-theme_linedraw()+
  theme(axis.text=element_text(size=30),
               axis.title.x=element_blank(),
               #axis.text.x=element_blank(),
               #axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.ticks.y=element_blank(),
               legend.title=element_blank(),
               panel.border=element_blank(),
               axis.line=element_line(),
               legend.position="none")

#make function for label breaks
find_breaks<-function(name){
library(dplyr)
  min.x <- round(min(name),dig=0)
max.x <- round(max(name),dig=0)
int.length <- (max.x-min.x)/5
x.axis.label <- seq(from=min.x, to=max.x, by=int.length)
x.axis.breaks <- (x.axis.label - mean(name))/
  sd(name)
axis.bind<-bind_cols(x.axis.label, x.axis.breaks)
colnames(axis.bind)<-c('axis.label','axis.breaks')
return(axis.bind)
}

#graph nobs - road density
scale_road<-find_breaks(all$road_density)
road<-ggpredict(m8, terms=c("road_density","season"))
ggplot(road, aes(x, predicted)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high, color=group))+
  scale_x_continuous(breaks = scale_road$axis.breaks, 
                     labels = as.character(scale_road$axis.label),
                     expand=c(0,0), limits=c(-2,2.5))+
  scale_y_continuous(expand=c(0,1), limits=c(0,50))+
  scale_color_manual(values = c("#4b2e83","#85754d"),
                     name = "Season", labels=c("Winter","Summer"))+
  scale_fill_manual(values = c("#4b2e83","#85754d"),
                    name = "Season", labels=c("Winter","Summer"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  mytheme
ggsave(here("figures/nobs_road_season.png"), width=14, height=8.3, units="in", dpi=300)

#graph nobs - forest density
scale_forest<-find_breaks(all$forest_density)
forest<-ggpredict(m8, terms=c("forest_density","season"))
ggplot(forest, aes(x, predicted)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.15)+
  geom_smooth(aes(color=group))+
  scale_x_continuous(breaks = scale_forest$axis.breaks, 
                     labels = scale_forest$axis.label,
                     expand = c(0,0), limits=c(-1.40001,NA))+
  scale_y_continuous(expand = c(0,1), limits=c(0,30))+
  scale_color_manual(values = c("#4b2e83","#85754d"),
                     name = "Season", labels=c("Winter","Summer"))+
  scale_fill_manual(values = c("#4b2e83","#85754d"),
                    name = "Season", labels=c("Winter","Summer"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  mytheme
ggsave(here("figures/nobs_forest_season.png"), width=14, height=8.3, units="in", dpi=300)


#graph cluster - building density
scale_building<-find_breaks(cluster$building_density)
building<-ggpredict(c6, terms=c("building_density","season"))
ggplot(building, aes(x, predicted)) + 
  geom_ribbon(data=building,aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.15)+
  geom_smooth(data=building,aes(ymin=conf.low, ymax=conf.high, color=group))+
  scale_x_continuous(breaks = scale_building$axis.breaks, labels = scale_building$axis.label,
                     expand=c(0,0), limits=c(0,12))+
  scale_y_continuous(expand=c(0,1), limits=c(NA,90))+
  scale_color_manual(values = c("#85754d","#4b2e83"), 
                    name = "Season", labels=c("Summer","Winter"))+
  scale_fill_manual(values = c("#85754d","#4b2e83"), 
                     name = "Season", labels=c("Summer","Winter"))+
  mytheme+
  guides(color=guide_legend(override.aes=list(fill=NA)))
ggsave(here("figures/cluster_building_season.png"), width=14, height=8.3, units="in", dpi=300)

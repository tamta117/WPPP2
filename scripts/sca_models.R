#plots for scavengers vs cougar nobs

library(ggplot2)

#raven vs cougars
all_sca<-read.csv("data/all_sca_1226.csv")%>%
  mutate(season=ifelse(season=="Spring" | season=="Winter", "winter", "summer"),
         wt_class=ifelse(wt_class=="medium", "large", wt_class))
ggplot(all_sca)+
  geom_jitter(aes(x=COUG, y=RAVE))
ggplot(all_sca)+
  geom_jitter(aes(x=COUG, y=MAGP))

#all scavengers vs cougars
test<-all_sca%>%
  pivot_longer(cols=c("MAGP", "EAGL", "RAVE", "VULT", "COWS", "BEAR", "BOBC", "COYO", "WOLF"),
               names_to="scavengers",
               values_to="count")%>%
  filter(count!=0)
ggplot(test)+
  geom_bar(aes(x=COUG, y=count, fill=scavengers),
           stat = "identity", position = position_dodge(0.9))
ggplot(test)+
  geom_boxplot(aes(x=COUG, y=count, color=scavengers))+
  ylim(0,50)

####prelim models####
library(glmmTMB)
library(MuMIn)

#null models
(null <- glmmTMB(COUG~1, 
                 zi=~1, 
                 family=list(family="truncated_poisson", link="log"), all_sca))

#univariate models
(m1 <- glmmTMB(COUG~MAGP, 
                 zi=~MAGP, 
                 family=list(family="truncated_poisson", link="log"), all_sca))
summary(m1)
(m2 <- glmmTMB(COUG~EAGL, 
               zi=~EAGL, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m2)
(m3 <- glmmTMB(COUG~RAVE, 
               zi=~RAVE, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m3)
(m4 <- glmmTMB(COUG~VULT, 
               zi=~VULT, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m4)
(m5 <- glmmTMB(COUG~COWS, 
               zi=~COWS, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m5)
(m6 <- glmmTMB(COUG~BEAR, 
               zi=~BEAR, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m6)
(m7 <- glmmTMB(COUG~BOBC, 
               zi=~BOBC, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m7)
(m8 <- glmmTMB(COUG~COYO, 
               zi=~COYO, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m8)
(m9 <- glmmTMB(COUG~WOLF, 
               zi=~WOLF, 
               family=list(family="truncated_poisson", link="log"), all_sca))
summary(m9)
#significance: ravens, bears, bobcats, coyotes
#no significance for zero inflational component

#dredge function
library(MuMIn)
m10<-glmmTMB(COUG~RAVE + BEAR + BOBC + COYO,
             zi=~wt_class, 
             family=list(family="truncated_poisson", link="log"), all_sca)
summary(m10)
sadmod<-dredge(m10)
#all 4 covariates = lowest AIC
#wt_class has better AIC than season

####graph models####
library(ggeffects)
RAVE<-ggpredict(m10, terms = "RAVE")
RAVE = RAVE[-19,]
ggplot(RAVE, aes(x, predicted))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high))+
  xlab("Raven")+
  ylab("Cougar")+
  stat_regline_equation(label.y = 20, aes(label = ..rr.label..), size=3)

BEAR<-ggpredict(m10, terms = "BEAR")
ggplot(BEAR, aes(x, predicted))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high))+
  xlab("Bear")+
  ylab("Cougar")+
  stat_regline_equation(label.y = 3, aes(label = ..rr.label..), size=3)

BOBC<-ggpredict(m10, terms = "BOBC")
ggplot(BOBC, aes(x, predicted))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high))+
  xlab("Bobcat")+
  ylab("Cougar")+
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..), size=3)

COYO<-ggpredict(m10, terms = "COYO")
ggplot(COYO, aes(x, predicted))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high))+
  xlab("Coyote")+
  ylab("Cougar")+
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..), size=3)

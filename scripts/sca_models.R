#plots for scavengers vs cougar nobs

library(ggplot2)
library(dplyr)

#scavengers vs cougars
all_sca<-read.csv("data/all_sca_0106.csv")%>%
  mutate(season=ifelse(season=="Spring" | season=="Winter", "winter", "summer"),
         wt_class=ifelse(wt_class=="medium", "large", wt_class))%>%
  filter(COUG != 0)%>%
  filter(COUG < 160)
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=RAVE))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=MAGP))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=EAGL))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=VULT))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=COWS))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=BEAR))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=COYO))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=WOLF))
ggplot(all_sca)+
  geom_point(aes(x=COUG, y=BOBC))
#cows are 0 --> no test
#wolf and bobcat each have 1 data point that's not 0 --> no test

####prelim models####
library(MuMIn)

#null models
(null<-glm(COUG~1, family=poisson(link = log), all_sca))

#univariate models
summary((glm(COUG~MAGP, family=poisson(link = log), all_sca)))
summary((glm(COUG~EAGL, family=poisson(link = log), all_sca)))
summary((glm(COUG~RAVE, family=poisson(link = log), all_sca)))
summary((glm(COUG~VULT, family=poisson(link = log), all_sca)))
summary((glm(COUG~BEAR, family=poisson(link = log), all_sca)))
summary((glm(COUG~COYO, family=poisson(link = log), all_sca)))

#low significance: magpie
#high significance: raven, bear
#no significance: eagle, vulture, cows, coyote

#dredge function
library(MuMIn)
full.mod<-glm(COUG~MAGP+RAVE+BEAR, family=poisson(link = log), all_sca, na.action = "na.fail")
sadmod<-dredge(full.mod)

#best AIC is bear, magpie, raven
#2nd best AIC with lower df is bear and raven - AIC diff of 31.66
summary(glm(COUG~MAGP+RAVE+BEAR, family=poisson(link = log), all_sca))

####graph models####
library(ggeffects)
MAGP<-ggpredict(full.mod, terms = "MAGP")
ggplot(MAGP, aes(x, predicted))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high))+
  xlab("Magpie")+
  ylab("Cougar")

RAVE<-ggpredict(full.mod, terms = "RAVE")
ggplot(RAVE, aes(x, predicted))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high))+
  xlab("Raven")+
  ylab("Cougar")

BEAR<-ggpredict(full.mod, terms = "BEAR")
ggplot(BEAR, aes(x, predicted))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15)+
  geom_smooth(aes(ymin=conf.low, ymax=conf.high))+
  xlab("Bear")+
  ylab("Cougar")

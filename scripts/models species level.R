
source("scripts/traits.R")

library(tidyverse)
library(lme4)
library(segmented)

subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))
dat<-subsetgeneraldata %>% mutate(sr=paste(site, sampling_round))
dat<-dat %>% rename(tl=tongue_length.tongue)

datbee <- dat %>% group_by(bee, site, sampling_round, tl) %>% summarize(mfd=mean(depth), ABUND=n(), vari=sd(depth)) 
datflower <- dat %>% group_by(plant_gs, site, sampling_round, depth) %>% summarize(mtl=mean(tl), ABUND=n(), vari=sd(tl))

#mixed model with random slope and intercept for site and separately for round, flower depth as mean per bee-site-round combo
m1<-lmer(mfd~tl+(tl|site)+(tl|sampling_round), data=datbee)
summary(m1)

datbee %>% ggplot(aes(tl,mfd))+
  geom_point(aes( alpha=0.3))+
  theme_classic()+
  xlim(c(0,14))+
  ylim(c(0,20))+
  geom_abline(aes(intercept=3.01999, slope=0.19072))+
  labs(x="tongue length for bee species", y="average floral depth for flowers visited")


m2<-lmer(vari~tl+(tl|site)+(tl|sampling_round), data=datbee)
summary(m2)

datbee %>% ggplot(aes(tl,vari))+
  geom_point(aes( alpha=0.3))+
  theme_classic()+
  xlim(c(0,14))+
  ylim(c(0,20))+
  geom_abline(aes(intercept=1.91489, slope=0.08220))+
  labs(x="tongue length for bee species", y="SD floral depth for flowers visited")




### plot means and sd of tongues of visitors of each flower species, weighted by abundance

m3<-lmer(mtl~depth+(depth|site)+(depth|sampling_round), data=datflower)
summary(m3)

datflower %>% ggplot(aes(depth,mtl))+
  geom_point(aes(alpha=0.3))+
  theme_classic()+
  geom_abline(aes(intercept=3.01999, slope=0.19072))+
  labs(x="flower depth", y="average tongue length of bees visiting")

m4<-lmer(vari~depth+(depth|site)+(depth|sampling_round), data=datflower)
summary(m4)


datflower %>% ggplot(aes(depth,vari))+
  geom_point(aes(alpha=0.3))+
  theme_classic()+
  geom_abline(aes(intercept=1.43284, slope=0.11679))+
  labs(x="flower depth", y="SD tongue length of bees visiting")



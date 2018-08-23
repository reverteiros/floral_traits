source("scripts/script_traits.R")
library(tidyverse)
library(lme4)
library(segmented)

subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))
dat<-subsetgeneraldata %>% mutate(sr=paste(site, sampling_round))
dat<-dat %>% rename(tl=tongue_length.tongue)

#with flower depth summarized at bee, site, round
dat2<- dat %>% group_by(bee, site, sampling_round, tl) %>% summarize(mfd=mean(depth))
#summarized at bee species, combining visits from all site-rounds
dat3<-dat %>% group_by(bee, tl) %>% summarize(gmfd=mean(depth))
#D.F. With all values for weighting, not used. 
dat4<-left_join(dat, left_join(dat2, dat3))

#mixed model with random slope and intercept for site and separately for round, flower depth as mean per bee-site-round combo
m1<-lmer(mfd~tl+(tl|site)+(tl|sampling_round), data=dat2)
#mixed model where flower depth on per-interaction basis
m2<-lmer(depth~tl+(tl|site)+(tl|sampling_round), data=dat4)
#fixed effects only, with flower dpeth as mean of all flowers visited by bee sp.
m3<-lm(gmfd~tl, data=dat3)
#fixed effects only, summarized at bee-site-round
m4<-lm(mfd~tl, data=dat2)
#fixed effects only, per-interaction
m5<-lm(depth~tl, data=dat4)

segm<-segmented(m4)
seg2<-segmented(m3)
segm
AIC(segm)
AIC(m4)
AIC(m1)
#random effects win

AIC(m2)
AIC(m5)
#random effects win

summary(m2)
summary(m5)
#estimates differ

summary(m4)
summary(m1)
#estimates similar but LMM std errors on fixed effects greater




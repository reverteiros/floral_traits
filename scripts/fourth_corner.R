##### try glm/ 4th corner to think more about whether trait matching matters. Using difference and is.forbidden rather than more typical trait-by-environment matching
# for more thoughts see TerBraak; Jamil; Brown et al. 2014
library(tidyverse)
library(lme4)
#DHARMa makes testing assumptions easier with GLMMs (nice graphical tools)
library(DHARMa)
source("scripts/traits.R")

#generate table with realized and unrealized sp-sp interactions with their trait matching
XYZ<-generaldata  %>% right_join(generaldata %>% complete(nesting(bee, tongue_length.tongue, IT_improved), nesting(plant_gs, depth, width))) %>%  
  group_by(diff=(tongue_length.tongue-depth)^2,  toodeep=tongue_length.tongue-depth<0, forbidden=toodeep*(as.numeric((IT_improved-width)>0)), bee, plant_gs, site, sampling_round) %>% 
  summarize(y=n_distinct(uniqueID)-1)
#add species abundances
XYZ<-XYZ %>% left_join(XYZ %>% group_by(plant_gs) %>% summarize(plantab=sum(y))) %>% left_join(XYZ %>% group_by(bee) %>% summarize(beeab=sum(y)))
#add observation (i.e. an interaction with any frequency between two species) level random effect
XYZ$olre<-as.factor(1:length(XYZ$y))
#generate a binary response too
XYZ$bi<-XYZ$y>0
# head(XYZ)
#histogram of distribution of frequencies. 
# XYZ %>% ggplot(aes(y))+geom_histogram()

#this is a simple poisson model with observation level random effect. it totally overfits the OLRE
m1<-glmer(y~scale(diff)+(1|bee)+(1|plant_gs)+(1|olre), family="poisson", data=XYZ)

#this is a negative binomial with pretty much all the predictors (many random)
m3<-glmer.nb(y~scale(diff)*forbidden+scale(plantab)+scale(beeab)+(1|site)+(1|bee)+(1|plant_gs)+(1|sampling_round),data=XYZ)

#this model officially stinks, but I think it's basically the Brown et al. model
m2<-glm.nb(y~scale(diff)*forbidden+scale(plantab)+scale(beeab)+site*sampling_round,data=XYZ)

#This is a negative binomial model with zero inflation. No idea what that really means BUT
built<-glmmTMB(y~scale(diff)*as.factor(forbidden)+scale(plantab)+scale(beeab)+(1|site)+(1|bee)+(1|plant_gs)+(1|sampling_round),ziformula=~1,family="nbinom2",data=XYZ)
#this has one of the better distributions of residuals

#This model has fewer weird assumptions but only predicts presence. It is therefore dissatisfying. 
builtbi<-glmer(bi~scale(diff)*as.factor(forbidden)+scale(plantab)+scale(beeab)+(1|site)+(1|sampling_round),family="binomial",data=XYZ)

#fewer random effects?
nosp<-glmer.nb(y~scale(diff)*forbidden+scale(plantab)+scale(beeab)+(1|site)+(1|sampling_round),data=XYZ)
noround<-glmer.nb(y~scale(diff)*forbidden+scale(plantab)+scale(beeab)+(1|site)+(1|bee)+(1|plant_gs),data=XYZ)
nosr<-glmer.nb(y~scale(diff)*forbidden+scale(plantab)+scale(beeab)+(1|bee)+(1|plant_gs),data=XYZ)

#use a variable to say which model to look at below, include check assumptions and see fit
a<-nosr

#use DHARMa to generate simulated residuals for things like QQ plot
srs<-simulateResiduals(a)


plot(srs)

#Look at predictions verses observed (note stuff needs to be different for binary data)
plot(XYZ$y, exp(predict(a, newdata=XYZ, allow.new.levels=T)))

#model summaries with stats
summary(built)
summary(builtbi)
summary(m3)
summary(noround)

anova(nosp, m3, noround, nosr)

#takehome: trait matching ain't worth much! Also, NONE of these models predicts anywhere near the observed variance, which must rely on something like extremely strong preferences unrelated to tongue-corolla.

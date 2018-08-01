library(nlme)
library(lme4)
?lme4

measvisits<-read.csv('data/measvisits.csv')
measvisits$round<-as.factor(measvisits$round)
measvisits$year<-as.factor(measvisits$round)

test<-lm(meantongue ~ depth + year + site + round, data=measvisits)
summary(test)

test_glmer<-glmer(meantongue ~ depth +(1|year) +(1|site) + (1|round), data=measvisits)

summary(test_glmer)
plot(measvisits$width, measvisits$meanIT)

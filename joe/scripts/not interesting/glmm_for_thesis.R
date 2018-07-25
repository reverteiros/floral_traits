# GLMM REGRESSION RANDOM SLOPES ONLY
require(lme4)
###########
joes_data<-read.csv("data/measvisits.csv")
#drop the unused columns to remove NA
joes_data<-joes_data[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
joes_data$year<-factor(joes_data$year)
joes_data$round<-factor(joes_data$round)
joes_data<-joes_data[complete.cases(joes_data),]

##glmm for non-log transformed data
fm1<-lmer(meantongue ~ depth + round + year + (1 | site), joes_data)
##get errors and fitted
errs1<-resid(fm1)
fit1<-fitted(fm1)
plot(jitter(joes_data$depth), errs1)
plot(joes_data$round, errs1)
plot(joes_data$year, errs1)
#Check for heterscedasticity:
plot(joes_data$meantongue, errs1)
plot(fit1, errs1)

##glmm for log transformed data
fm2<-lmer(meantongue ~ log(depth+1) + round + year + (1 | site), joes_data)
##get errors and fitted
errs2<-resid(fm2)
fit2<-fitted(fm2)
plot(jitter(joes_data$depth), errs2)
plot(joes_data$round, errs2)
plot(joes_data$year, errs2)
#Check for heterscedasticity:
plot(joes_data$meantongue, errs2)
plot(fit2, errs2)

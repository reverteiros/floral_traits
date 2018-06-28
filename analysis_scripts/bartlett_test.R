#########################################################
### run quantile regressions for Joe's thesis     #######
#########################################################
######### look out for homogeneity of variance for JZ
require(lattice)

#data

joes_dat<-read.csv("data/measvisits.csv")
#drop the unused columns to remove NA
joes_dat<-joes_dat[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
joes_dat$year<-factor(joes_dat$year)
joes_dat$round<-factor(joes_dat$round)
joes_dat<-joes_dat[complete.cases(joes_dat),]

#do a bartlett test for homogeneity of variance
nosite<-bartlett.test(formula=meantongue~depth,data=joes_dat)

#dunno about that bartlett stuff. just plot residuals!
errs<-resid(lm(meantongue~depth+year+round, data=joes_dat))
fit<-fitted(lm(meantongue~depth+year+round, data=joes_dat))
plot(jitter(joes_dat$depth), resid(lm(meantongue~depth+year+round, data=joes_dat)))
plot(joes_dat$round, resid(lm(meantongue~depth+year+round, data=joes_dat)))
plot(joes_dat$year, resid(lm(meantongue~depth+year+round, data=joes_dat)))
#Check for heterscedasticity:
plot(joes_dat$meantongue, errs)
plot(fit, errs)
# that's what heteroscedasticity looks like, I think!

hist(errs)

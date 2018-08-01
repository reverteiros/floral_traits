
library(lqmm)

subsetgeneraldata <- as.data.frame(subsetgeneraldata)
subsetgeneraldata<-subsetgeneraldata[complete.cases(subsetgeneraldata),]

subsetgeneraldata$sampling_round <- as.character(subsetgeneraldata$sampling_round)

two<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.2), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
four<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.4), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=100000))
six<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.6), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=100000))
eight<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.8), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=100000))
nine<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.9), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=100000))

summary(two)

coef.lqmm(two)

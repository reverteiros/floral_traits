## In this script, we are making fake data sets to test our modelss
require(plyr)
require(reshape)
require(lqmm)
## define what the vectors are going to look like
a<-c(1:20)
slps<-(0:9/10)

meanf<- function (x){
  out=rnorm(20, mean=slps[x]*a, sd = 1)
  return(out)
}

newdata<-ldply(1:10, meanf)
newdata<-melt(newdata)
#assign random sites to data
newdata$site<-sample(1:10, 200, replace=T)

plot(as.numeric(newdata$variable), newdata$value)
newdata$variable<-as.numeric(newdata$variable)
newdata$site<-as.factor(newdata$site)
######################################################################
test5<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.05, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test5)

test10<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.10, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test10)
test09<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.09, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test09)
test095<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.095, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test095)
test092<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.092, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test092)
test093<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.093, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test093)


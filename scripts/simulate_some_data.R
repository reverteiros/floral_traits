#make up some fake data
library(tidyverse)
#first case: linear regression
?rnorm
tongues<-c(rnorm(10000,1,1), rnorm(10000,7,1))
tongues<-tongues[tongues>0]
hist(tongues)

depth_linear<-tongues+rnorm(length(tongues), sd=2)
depth_linear[depth_linear<0]<-0
hist(depth_linear)

plot(depth_linear, tongues)
summary(lm(tongues~depth_linear))

#second case: flowers deeper

dl1<-unlist(lapply(1:length(tongues), function(x){runif(1, 0,tongues[x])}))
hist(dl1)
plot(dl1, tongues)                  
summary(lm(tongues~dl1))


dr<-unlist(lapply(rbind(rnorm(10000,2,1),rnorm(10000,6,3)), function(x){ifelse(x<0,0,x)}))
hist(dr)

plot(dr[1:length(tongues)], tongues)
summary(lm(tongues~dr[1:length(tongues)]))

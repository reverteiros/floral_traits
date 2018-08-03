
library(ggplot2)
library(quantreg)

source("scripts/script_traits.R")

subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))
subsetgeneraldata$sampling_round <- as.character(subsetgeneraldata$sampling_round)

# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends
subsetgeneraldata<-subsetgeneraldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72))
subsetgeneraldata<-subsetgeneraldata %>% mutate(beewider=if_else(IT_improved>width, "true", "false"))
subsetgeneraldata<-subsetgeneraldata %>% mutate(depthchanged=if_else(beewider=="true", depth,0))

# work with more than one quantile at a time
qs <- c(0.2,0.4,0.6,0.8,0.9)
qr1 <- rq(subsetgeneraldata$tongue_length.tongue ~ subsetgeneraldata$depth+subsetgeneraldata$site+subsetgeneraldata$sampling_round, data=subsetgeneraldata,  tau = qs)
coef(qr1)
summary(qr1)
ggplot(subsetgeneraldata, aes(depth,tongue_length.tongue)) + geom_point() + geom_quantile(quantiles = qs) 
# plot the ratio of slope change between quantiles. If they fall outside the red area, it means that they are far from the confidence intervals of the least minimum square and you need to perform quantile regression because a lineal regression does not fit.
plot(summary(qr1), parm="subsetgeneraldata$depth",ylim=c(0,0.6))



### The same as before but dropping the bees that are smaller than the flowers and they crawl in
subsetgeneraldatawiderbees <- droplevels(dplyr::filter(subsetgeneraldata, beewider=="true"))

qr2 <- rq(subsetgeneraldatawiderbees$tongue_length.tongue ~ subsetgeneraldatawiderbees$depth+subsetgeneraldatawiderbees$site+subsetgeneraldatawiderbees$sampling_round, data=subsetgeneraldatawiderbees,  tau = qs)
coef(qr2)
summary(qr2)

ggplot(subsetgeneraldatawiderbees, aes(depth,tongue_length.tongue)) + geom_point() + geom_quantile(quantiles = qs) 
plot(summary(qr2), parm="subsetgeneraldatawiderbees$depth",ylim=c(0,0.6))


### The same as before but assigning depth of 0 to the interactions occurring with the bees that are smaller than the flowers and they crawl in
qr3 <- rq(subsetgeneraldata$tongue_length.tongue ~ subsetgeneraldata$depthchanged+subsetgeneraldata$site+subsetgeneraldata$sampling_round, data=subsetgeneraldata,  tau = qs)
coef(qr3)
summary(qr3)

ggplot(subsetgeneraldata, aes(depthchanged,tongue_length.tongue)) + geom_point() + geom_quantile(quantiles = qs) 
plot(summary(qr3), parm="subsetgeneraldata$depthchanged",ylim=c(0,0.6))

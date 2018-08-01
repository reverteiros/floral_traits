
library(ggplot2)
library(quantreg)

source("scripts/script plots.R")

# graph a lineal model of the data to see the fit
ggplot(subsetgeneraldata, aes(depth,tongue_length.tongue)) + geom_point() + geom_smooth(method="lm")

subsetgeneraldata$sampling_round <- as.character(subsetgeneraldata$sampling_round)

# work with more than one quantile at a time
qs <- 1:4/5
qr2 <- rq(subsetgeneraldata$tongue_length.tongue ~ subsetgeneraldata$depth+subsetgeneraldata$site+subsetgeneraldata$sampling_round, data=subsetgeneraldata,  tau = qs)
coef(qr2)
summary(qr2)
ggplot(subsetgeneraldata, aes(depth,tongue_length.tongue)) + geom_point() + geom_quantile(quantiles = qs) 
# plot the ratio of slope change between quantiles. If they fall outside the red area, it means that they are far from the confidence intervals of the least minimum square and you need to perform quantile regression because a lineal regression does not fit.
plot(summary(qr2), parm="subsetgeneraldata$depth")



### The same as before but dropping the bees that are smaller than the flowers and they crawl in
subsetgeneraldatawiderbees <- droplevels(dplyr::filter(subsetgeneraldata, beewider=="true"))

# graph a lineal model of the data to see the fit
ggplot(subsetgeneraldatawiderbees, aes(depth,tongue_length.tongue)) + geom_point() + geom_smooth(method="lm")

# work with more than one quantile at a time
qs <- 1:4/5
qr2 <- rq(subsetgeneraldatawiderbees$tongue_length.tongue ~ subsetgeneraldatawiderbees$depth, data=subsetgeneraldatawiderbees,  tau = qs)
coef(qr2)
summary(qr2)
ggplot(subsetgeneraldatawiderbees, aes(depth,tongue_length.tongue)) + geom_point() + geom_quantile(quantiles = qs) 
# plot the ratio of slope change between quantiles. If they fall outside the red area, it means that they are far from the confidence intervals of the least minimum square and you need to perform quantile regression because a lineal regression does not fit.
plot(summary(qr2), parm="subsetgeneraldatawiderbees$depth")

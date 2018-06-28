
library(ggplot2)
library(quantreg)

source("scripts/script plots.R")

qr1 <- rq(subsetgeneraldata$tongue_length.tongue ~ subsetgeneraldata$depth, data=subsetgeneraldata, tau = 0.9)
ggplot(subsetgeneraldata, aes(depth,tongue_length.tongue)) + geom_point() + geom_smooth(method="lm")
summary(qr1)
ggplot(subsetgeneraldata, aes(depth,tongue_length.tongue)) + geom_point() + 
  geom_abline(intercept=coef(qr1)[1], slope=coef(qr1)[2])
qs <- 1:9/10
qr2 <- rq(subsetgeneraldata$tongue_length.tongue ~ subsetgeneraldata$depth, data=subsetgeneraldata,  tau = qs)
coef(qr2)
ggplot(subsetgeneraldata, aes(depth,tongue_length.tongue)) + geom_point() + geom_quantile(quantiles = qs)
plot(summary(qr2), parm="x")

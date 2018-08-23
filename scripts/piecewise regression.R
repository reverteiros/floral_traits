
source("scripts/script_traits.R")

library(ggplot2)
library(MASS)
library(segmented)

subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))
subsetgeneraldata$difference <- subsetgeneraldata$tongue_length.tongue-subsetgeneraldata$depth
subsetgeneraldata$sr <- paste(subsetgeneraldata$site, subsetgeneraldata$sampling_round)

datbees <- subsetgeneraldata %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(mfd=mean(depth), ABUND=n()) 

# Weighted_fit <- rlm(mfd ~ tongue_length.tongue, data = datbees, weights = ABUND)
# summary(Weighted_fit)
nonWeighted_fit <- lm(mfd ~ tongue_length.tongue, data = datbees)
summary(nonWeighted_fit)
overall_fit <- lm(depth ~ tongue_length.tongue, data = subsetgeneraldata)
summary(overall_fit)

plot(mfd ~ tongue_length.tongue, data = datbees)
# abline(a=1.6469, b=0.5918,col="blue")#weighted
abline(a=2.4173, b=0.5800,col="red")#non-weigthed
abline(a=2.14923, b=0.63106,col="green")#allpoints


datflowers <- subsetgeneraldata%>% group_by(plant_gs, sr, depth) %>% summarize(mtl=mean(tongue_length.tongue), ABUND=n()) 

# Weighted_fit <- rlm(mtl ~ depth, data = datflowers, weights = ABUND)
# summary(Weighted_fit)
nonWeighted_fit <- lm(mtl ~ depth, data = datflowers)
summary(nonWeighted_fit)
overall_fit2 <- lm(tongue_length.tongue ~ depth, data = subsetgeneraldata)
summary(overall_fit2)

plot(mtl ~ depth, data = datflowers)
# abline(a=2.2302, b=0.2606,col="blue")#weighted
abline(a=3.1486, b=0.1612,col="red")#non-weigthed
abline(a=2.8578, b=0.2452,col="green")#allpoints



##### Piecewise regression. Pedestrian version. Discontinuous segments, you can choose the breakpoints

## bees and mean depth
piecewise2 <- lm(mfd ~ tongue_length.tongue*(tongue_length.tongue < 7) + tongue_length.tongue*(tongue_length.tongue > 7),dat=datbees)
summary(piecewise2)

plot(mfd~tongue_length.tongue, data=datbees,pch=16)
curve((12.7615 -10.0410) + (-0.4383+0.8679)*x, add=T, from=0, to=7)
curve((12.7615) -0.4383*x, add=T, from=7, to=max(x))
abline(v=7, lty=3)

## flowers and mean tongue
piecewise2 <- lm(mtl ~ depth*(depth < 12) + depth*(depth > 12),dat=datflowers)
summary(piecewise2)

plot(mtl~depth, data=datflowers,pch=16)
curve((5.17221 -2.40293) + (0.03202+0.28318)*x, add=T, from=0, to=12)
curve((5.17221) + 0.03202*x, add=T, from=12, to=31)
abline(v=12, lty=3)

##### Piecewise regression. Automatic version. R decides breakpoints. Continuous segments

lin.mod <- lm(mfd ~ tongue_length.tongue,dat=datbees)
segmented.mod <- segmented(lin.mod, seg.Z = ~tongue_length.tongue, psi=3)
segmented.mod

plot(mfd~tongue_length.tongue, pch=16,dat=datbees)
plot(segmented.mod, add=T)


lin.mod <- lm(mtl ~ depth,dat=datflowers)
segmented.mod <- segmented(lin.mod, seg.Z = ~depth, psi=1)
segmented.mod

plot(mtl~depth, pch=16,dat=datflowers)
abline(lin.mod)
plot(segmented.mod, add=T)







datbees <- subsetgeneraldata %>% group_by(bee, tongue_length.tongue) %>% summarize(mfd=mean(depth), ABUND=n()) 
datbeessr <- subsetgeneraldata %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(mfd=mean(depth), ABUND=n()) 

lin.mod <- lm(mfd ~ tongue_length.tongue,dat=datbees)
summary(lin.mod)
segmented.mod <- segmented(lin.mod, seg.Z = ~tongue_length.tongue)
summary(segmented.mod)

 lin.mod2 <- lm(mfd ~ tongue_length.tongue,dat=datbeessr)
summary(lin.mod2)
segmented.mod2 <- segmented(lin.mod2, seg.Z = ~tongue_length.tongue, psi=8)
summary.segmented(segmented.mod2)

AIC(segmented.mod)
AIC(lin.mod)

AIC(segmented.mod2)
AIC(lin.mod2)


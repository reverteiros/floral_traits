
source("scripts/traits.R")

library(ggplot2)
library(ggExtra)

## Define subsets of variables with appropriate names

# Work only with the plants from which we have data on traits
subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))

# New variables
subsetgeneraldata$difference <- subsetgeneraldata$tongue_length.tongue-subsetgeneraldata$depth
subsetgeneraldata$sr <- subsetgeneraldata %>% mutate(sr=paste(site, sampling_round))
# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends
subsetgeneraldata<-subsetgeneraldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72))
subsetgeneraldata<-subsetgeneraldata %>% mutate(beewider=if_else(IT_improved>width, "true", "false"))


## data for histograms with species
histbeespecies<-subsetgeneraldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue), integertongue=mean(as.integer(tongue_length.tongue)),IT=mean(IT_improved),abundance=n())
histplantspecies<-subsetgeneraldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth), width=mean(width), integerdepth=mean(as.integer(depth)),abundance=n())


############ plots
# rank-abundance plot of bee species
barplot(sort(histbeespecies$abundance,decreasing=T))

#all bees tongue length histogram
hist(subsetgeneraldata$tongue_length.tongue, xlab="Tongue length (mm)",main="",breaks=seq(0, 14, by = 1))
# #all bees improved IT histogram
# hist(subsetgeneraldata$IT_improved, xlab="IT (mm)",main="")
#bee species tongue length histogram
hist(histbeespecies$tongue, xlab="Tongue length (mm)",main="",breaks=seq(0, 14, by = 1))
# #bee species IT histogram
# hist(histbeespecies$IT, xlab="IT (mm)",main="")
#all visits corolla depth histogram
hist(subsetgeneraldata$depth, xlab="Flower depth (mm)",main="",breaks=seq(0, 31, by = 1))
# #all visits corolla width histogram
# hist(subsetgeneraldata$width, xlab="Flower width (mm)",main="")
#flower species corolla depth histogram
hist(histplantspecies$depth, xlab="Flower depth (mm)",main="",breaks=seq(0, 31, by = 1))
# #flower species corolla width histogram
# hist(histplantspecies$width, xlab="Flower width (mm)",main="")
#all bees difference between depth and tongue
hist(subsetgeneraldata$difference, xlab="Tongue length - flower depth (mm)",main="",breaks=seq(-13, 32, by = 1))
#this looks impressively balanced around 0! What is null i.e. totally random visits? 


#### Plot quantile regresssion
ggplot(subsetgeneraldata, aes(y=tongue_length.tongue, x=depth)) + 
  geom_jitter(alpha=0.1, height=0.1) + 
  theme_classic() 

#### Density plots of flowers with quantiles
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
dens <- density(subsetgeneraldata$depth)
df <- data.frame(x=dens$x, y=dens$y)
quantiles <- quantile(subsetgeneraldata$depth, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")

#### Density plots of bees with quantiles
subsetgeneraldata2 <- subsetgeneraldata[!is.na(subsetgeneraldata$tongue_length.tongue),]

probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
dens <- density(subsetgeneraldata2$tongue_length.tongue)
df <- data.frame(x=dens$x, y=dens$y)
quantiles <- quantile(subsetgeneraldata2$tongue_length.tongue, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles)) 
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")


## separating longue-tongued bees and short-tongued bees

longuetongues <- dplyr::filter(subsetgeneraldata, tongue_length.tongue > 8)
shorttongues <- dplyr::filter(subsetgeneraldata, tongue_length.tongue < 4)

histshortbeespecies<-shorttongues %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue), integertongue=mean(as.integer(tongue_length.tongue)),IT=mean(IT_improved),abundance=n())

histlongbeespecies<-longuetongues %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue), integertongue=mean(as.integer(tongue_length.tongue)),IT=mean(IT_improved),abundance=n())


### plot means and sd of depths visited by each bee species, weighted by abundance
dat %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(mfd=mean(depth), ABUND=n(), vari=sd(depth)) %>% ggplot(aes(tongue_length.tongue,mfd))+
  geom_point(aes( alpha=0.3))+
  theme_classic()+
  # geom_errorbar(aes(ymin=mfd-1.96*vari, ymax=mfd+1.96*vari))+
  xlim(c(0,14))+
  ylim(c(0,20))+
  geom_smooth()+
  labs(x="tongue length for bee species", y="average floral depth for flowers visited")

### plot means and sd of tongues of visitors of each flower species, weighted by abundance
dat %>% group_by(plant_gs, sr, depth) %>% summarize(mtl=mean(tongue_length.tongue), ABUND=n(), vari=sd(tongue_length.tongue)) %>% ggplot(aes(depth,mtl))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  geom_errorbar(aes(ymin=mtl-1.96*vari, ymax=mtl+1.96*vari))+
  xlim(c(0,33))+
  ylim(c(0,16))+
  geom_smooth()+
  labs(x="flower depth", y="average tongue length of bees visiting")




#### Plot tongue vs depth with histograms at margins
g <- ggplot(subsetgeneraldata, aes(y=depth, x=tongue_length.tongue)) + 
  geom_jitter(alpha=0.1, height=0.1) + 
  theme_classic() +
  labs(y="Flower depth (mm)", x="Bee tongue length (mm)")

ggMarginal(g, type = "histogram", fill="transparent")




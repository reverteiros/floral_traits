
source("scripts/traits.R")

library(ggplot2)
library(ggExtra)


require(ggplot2)
require(reshape2)
mtcars2 = melt(mtcars, id.vars='mpg')

andrenidae <- generaldata %>%
  filter(bee_family == "Andrenidae")

hist(andrenidae$newdifference)

colletidae <- generaldata %>%
  filter(bee_family == "Colletidae")

halictidae <- generaldata %>%
  filter(bee_family == "Halictidae")

apidae <- generaldata %>%
  filter(bee_family == "Apidae")

megachilidae <- generaldata %>%
  filter(bee_family == "Megachilidae")

ggplot(megachilidae) +
  geom_jitter(aes(depth,tongue_length.tongue, alpha = 0.5,colour=bee_family)) + 
  geom_smooth(aes(depth,tongue_length.tongue, colour=bee_family), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Flower depth (mm)", y="Bee tongue length (mm)")




#### Plot tongue vs depth with histograms at margins
g <- ggplot(generaldata, aes(y=depth, x=tongue_length.tongue,color=bee_family)) + 
  geom_jitter(alpha=0.1, height=0.1) + 
  theme_classic() +
  labs(y="Flower depth (mm)", x="Bee tongue length (mm)")

ggMarginal(g, type = "histogram", fill="transparent")



# ## data for histograms with species
# histbeespecies<-generaldata %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue), integertongue=mean(as.integer(tongue_length.tongue)),IT=mean(IT_improved),abundance=n())
# histplantspecies<-generaldata %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth), width=mean(width), integerdepth=mean(as.integer(depth)),abundance=n())


############ plots
# # rank-abundance plot of bee species
# barplot(sort(histbeespecies$abundance,decreasing=T))

#all bees tongue length histogram
hist(generaldata$tongue_length.tongue, xlab="Tongue length (mm)",main="",breaks=seq(0, 14, by = 1))
# #all bees improved IT histogram
# hist(generaldata$IT_improved, xlab="IT (mm)",main="")
#bee species tongue length histogram
hist(histbeespecies$tongue, xlab="Tongue length (mm)",main="",breaks=seq(0, 14, by = 1))
# #bee species IT histogram
# hist(histbeespecies$IT, xlab="IT (mm)",main="")
#all visits corolla depth histogram
hist(generaldata$depth, xlab="Flower depth (mm)",main="",breaks=seq(0, 31, by = 1))
# #all visits corolla width histogram
# hist(generaldata$width, xlab="Flower width (mm)",main="")
#flower species corolla depth histogram
hist(histplantspecies$depth, xlab="Flower depth (mm)",main="",breaks=seq(0, 31, by = 1))
# #flower species corolla width histogram
# hist(histplantspecies$width, xlab="Flower width (mm)",main="")
#all bees difference between depth and tongue
hist(generaldata$difference, xlab="Tongue length - flower depth (mm)",main="",breaks=seq(-13, 32, by = 1))
#this looks impressively balanced around 0! What is null i.e. totally random visits? 



# 
# #### Plot quantile regresssion
# ggplot(generaldata, aes(y=tongue_length.tongue, x=depth)) + 
#   geom_jitter(alpha=0.1, height=0.1) + 
#   theme_classic() 
# 
# #### Density plots of flowers with quantiles
# probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
# dens <- density(generaldata$depth)
# df <- data.frame(x=dens$x, y=dens$y)
# quantiles <- quantile(generaldata$depth, prob=probs)
# df$quant <- factor(findInterval(df$x,quantiles))
# ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")
# 
# #### Density plots of bees with quantiles
# generaldata2 <- generaldata[!is.na(generaldata$tongue_length.tongue),]
# 
# probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
# dens <- density(generaldata2$tongue_length.tongue)
# df <- data.frame(x=dens$x, y=dens$y)
# quantiles <- quantile(generaldata2$tongue_length.tongue, prob=probs)
# df$quant <- factor(findInterval(df$x,quantiles)) 
# ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")
# 
# 
# ## separating longue-tongued bees and short-tongued bees
# 
# longuetongues <- dplyr::filter(generaldata, tongue_length.tongue > 8)
# shorttongues <- dplyr::filter(generaldata, tongue_length.tongue < 4)
# 
# histshortbeespecies<-shorttongues %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue), integertongue=mean(as.integer(tongue_length.tongue)),IT=mean(IT_improved),abundance=n())
# 
# histlongbeespecies<-longuetongues %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue), integertongue=mean(as.integer(tongue_length.tongue)),IT=mean(IT_improved),abundance=n())
# 
# 
# ### plot means and sd of depths visited by each bee species, weighted by abundance
# dat %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(mfd=mean(depth), ABUND=n(), vari=sd(depth)) %>% ggplot(aes(tongue_length.tongue,mfd))+
#   geom_point(aes( alpha=0.3))+
#   theme_classic()+
#   # geom_errorbar(aes(ymin=mfd-1.96*vari, ymax=mfd+1.96*vari))+
#   xlim(c(0,14))+
#   ylim(c(0,20))+
#   geom_smooth()+
#   labs(x="tongue length for bee species", y="average floral depth for flowers visited")
# 
# ### plot means and sd of tongues of visitors of each flower species, weighted by abundance
# dat %>% group_by(plant_gs, sr, depth) %>% summarize(mtl=mean(tongue_length.tongue), ABUND=n(), vari=sd(tongue_length.tongue)) %>% ggplot(aes(depth,mtl))+
#   geom_point(aes(size=ABUND, alpha=0.3))+
#   theme_classic()+
#   geom_errorbar(aes(ymin=mtl-1.96*vari, ymax=mtl+1.96*vari))+
#   xlim(c(0,33))+
#   ylim(c(0,16))+
#   geom_smooth()+
#   labs(x="flower depth", y="average tongue length of bees visiting")



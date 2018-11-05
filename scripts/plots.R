
source("scripts/traits.R")

library(ggplot2)
library(ggExtra)


###################
# MS fig 1
### show all data

#### Plot tongue vs depth with histograms at margins
g <- ggplot(generaldata, aes(depth, tongue_length.tongue)) + 
  geom_jitter(alpha=0.05, height=0.2, width=0.1, size=0.6) + 
  theme_classic() +
  coord_fixed()+
  labs(x="Flower depth (mm)", y="Bee tongue length (mm)")

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
# hist(generaldata$tongue_length.tongue, xlab="Tongue length (mm)",main="",breaks=seq(0, 14, by = 1))
# #all bees improved IT histogram
# hist(generaldata$IT_improved, xlab="IT (mm)",main="")
#bee species tongue length histogram
#This object no longer exists
# hist(histbeespecies$tongue, xlab="Tongue length (mm)",main="",breaks=seq(0, 14, by = 1))

### if we want, distribution of tongue lenghts by species, rather than by individual
generaldata %>% group_by(bee) %>% summarize(tongue=mean(tongue_length.tongue)) %>% ggplot(aes(tongue))+
  geom_histogram(bins=12)+
  theme_classic()+
  labs(x="tongue length (mm)", y="# species")


# #bee species IT histogram
# hist(histbeespecies$IT, xlab="IT (mm)",main="")
#all visits corolla depth histogram
# hist(generaldata$depth, xlab="Flower depth (mm)",main="",breaks=seq(0, 31, by = 1))
# #all visits corolla width histogram
# hist(generaldata$width, xlab="Flower width (mm)",main="")
#flower species corolla depth histogram
### if we want, distribution of tongue lenghts by species, rather than by individual
generaldata %>% group_by(plant_gs) %>% summarize(depth=mean(depth)) %>% ggplot(aes(depth))+
  geom_histogram(bins=12)+
  theme_classic()+
  labs(x="corolla depth (mm)", y="# species")


# #flower species corolla width histogram

#all bees difference between depth and tongue
hist(generaldata$difference, xlab="tongue length - flower depth (mm)",main="")
#this looks impressively balanced around 0! What is null i.e. totally random visits? 



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



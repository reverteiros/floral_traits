
source("scripts/1. Data cleaning.R")

library(ggplot2)
library(ggExtra)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)


bees_qualitative <- generaldata %>% 
  mutate(N = 1) %>%
  group_by(bee,plant_gs) %>% 
  summarize(tongue=mean(tongue_length.tongue),depth=mean(depth),abundance=sum(N)) 



#### Plot tongue vs depth with histograms at margins
g <- ggplot(bees_qualitative, aes(y=tongue, x=depth)) + 
  geom_jitter(alpha=0.5, height=0.2,aes(size = abundance)) + 
  theme_classic() +
  coord_fixed()+
  labs(x="Flower depth (mm)", y="Bee tongue length (mm)")+
  coord_cartesian(ylim = c(0, 13))+
  geom_abline(aes(intercept=0, slope=1))+
  theme(legend.position = "none") 


ggMarginal(g, type = "histogram", fill="transparent")


ggplot(bees_qualitative, aes(y=tongue, x=depth)) + 
  geom_raster(aes(fill = abundance)) + 
  theme_classic() +
  coord_fixed()+
  labs(x="Flower depth (mm)", y="Bee tongue length (mm)")+
  coord_cartesian(ylim = c(0, 13))+
  geom_abline(aes(intercept=0, slope=1))+
  theme(legend.position = "none") 


geom_raster(aes(fill = z), hjust=0.5,vjust=0.5, interpolate=FALSE)




# #flower species corolla width histogram

#all bees difference between depth and tongue
hist(generaldata$difference, xlab="tongue length - flower depth (mm)",main="")
#this looks impressively balanced around 0! What is null i.e. totally random visits? 




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


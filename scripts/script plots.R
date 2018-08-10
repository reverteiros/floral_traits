
source("scripts/script_traits.R")

library(ggplot2)
library(directlabels)

## Define subsets of variables with appropriate names

# Work only with the plants from which we have data on traits
subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))

# New variable: difference between flower depth and proboscis length
subsetgeneraldata$difference <- subsetgeneraldata$depth-subsetgeneraldata$tongue_length.tongue
# Proboscis is logner than flower depth or not
subsetgeneraldata<-subsetgeneraldata %>% mutate(proboscislonger=if_else(tongue_length.tongue>depth, "true", "false"))
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

# ## data for bubble plots with interaction strength of each plant-pollinator interaction
# interactionstrength<-subsetgeneraldata %>%
#   group_by(bee,plant_gs) %>%
#   summarize(abundance=n(),tongue=mean(tongue_length.tongue),depth=mean(depth),it=mean(IT_improved),width=mean(width))


############ plots
# rank-abundance plot of bee species
barplot(sort(histbeespecies$abundance,decreasing=T))

table(subsetgeneraldata$tongue_length.tongue)

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
hist(subsetgeneraldata$difference, xlab="Flower depth - tongue length (mm)",main="",breaks=seq(-13, 32, by = 1))
#this looks impressively balanced around 0! What is null i.e. totally random visits? 

# # histograms separing per site or sampling round
# # tongue length
# subsetgeneraldata %>%
#   ggplot(aes(x=tongue_length.tongue),alpha=0.5)+
#   geom_histogram(alpha=0.5)+
#   theme_classic()+
#   facet_wrap(~ site,  scales="free")
# 
# subsetgeneraldata %>%
#   ggplot(aes(x=tongue_length.tongue),alpha=0.5)+
#   geom_histogram(alpha=0.5)+
#   theme_classic()+
#   facet_wrap(~ sampling_round,  scales="free")
# # corolla depth
# subsetgeneraldata %>%
#   ggplot(aes(x=depth),alpha=0.5)+
#   geom_histogram(alpha=0.5)+
#   theme_classic()+
#   facet_wrap(~ site,  scales="free")
# 
# subsetgeneraldata %>%
#   ggplot(aes(x=depth),alpha=0.5)+
#   geom_histogram(alpha=0.5)+
#   theme_classic()+
#   facet_wrap(~ sampling_round,  scales="free")
# 
# # observe the variation in flower depth distribution across space and time
# dplyr::filter(subsetgeneraldata, !is.na(sampling_round)) %>%
#   ggplot(aes(x=depth),alpha=0.5)+
#   geom_histogram(alpha=0.5)+
#   facet_wrap(~ sampling_round+site,  scales="free")+
#   theme(strip.placement="none")

# 
# # Barplots (converting tongue length and corolla depth to integer to simplify and make code easier) with each bar is all the flowers/bees with a specific mm. Each colour represents a diferent species, so some bars are the sum of several species with the same trait value
# histbeespecies %>%
#   ggplot(aes(x=integertongue, y=abundance, fill=bee)) +
#   geom_bar(stat="identity")+
#   theme_classic()+
#   theme(legend.position="none")
# 
# histplantspecies %>%
#   ggplot(aes(x=integerdepth, y=abundance, fill=plant_gs)) +
#   geom_bar(stat="identity")+
#   theme_classic()+
#   theme(legend.position="none")


#### Plots comparing proboscis and depth
# dotplot depth and proboscis length separing proboscis longer and shorter than flowers
subsetgeneraldata %>%
  ggplot(aes(x=depth, tongue_length.tongue))+
  geom_jitter(aes(color=proboscislonger),alpha=0.1, height=0.1)+
  theme_classic()
# bubble plots with interaction strength of each plant-pollinator interaction
symbols(interactionstrength$depth, interactionstrength$tongue, circles = sqrt(interactionstrength$abundance/pi), inches = 0.25, fg = "white", bg = "red", ylab=c("Proboscis length (mm)"),xlab=c("Flower depth (mm)"), main = "Sized by Interaction Strength")
abline(a = 0, b = 1)#adding the 1-1 line


#### Plot quantile regresssion. Labels not working don't know why
ggplot(subsetgeneraldata, aes(y=tongue_length.tongue, x=depth)) + 
  geom_jitter(alpha=0.1, height=0.1) + 
  theme_classic() +
  stat_quantile(quantiles=c(0.10),col="blue",size=1) + 
  stat_quantile(quantiles=c(0.25),col="red",size=1) + 
  stat_quantile(quantiles=c(0.50),col="orange",size=1) +
  stat_quantile(quantiles=c(0.75),col="green",size=1) +
  stat_quantile(quantiles=c(0.90),col="brown",size=1) +
  scale_colour_manual(name = 'Quantiles', breaks = c('blue', 'red','orange','green','brown'), values = c('blue', 'red','orange','green','brown'), labels = c('0.1','0.25','0.5','0.75','0.9')) 




# #### Plots comparing IT and width
# # combining the histograms of IT and width
# subsetgeneraldata %>%
#   ggplot(aes(x=IT_improved),alpha=0.5)+
#   geom_histogram(alpha=0.5)+
#   geom_histogram(aes(x=width),alpha=0.5)+
#   theme_classic()#+
# #annotate(geom = "text", x = 3.2, y = 3500, label = "Red = Bee IT")+
# #annotate(geom = "text", x = 3.2, y = 3000, label = "Grey = Corolla width")
# 
# # dotplot width and IT separing bees larger and smaller than flowers
# subsetgeneraldata %>%
#   ggplot(aes(x=width, IT_improved))+
#   geom_point(aes(color=beewider))+
#   theme_classic()
# table(subsetgeneraldata)
# # bubble plots with interaction strength of each plant-pollinator interaction
# symbols(interactionstrength$width, interactionstrength$it, circles = sqrt(interactionstrength$abundance/pi), inches = 0.25, fg = "white", bg = "red", ylab=c("IT (mm)"),xlab=c("Flower width (mm)"), main = "Sized by Interaction Strength")
# abline(a = 0, b = 1)#adding the 1-1 line


# ## same plots than before but separating by sociality degree
# subsetgeneraldata %>%
#   ggplot(aes(x=depth, tongue_length.tongue))+
#   geom_jitter(aes(color=sociality),alpha=0.1, height=0.1)+
#   theme_classic()
# 
# subsetgeneraldata %>%
#   ggplot(aes(x=width, IT_improved))+
#   geom_jitter(aes(color=sociality),alpha=0.1, height=0.1)+
#   theme_classic()
# 
# ## plot differences along time of day
# subsetgeneraldata %>%
#   ggplot(aes(x=midbout, difference))+
#   geom_jitter(aes(color=proboscislonger),alpha=0.1, height=0.1)+
#   geom_smooth(aes(color=proboscislonger))+
#   theme_classic()



source("scripts/script_traits.R")

library(ggplot2)

## Define subsets of variables with appropriate names

# Work only with the plants from which we have data on traits
subsetgeneraldata <- dplyr::filter(generaldata, !is.na(depth))

# New variable: difference between flower depth and proboscis length
subsetgeneraldata$difference <- subsetgeneraldata$depth-subsetgeneraldata$tongue_length.tongue
# Separate long tongued-bees from short-tongued bees within the dataset. 6mm is arbitrary
subsetgeneraldata<-subsetgeneraldata %>% mutate(bee_size=if_else(tongue_length.tongue>6, "long", "short"))
subsetgeneraldata<-subsetgeneraldata %>% mutate(proboscislonger=if_else(tongue_length.tongue>depth, "true", "false"))

# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends
subsetgeneraldata<-subsetgeneraldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72))
subsetgeneraldata<-subsetgeneraldata %>% mutate(beewider=if_else(IT_improved>width, "true", "false"))


# Which species do visit long and short-tongued bees, in which abundance and mean depth of the plant species
longtonguedbees <- dplyr::filter(subsetgeneraldata, bee_size == "long")
shorttonguedbees <- dplyr::filter(subsetgeneraldata, bee_size == "short")

group1 <- group_by(longtonguedbees, plant_gs)
longtonguedbeesflowers <- summarize(group1, abundance=n(),depth=mean(depth))

group2 <- group_by(shorttonguedbees, plant_gs)
shorttonguedbeesflowers <- summarize(group2, abundance=n(),depth=mean(depth))

## data for histograms with species

histbeespecies<-subsetgeneraldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue), IT=mean(IT_improved),abundance=n())

histplantspecies<-subsetgeneraldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth), width=mean(width))

histplantspecies<-subsetgeneraldata %>%
  group_by(bee,plant_gs) %>%
  summarize(dif=mean(difference))

hist(histplantspecies$dif)

longtonguedbees <- dplyr::filter(subsetgeneraldata, proboscislonger == "false"&beewider=="true")
View(longtonguedbees)

hist(longtonguedbees$difference)

############ General plots

subsetgeneraldata %>%
  ggplot(aes(x=IT_improved),alpha=0.5)+
  geom_histogram(alpha=0.5,fill="red")+
  geom_histogram(aes(x=width),alpha=0.5)+
  theme_classic()+
  annotate(geom = "text", x = 3.2, y = 3500, label = "Red = Bee IT")+
  annotate(geom = "text", x = 3.2, y = 3000, label = "Grey = Corolla width")

  
#all bees tongue length histogram
hist(subsetgeneraldata$tongue_length.tongue, xlab="Tongue length (mm)",main="")
#all bees improved IT histogram
hist(subsetgeneraldata$IT_improved, xlab="IT (mm)",main="")
#bee species tongue length histogram
hist(histbeespecies$tongue, xlab="Tongue length (mm)",main="")
#bee species IT histogram
hist(histbeespecies$IT, xlab="IT (mm)",main="")
#all visits corolla depth histogram
hist(subsetgeneraldata$depth, xlab="Flower depth (mm)",main="")
#all visits corolla width histogram
hist(subsetgeneraldata$width, xlab="Flower width (mm)",main="")
#flower species corolla depth histogram
hist(histplantspecies$depth, xlab="Flower depth (mm)",main="")
#flower species corolla width histogram
hist(histplantspecies$width, xlab="Flower width (mm)",main="")
#all bees difference between depth and tongue
hist(subsetgeneraldata$difference, xlab="Flower depth - tongue length (mm)",main="")
#this looks impressively balanced around 0! What is null i.e. totally random visits? 

### which corolla depth do long-and short-tongued bees preferentially visit
hist(longtonguedbees$depth, xlab="Flower depth (mm)",main="")
hist(shorttonguedbees$depth, xlab="Flower depth (mm)",main="")
# looks like there is a pattern!!


## plot depth and proboscis length separing proboscis longer and shorter than flowers
subsetgeneraldata %>%
  ggplot(aes(x=depth, tongue_length.tongue))+
  geom_jitter(aes(color=proboscislonger),alpha=0.1, height=0.1)+
  theme_classic()
## plot width and IT separing bees larger and smaller than flowers
subsetgeneraldata %>%
  ggplot(aes(x=width, IT_improved))+
  geom_point(aes(color=beewider))+
  theme_classic()
table(subsetgeneraldata)

## same plots than before but separating by bee family
subsetgeneraldata %>%
  ggplot(aes(x=depth, tongue_length.tongue))+
  geom_point(aes(color=bee_family))+
  geom_smooth(aes(color=bee_family))+
  theme_classic()

subsetgeneraldata %>%
  ggplot(aes(x=width, IT_improved))+
  geom_point(aes(color=bee_family))+
  theme_classic()

## same plots than before but separating by socility degree
subsetgeneraldata %>%
  ggplot(aes(x=depth, tongue_length.tongue))+
  geom_jitter(aes(color=sociality),alpha=0.1, height=0.1)+
  theme_classic()

subsetgeneraldata %>%
  ggplot(aes(x=width, IT_improved))+
  geom_jitter(aes(color=sociality),alpha=0.1, height=0.1)+
  theme_classic()

## plot differences along time of day
subsetgeneraldata %>%
  ggplot(aes(x=midbout, difference))+
  geom_jitter(aes(color=proboscislonger),alpha=0.1, height=0.1)+
  geom_smooth(aes(color=proboscislonger))+
  theme_classic()


# # Specifically with the bees that are bigger than the flowers they visit, Plot the difference between corolla depth and tongue length with flower depth and tongue length
# plot(beeslargerthanflowers$difference~beeslargerthanflowers$depth)
# plot(beeslargerthanflowers$difference~beeslargerthanflowers$tongue_length.tongue)

# The same plot as before but separating by: bee family, plant family, and plant genus
# beeslargerthanflowers %>%
#   ggplot(aes(x=tongue_length.tongue, difference))+
#   geom_jitter(aes(color=bee_family),alpha=0.1, height=0.1)+
#   theme_classic()
# 
# beeslargerthanflowers %>%
#   ggplot(aes(x=tongue_length.tongue, difference))+
#   geom_jitter(aes(color=plant_family),alpha=0.1, height=0.1)+
#   theme_classic()
# 
# beeslargerthanflowers %>%
#   ggplot(aes(x=tongue_length.tongue, difference))+
#   geom_jitter(aes(color=plant_genus.y),alpha=0.1, height=0.1)+
#   theme_classic()

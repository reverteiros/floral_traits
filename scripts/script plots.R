
source("scripts/script_traits.R")

library(ggplot2)

## Define subsets of variables with appropriate names

# Work only with the plants from which we have data on traits
subsetgeneraldata <- generaldata[which(generaldata$depth != "NA"),]
# New variable: difference between flower depth and proboscis length
subsetgeneraldata$difference <- subsetgeneraldata$depth-subsetgeneraldata$tongue_length.tongue
# Separate long tongued-bees from short-tongued bees within the dataset. 6mm is arbitrary

#a couple notes on coding style here: 
#in tidyverse, there is  a function called mutate that does a lot of this 
#also, I would avoid having this two-step conversion system

#e.g.
# sgd<-subsetgeneraldata %>% mutate(bee_size=if_else(tongue_length.tongue>6, "long", "short"))

subsetgeneraldata$bee_size <- subsetgeneraldata$tongue_length.tongue
subsetgeneraldata$bee_size[subsetgeneraldata$bee_size<6]<-0
subsetgeneraldata$bee_size[subsetgeneraldata$bee_size>6]<-12
subsetgeneraldata$bee_size[subsetgeneraldata$bee_size=="0"]<-"short"
subsetgeneraldata$bee_size[subsetgeneraldata$bee_size=="12"]<-"long"
subsetgeneraldata$bee_size <- as.character(subsetgeneraldata$bee_size)

#here, rather than create a separate dataset, I'd add a factor column that splits the data like above, which can then be used for filtering, or for facet_wrap or color arguments in ggplot, but means creating fewer new objects

# Probiscis longer and shorter than flowers in different datasets
proboscislongerthanflowers <- dplyr::filter(subsetgeneraldata, tongue_length.tongue > depth)
proboscisshorterthanflowers <- dplyr::filter(subsetgeneraldata, tongue_length.tongue < depth)

# Flowers shorter than probiscis are matching. Nothing new. We want to see tongues shorter, they are the mismatchings and interesting. Split into small bees that can crawl into the flowers and those that cannot

#I think we can improve this with the estimate from your regression if head widths are typically larger than IT for small bees
beeslargerthanflowers <- dplyr::filter(proboscisshorterthanflowers, IT_mm > width)
beessmallerthanflowers <- dplyr::filter(proboscisshorterthanflowers, IT_mm < width)

# Separate long tongued-bees from short-tongued bees generating two new datasets
shorttonguedbees <- dplyr::filter(proboscisshorterthanflowers, bee_size == "short")
longtonguedbees <- dplyr::filter(proboscisshorterthanflowers, bee_size == "long")

# Which species do visit long and short-tongued bees, in which abundance and mean depth of the plant species
group1 <- group_by(longtonguedbees, plant_gs)
longtonguedbeesflowers <- summarize(group1, abundance=n(),depth=mean(depth))

#in tidyverse, 

# ltbf<-longtonguedbees %>% 
#   group_by(plant_gs) %>%#could also group by short vs long here
#   summarize(abund=n(), depth=mean(depth))

group2 <- group_by(shortonguedbees, plant_gs)
shortonguedbeesflowers <- summarize(group2, abundance=n(),depth=mean(depth))


############ General plots

#all bees tongue length histogram
hist(subsetgeneraldata$tongue_length.tongue, xlab="Tongue length (mm)",main="")
#all visits corolla depth histogram
hist(subsetgeneraldata$depth, xlab="Flower depth (mm)",main="")
#all visits corolla width histogram
hist(subsetgeneraldata$width, xlab="Flower width (mm)",main="")
#all visits tongue vs depth
plot(subsetgeneraldata$tongue_length.tongue~subsetgeneraldata$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)", xlim=c(0,33), ylim=c(0,32))
#all visits IT vs width
plot(subsetgeneraldata$IT_mm~subsetgeneraldata$width,ylab="IT distance (mm)", xlab="Flower width (mm)")
#all bees difference between depth and tongue
hist(subsetgeneraldata$difference, xlab="Flower depth - tongue length (mm)",main="")
#this looks impressively balanced around 0! What is null i.e. totally random visits? 

## divide in flowers with proboscis longer than flowers and shorter
# longer
#not sure what this says
plot(proboscislongerthanflowers$tongue_length.tongue~proboscislongerthanflowers$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)")
#what does this say?
plot(proboscislongerthanflowers$IT_mm~proboscislongerthanflowers$width,ylab="IT distance (mm)", xlab="Flower width (mm)")
# shorter
plot(proboscisshorterthanflowers$tongue_length.tongue~proboscisshorterthanflowers$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)")
plot(proboscisshorterthanflowers$IT_mm~proboscisshorterthanflowers$width,ylab="IT distance (mm)", xlab="Flower width (mm)")

# Difference between proboscis length and flower depth for bees that have shorter proboscis than the flowers they visit
hist(proboscisshorterthanflowers$difference, xlab="Flower depth - tongue length (mm)",main="")

# From the bees with shorter proboscis than the flowers they visit, separate the bees that are larger than flowers than the ones smaller. smaller bees can crawl into the flowers, exclude
hist(beeslargerthanflowers$difference, xlab="Flower depth - tongue length (mm)",main="")
hist(beeslargerthanflowers$tongue_length.tongue, xlab="Tongue length (mm)",main="")
plot(beeslargerthanflowers$tongue_length.tongue~beeslargerthanflowers$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)")

hist(beessmallerthanflowers$difference, xlab="Flower depth - tongue length (mm)",main="")
hist(beessmallerthanflowers$tongue_length.tongue, xlab="Tongue length (mm)",main="")
#is this plot labelled right?
plot(beessmallerthanflowers$IT_mm~beessmallerthanflowers$width,ylab="Tongue length (mm)", xlab="Flower depth (mm)")

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

## plot differences along time of day
beeslargerthanflowers %>%
  ggplot(aes(x=midbout, difference))+
  geom_point(aes(color=bee_size))+
  geom_smooth(aes(color=bee_size))+
  theme_classic()

### which plants do short and longued-tongued bees visit, in which abundance and mean corolla depth of these species
plot(longtonguedbeesflowers$abundance~longtonguedbeesflowers$depth)
plot(shortonguedbeesflowers$abundance~shortonguedbeesflowers$depth)
 

source("scripts/script_traits.R")

library(ggplot2)

# Error in eval(ei, envir) : object 'whichflowers' not found

## we need family data

#### this seems sketchy to me for a number of reasons... one is that I don't see why pseudoanthidium wouldn't be megachilidae ro Dufouria not Halictidae...
#### the bigger thing is that hard coding this just seems dangerous, clunky, hard to replicate and clean and fix.

#here's a table with almost every species we could imagine and their traits, as well as phylogenetic info like family. We need to update this table a bit and also there will be species complexes to do manually as below, I just merged this in script_traits.R


# then merge the family column from there with the data
#table generaldata not made with source command. Ok, I just figured it out. 


# generaldata$bee_family <- generaldata$bee_genus
# generaldata$bee_family <- as.character(generaldata$bee_family)
# 
# generaldata$bee_family[generaldata$bee_family=="Xylocopa"]<-"Apidae"
generaldata$bee_family[generaldata$bee_genus=="Triepeolus"]<-"Apidae"
# generaldata$bee_family[generaldata$bee_family=="Stelis"]<-"Megachilidae"
# generaldata$bee_family[generaldata$bee_family=="Sphecodes"]<-"Halictidae"
# generaldata$bee_family[generaldata$bee_family=="sand wasp"]<-"Others"
# generaldata$bee_family[generaldata$bee_family=="Ptilothrix"]<-"Apidae"
# generaldata$bee_family[generaldata$bee_family=="Pseudoanthidium"]<-"Others"
# generaldata$bee_family[generaldata$bee_family=="Osmia"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_genus=="Nomada"]<-"Apidae"
# generaldata$bee_family[generaldata$bee_family=="Melissodes"]<-"Apidae"
# generaldata$bee_family[generaldata$bee_family=="Megachile"]<-"Megachilidae"
# generaldata$bee_family[generaldata$bee_family=="Lithurgus"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_genus=="Lasioglossum"]<-"Halictidae"
# generaldata$bee_family[generaldata$bee_family=="Hoplitis"]<-"Megachilidae"
# generaldata$bee_family[generaldata$bee_family=="Heriades"]<-"Megachilidae"
# generaldata$bee_family[generaldata$bee_family=="Halictus"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_genus=="Hylaeus"]<-"Colletidae"
generaldata$bee_family[generaldata$bee_genus=="Dufouria"]<-"Halictidae"
# generaldata$bee_family[generaldata$bee_family=="Coelioxys"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_genus=="Ceratina"]<-"Apidae"
# generaldata$bee_family[generaldata$bee_family=="Calliopsis"]<-"Andrenidae"
# generaldata$bee_family[generaldata$bee_family=="Bombus"]<-"Apidae"
# generaldata$bee_family[generaldata$bee_family=="Augochloropsis"]<-"Halictidae"
# generaldata$bee_family[generaldata$bee_family=="Augochlorella"]<-"Halictidae"
# generaldata$bee_family[generaldata$bee_family=="Augochlora"]<-"Halictidae"
# generaldata$bee_family[generaldata$bee_family=="Anthophora"]<-"Apidae"
# generaldata$bee_family[generaldata$bee_family=="Agapostemon"]<-"Halictidae"
# generaldata$bee_family[generaldata$bee_family=="Anacrabro"]<-"Others"
# generaldata$bee_family[generaldata$bee_family=="Andrena"]<-"Andrenidae"
# generaldata$bee_family[generaldata$bee_family=="Anthidiellum"]<-"Megachilidae"
# generaldata$bee_family[generaldata$bee_family=="Anthidium"]<-"Megachilidae"

# Remove other bugs
#instead, use basicID column
# generaldata<-generaldata %>% filter(basicID=="bee")


#I think all bees have families now
generaldata[is.na(generaldata$bee_family),]
table(generaldata$bee_family)

# Calculate bee proboscis length and body size
Out <- ITconverter(IT = generaldata$IT_mm, family = as.character(generaldata$bee_family))

generaldata$body_mass <- Out$body_mass
generaldata$tongue_length.tongue <- Out$tongue_length.tongue



## Plots only for the plants from which we have data on traits

subsetgeneraldata <- generaldata[which(generaldata$depth != "NA"),]

hist(subsetgeneraldata$tongue_length.tongue, xlab="Tongue length (mm)",main="")
hist(subsetgeneraldata$depth, xlab="Flower depth (mm)",main="")
hist(subsetgeneraldata$width, xlab="Flower width (mm)",main="")
plot(subsetgeneraldata$tongue_length.tongue~subsetgeneraldata$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)")
plot(subsetgeneraldata$IT_mm~subsetgeneraldata$width,ylab="IT distance (mm)", xlab="Flower width (mm)")

## divide in flowers with proboscis longer  than flowers and shorter

proboscislongerthanflowers <- dplyr::filter(subsetgeneraldata, tongue_length.tongue > depth)
proboscisshorterthanflowers <- dplyr::filter(subsetgeneraldata, tongue_length.tongue < depth)

plot(proboscislongerthanflowers$tongue_length.tongue~proboscislongerthanflowers$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)")
plot(proboscislongerthanflowers$IT_mm~proboscislongerthanflowers$width,ylab="IT distance (mm)", xlab="Flower width (mm)")

plot(proboscisshorterthanflowers$tongue_length.tongue~proboscisshorterthanflowers$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)",xlim=c(0,32),ylim=c(0,15))
plot(proboscisshorterthanflowers$IT_mm~proboscisshorterthanflowers$width,ylab="IT distance (mm)", xlab="Flower width (mm)",xlim=c(0,6),ylim=c(0,6))

# Calculate the difference between proboscis length and flower depth for bees
# that have shorter proboscis than the flowers they visit, histogram

proboscisshorterthanflowers$difference <- proboscisshorterthanflowers$depth-proboscisshorterthanflowers$tongue_length.tongue

hist(proboscisshorterthanflowers$difference, xlab="Flower depth - tongue length (mm)",main="")

# we barely see anything, restrict to 5mm difference

difff <- dplyr::filter(proboscisshorterthanflowers, difference < 5)

hist(difff$difference, xlab="Flower depth - tongue length (mm)",main="")

# From these data, separate the bees that are larger than flowers than the ones smaller

beeslargerthanflowers <- dplyr::filter(proboscisshorterthanflowers, IT_mm > width)
beessmallerthanflowers <- dplyr::filter(proboscisshorterthanflowers, IT_mm < width)

hist(beeslargerthanflowers$difference, xlab="Flower depth - tongue length (mm)",main="")
hist(beessmallerthanflowers$difference, xlab="Flower depth - tongue length (mm)",main="")

length(beeslargerthanflowers$difference)
length(beessmallerthanflowers$difference)

plot(beeslargerthanflowers$tongue_length.tongue~beeslargerthanflowers$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)")

plot(beessmallerthanflowers$IT_mm~beessmallerthanflowers$width,ylab="Tongue length (mm)", xlab="Flower depth (mm)")



# Plot the difference between corolla depth and tongue length with these two

plot(beeslargerthanflowers$difference~beeslargerthanflowers$depth)
plot(beeslargerthanflowers$difference~beeslargerthanflowers$tongue_length.tongue)

# Show the plant family in the plots

beeslargerthanflowers %>%
  ggplot(aes(x=tongue_length.tongue, difference))+
  geom_jitter(aes(color=bee_family),alpha=0.1, height=0.1)+
  theme_classic()

beeslargerthanflowers %>%
  ggplot(aes(x=tongue_length.tongue, difference))+
  geom_jitter(aes(color=plant_family),alpha=0.1, height=0.1)+
  theme_classic()

beeslargerthanflowers %>%
  ggplot(aes(x=tongue_length.tongue, difference))+
  geom_jitter(aes(color=plant_genus.y),alpha=0.1, height=0.1)+
  theme_classic()

beeslargerthanflowers %>%
  ggplot(aes(x=depth, difference))+
  geom_point(aes(color=genus_species))+
  theme_classic()

table(droplevels(as.factor(beeslargerthanflowers$genus_species)))

flowers <- floraltraits %>% semi_join(beeslargerthanflowers, by=(c("genus_species")))


shortflowers <- dplyr::filter(beeslargerthanflowers, difference < 5)

hist(shortflowers$difference)

shortflowers %>%
  ggplot(aes(x=depth, tongue_length.tongue))+
  geom_point(aes(color=genus_species))+
  theme_classic()


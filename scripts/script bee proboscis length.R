
source("scripts/script_traits.R")
library(ggplot2)

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

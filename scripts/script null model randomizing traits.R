
source("scripts/script_traits.R")

library(ggplot2)

### including all data

alldata <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))

databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue))

databees <- as.data.frame(databees)
tongue <- databees$tongue
tonguesampled <- sample(databees$tongue)
databees$tonguesampled <- tonguesampled

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth))

dataflowers <- as.data.frame(dataflowers)
depth <- dataflowers$depth
depthsampled <- sample(dataflowers$depth)
dataflowers$depthsampled <- depthsampled


datasampled <- alldata %>% left_join(databees, by=(c("bee")))
datasampled <- datasampled %>% left_join(dataflowers, by=(c("plant_gs")))

datasampled$difference <- datasampled$tonguesampled-datasampled$depthsampled

hist(datasampled$difference)


source("scripts/script_traits.R")

library(ggplot2)

### including all data

alldata <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))
alldata$difference <- alldata$tongue_length.tongue-alldata$depth

databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue))

databees <- as.data.frame(databees)
tongue <- databees$tongue

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth))

dataflowers <- as.data.frame(dataflowers)
depth <- dataflowers$depth


randomize <- function(n){
  sumdifferences <- numeric(n)
  for(i in 1:n){
    tonguesampled <- sample(databees$tongue)
    databees$tonguesampled <- tonguesampled
    depthsampled <- sample(dataflowers$depth)
    dataflowers$depthsampled <- depthsampled
    datasampled <- alldata %>% left_join(databees, by=(c("bee")))
    datasampled2 <- datasampled %>% left_join(dataflowers, by=(c("plant_gs")))
    datasampled2$differencesampled <- datasampled2$tonguesampled-datasampled2$depthsampled
    sumdifferences[i] <- sum(abs(datasampled2$differencesampled))
  }
  print(sumdifferences)
}

hist(randomize(10000))
abline(v = sum(abs(alldata$difference)))



source("scripts/script_traits.R")

library(ggplot2)

### Second null model

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
    datasampled <- alldata %>% left_join(databees, by=(c("bee")))
    datasampled$differencesampled <- datasampled$tonguesampled-datasampled$depth
    sumdifferences[i] <- sum((abs(datasampled$differencesampled))^2)
  }
  return(sumdifferences)
}

hist(randomize(1000))
abline(v = sum((abs(alldata$difference))^2))


## First null model


randomize2 <- function(n){
  sumdifferences <- numeric(n)
  for(i in 1:n){
    alldata$nullbees <- sample(databees$tongue, length(alldata$difference), replace = T, prob = databees$abundance)
    alldata$nullflower <- sample(dataflowers$depth, length(alldata$difference), replace = T, prob = dataflowers$abundance)
    alldata <- alldata %>% mutate(nulldiff=nullflower-nullbees)
    sumdifferences[i] <- sum((abs(alldata$nulldiff))^2)
  }
  return(sumdifferences)
}

hist(randomize2(1000))
abline(v = sum((abs(alldata$difference)))^2)

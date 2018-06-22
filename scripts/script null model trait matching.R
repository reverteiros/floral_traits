
source("scripts/script_traits.R")

library(ggplot2)

alldata <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))

# New variable: difference between flower depth and proboscis length
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())


alldata$nullbees <- sample(databees$tongue, length(alldata$difference), replace = T, prob = databees$abundance)
alldata$nullflower <- sample(dataflowers$depth, length(alldata$difference), replace = T, prob = dataflowers$abundance)

alldata <- alldata %>% mutate(nulldiff=nullflower-nullbees)

alldata %>%
  ggplot(aes(x=difference),alpha=0.5)+
  geom_histogram(alpha=0.5,fill="red")+
  geom_histogram(aes(x=nulldiff),alpha=0.5)+
  theme_classic()+
  labs(x = "Differences between flower depth and tongue length (mm)")+
  annotate(geom = "text", x = 20, y = 4000, label = "Red = observed")+
  annotate(geom = "text", x = 20, y = 3800, label = "Grey = expected")

hist(alldata$difference)



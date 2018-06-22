
source("scripts/script_traits.R")

library(ggplot2)

### including all data

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

### filtering for the bees that have shorter proboscis than corolla (mismatching)

alldata<-alldata %>% mutate(proboscislonger=if_else(tongue_length.tongue>depth, "true", "false"))
alldata<-alldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72))
alldata<-alldata %>% mutate(beewider=if_else(IT_improved>width, "true", "false"))

# Which species do visit long and short-tongued bees, in which abundance and mean depth of the plant species

shorttonguedbees <- dplyr::filter(alldata, beewider=="true")

databees<-shorttonguedbees %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())

dataflowers<-shorttonguedbees %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())


shorttonguedbees$nullbees <- sample(databees$tongue, length(shorttonguedbees$difference), replace = T, prob = databees$abundance)
shorttonguedbees$nullflower <- sample(dataflowers$depth, length(shorttonguedbees$difference), replace = T, prob = dataflowers$abundance)

shorttonguedbees <- shorttonguedbees %>% mutate(nulldiff=nullflower-nullbees)

shorttonguedbees %>%
  ggplot(aes(x=difference),alpha=0.5)+
  geom_histogram(alpha=0.5,fill="red")+
  geom_histogram(aes(x=nulldiff),alpha=0.5)+
  theme_classic()+
  labs(x = "Differences between flower depth and tongue length (mm)")+
  annotate(geom = "text", x = 20, y = 2200, label = "Red = observed")+
  annotate(geom = "text", x = 20, y = 2000, label = "Grey = expected")

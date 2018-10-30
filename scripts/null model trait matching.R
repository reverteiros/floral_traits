
source("scripts/traits.R")

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

# plot regression with null model and real data
alldata$nullbees <- sample(databees$tongue, length(alldata$difference), replace = T, prob = databees$abundance)
alldata$nullflower <- sample(dataflowers$depth, length(alldata$difference), replace = T, prob = dataflowers$abundance)

ggplot(alldata, aes(y=nullbees, x=nullflower)) + 
  geom_jitter(alpha=0.01, height=0.1) + 
  theme_classic() #+

ggplot(subsetgeneraldata, aes(y=tongue_length.tongue, x=depth)) + 
  geom_jitter(alpha=0.01, height=0.1) + 
  theme_classic() #+


alldata <- alldata %>% mutate(nulldiff=nullflower-nullbees)

#################
### Really nice histogram for flower_depth-tongue_length for all bees, superimposed on a histogram of differences if visitor ID were randomized

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
################
## again histogram of differences between permutation null with observed data superimposed
shorttonguedbees %>%
  ggplot(aes(x=difference),alpha=0.5)+
  geom_histogram(alpha=0.5,fill="red")+
  geom_histogram(aes(x=nulldiff),alpha=0.5)+
  theme_classic()+
  labs(x = "Differences between flower depth and tongue length (mm)")+
  annotate(geom = "text", x = 20, y = 2200, label = "Red = observed")+
  annotate(geom = "text", x = 20, y = 2000, label = "Grey = expected")


# ggplot(subsetgeneraldata, aes(y=tongue_length.tongue, x=depth)) + 
#   geom_jitter(alpha=0.1, height=0.1) + 
#   theme_classic() #+



## null model per site and sampling round

# 
# baldpate1 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "1" & site == "Baldpate")
# databees<-baldpate1 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-baldpate1 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# baldpate1$nullbees <- sample(databees$tongue, length(baldpate1$difference), replace = T, prob = databees$abundance)
# baldpate1$nullflower <- sample(dataflowers$depth, length(baldpate1$difference), replace = T, prob = dataflowers$abundance)
# baldpate1 <- baldpate1 %>% mutate(nulldiff=nullflower-nullbees)
# 
# baldpate2 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "2" & site == "Baldpate")
# databees<-baldpate2 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-baldpate2 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# baldpate2$nullbees <- sample(databees$tongue, length(baldpate2$difference), replace = T, prob = databees$abundance)
# baldpate2$nullflower <- sample(dataflowers$depth, length(baldpate2$difference), replace = T, prob = dataflowers$abundance)
# baldpate2 <- baldpate2 %>% mutate(nulldiff=nullflower-nullbees)
# 
# baldpate3 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "3" & site == "Baldpate")
# databees<-baldpate3 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-baldpate3 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# baldpate3$nullbees <- sample(databees$tongue, length(baldpate3$difference), replace = T, prob = databees$abundance)
# baldpate3$nullflower <- sample(dataflowers$depth, length(baldpate3$difference), replace = T, prob = dataflowers$abundance)
# baldpate3 <- baldpate3 %>% mutate(nulldiff=nullflower-nullbees)
# 
# baldpate4 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "4" & site == "Baldpate")
# databees<-baldpate4 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-baldpate4 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# baldpate4$nullbees <- sample(databees$tongue, length(baldpate4$difference), replace = T, prob = databees$abundance)
# baldpate4$nullflower <- sample(dataflowers$depth, length(baldpate4$difference), replace = T, prob = dataflowers$abundance)
# baldpate4 <- baldpate4 %>% mutate(nulldiff=nullflower-nullbees)
# 
# baldpate5 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "5" & site == "Baldpate")
# databees<-baldpate5 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-baldpate5 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# baldpate5$nullbees <- sample(databees$tongue, length(baldpate5$difference), replace = T, prob = databees$abundance)
# baldpate5$nullflower <- sample(dataflowers$depth, length(baldpate5$difference), replace = T, prob = dataflowers$abundance)
# baldpate5 <- baldpate5 %>% mutate(nulldiff=nullflower-nullbees)
# 
# coldsoil1 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "1" & site == "Cold Soil")
# databees<-coldsoil1 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-coldsoil1 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# coldsoil1$nullbees <- sample(databees$tongue, length(coldsoil1$difference), replace = T, prob = databees$abundance)
# coldsoil1$nullflower <- sample(dataflowers$depth, length(coldsoil1$difference), replace = T, prob = dataflowers$abundance)
# coldsoil1 <- coldsoil1 %>% mutate(nulldiff=nullflower-nullbees)
# 
# coldsoil2 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "2" & site == "Cold Soil")
# databees<-coldsoil2 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-coldsoil2 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# coldsoil2$nullbees <- sample(databees$tongue, length(coldsoil2$difference), replace = T, prob = databees$abundance)
# coldsoil2$nullflower <- sample(dataflowers$depth, length(coldsoil2$difference), replace = T, prob = dataflowers$abundance)
# coldsoil2 <- coldsoil2 %>% mutate(nulldiff=nullflower-nullbees)
# 
# coldsoil3 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "3" & site == "Cold Soil")
# databees<-coldsoil3 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-coldsoil3 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# coldsoil3$nullbees <- sample(databees$tongue, length(coldsoil3$difference), replace = T, prob = databees$abundance)
# coldsoil3$nullflower <- sample(dataflowers$depth, length(coldsoil3$difference), replace = T, prob = dataflowers$abundance)
# coldsoil3 <- coldsoil3 %>% mutate(nulldiff=nullflower-nullbees)
# 
# coldsoil4 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "4" & site == "Cold Soil")
# databees<-coldsoil4 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-coldsoil4 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# coldsoil4$nullbees <- sample(databees$tongue, length(coldsoil4$difference), replace = T, prob = databees$abundance)
# coldsoil4$nullflower <- sample(dataflowers$depth, length(coldsoil4$difference), replace = T, prob = dataflowers$abundance)
# coldsoil4 <- coldsoil4 %>% mutate(nulldiff=nullflower-nullbees)
# 
# coldsoil5 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "5" & site == "Cold Soil")
# databees<-coldsoil5 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-coldsoil5 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# coldsoil5$nullbees <- sample(databees$tongue, length(coldsoil5$difference), replace = T, prob = databees$abundance)
# coldsoil5$nullflower <- sample(dataflowers$depth, length(coldsoil5$difference), replace = T, prob = dataflowers$abundance)
# coldsoil5 <- coldsoil5 %>% mutate(nulldiff=nullflower-nullbees)
# 
# foxhill1 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "1" & site == "Fox Hill")
# databees<-foxhill1 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-foxhill1 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# foxhill1$nullbees <- sample(databees$tongue, length(foxhill1$difference), replace = T, prob = databees$abundance)
# foxhill1$nullflower <- sample(dataflowers$depth, length(foxhill1$difference), replace = T, prob = dataflowers$abundance)
# foxhill1 <- foxhill1 %>% mutate(nulldiff=nullflower-nullbees)
# 
# foxhill2 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "2" & site == "Fox Hill")
# databees<-foxhill2 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-foxhill2 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# foxhill2$nullbees <- sample(databees$tongue, length(foxhill2$difference), replace = T, prob = databees$abundance)
# foxhill2$nullflower <- sample(dataflowers$depth, length(foxhill2$difference), replace = T, prob = dataflowers$abundance)
# foxhill2 <- foxhill2 %>% mutate(nulldiff=nullflower-nullbees)
# 
# foxhill3 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "3" & site == "Fox Hill")
# databees<-foxhill3 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-foxhill3 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# foxhill3$nullbees <- sample(databees$tongue, length(foxhill3$difference), replace = T, prob = databees$abundance)
# foxhill3$nullflower <- sample(dataflowers$depth, length(foxhill3$difference), replace = T, prob = dataflowers$abundance)
# foxhill3 <- foxhill3 %>% mutate(nulldiff=nullflower-nullbees)
# 
# foxhill4 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "4" & site == "Fox Hill")
# databees<-foxhill4 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-foxhill4 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# foxhill4$nullbees <- sample(databees$tongue, length(foxhill4$difference), replace = T, prob = databees$abundance)
# foxhill4$nullflower <- sample(dataflowers$depth, length(foxhill4$difference), replace = T, prob = dataflowers$abundance)
# foxhill4 <- foxhill4 %>% mutate(nulldiff=nullflower-nullbees)
# 
# foxhill5 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "5" & site == "Fox Hill")
# databees<-foxhill5 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-foxhill5 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# foxhill5$nullbees <- sample(databees$tongue, length(foxhill5$difference), replace = T, prob = databees$abundance)
# foxhill5$nullflower <- sample(dataflowers$depth, length(foxhill5$difference), replace = T, prob = dataflowers$abundance)
# foxhill5 <- foxhill5 %>% mutate(nulldiff=nullflower-nullbees)
# 
# ias1 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "1" & site == "IAS")
# databees<-ias1 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-ias1 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# ias1$nullbees <- sample(databees$tongue, length(ias1$difference), replace = T, prob = databees$abundance)
# ias1$nullflower <- sample(dataflowers$depth, length(ias1$difference), replace = T, prob = dataflowers$abundance)
# ias1 <- ias1 %>% mutate(nulldiff=nullflower-nullbees)
# 
# ias2 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "2" & site == "IAS")
# databees<-ias2 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-ias2 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# ias2$nullbees <- sample(databees$tongue, length(ias2$difference), replace = T, prob = databees$abundance)
# ias2$nullflower <- sample(dataflowers$depth, length(ias2$difference), replace = T, prob = dataflowers$abundance)
# ias2 <- ias2 %>% mutate(nulldiff=nullflower-nullbees)
# 
# ias3 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "3" & site == "IAS")
# databees<-ias3 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-ias3 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# ias3$nullbees <- sample(databees$tongue, length(ias3$difference), replace = T, prob = databees$abundance)
# ias3$nullflower <- sample(dataflowers$depth, length(ias3$difference), replace = T, prob = dataflowers$abundance)
# ias3 <- ias3 %>% mutate(nulldiff=nullflower-nullbees)
# 
# ias4 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "4" & site == "IAS")
# databees<-ias4 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-ias4 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# ias4$nullbees <- sample(databees$tongue, length(ias4$difference), replace = T, prob = databees$abundance)
# ias4$nullflower <- sample(dataflowers$depth, length(ias4$difference), replace = T, prob = dataflowers$abundance)
# ias4 <- ias4 %>% mutate(nulldiff=nullflower-nullbees)
# 
# ias5 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "5" & site == "IAS")
# databees<-ias5 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-ias5 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# ias5$nullbees <- sample(databees$tongue, length(ias5$difference), replace = T, prob = databees$abundance)
# ias5$nullflower <- sample(dataflowers$depth, length(ias5$difference), replace = T, prob = dataflowers$abundance)
# ias5 <- ias5 %>% mutate(nulldiff=nullflower-nullbees)
# 
# urwa1 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "1" & site == "URWA")
# databees<-urwa1 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-urwa1 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# urwa1$nullbees <- sample(databees$tongue, length(urwa1$difference), replace = T, prob = databees$abundance)
# urwa1$nullflower <- sample(dataflowers$depth, length(urwa1$difference), replace = T, prob = dataflowers$abundance)
# urwa1 <- urwa1 %>% mutate(nulldiff=nullflower-nullbees)
# 
# urwa2 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "2" & site == "URWA")
# databees<-urwa2 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-urwa2 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# urwa2$nullbees <- sample(databees$tongue, length(urwa2$difference), replace = T, prob = databees$abundance)
# urwa2$nullflower <- sample(dataflowers$depth, length(urwa2$difference), replace = T, prob = dataflowers$abundance)
# urwa2 <- urwa2 %>% mutate(nulldiff=nullflower-nullbees)
# 
# urwa3 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "3" & site == "URWA")
# databees<-urwa3 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-urwa3 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# urwa3$nullbees <- sample(databees$tongue, length(urwa3$difference), replace = T, prob = databees$abundance)
# urwa3$nullflower <- sample(dataflowers$depth, length(urwa3$difference), replace = T, prob = dataflowers$abundance)
# urwa3 <- urwa3 %>% mutate(nulldiff=nullflower-nullbees)
# 
# urwa4 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "4" & site == "URWA")
# databees<-urwa4 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-urwa4 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# urwa4$nullbees <- sample(databees$tongue, length(urwa4$difference), replace = T, prob = databees$abundance)
# urwa4$nullflower <- sample(dataflowers$depth, length(urwa4$difference), replace = T, prob = dataflowers$abundance)
# urwa4 <- urwa4 %>% mutate(nulldiff=nullflower-nullbees)
# 
# urwa5 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "5" & site == "URWA")
# databees<-urwa5 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-urwa5 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# urwa5$nullbees <- sample(databees$tongue, length(urwa5$difference), replace = T, prob = databees$abundance)
# urwa5$nullflower <- sample(dataflowers$depth, length(urwa5$difference), replace = T, prob = dataflowers$abundance)
# urwa5 <- urwa5 %>% mutate(nulldiff=nullflower-nullbees)
# 
# stirling1 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "1" & site == "Lord Stirling")
# databees<-stirling1 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-stirling1 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# stirling1$nullbees <- sample(databees$tongue, length(stirling1$difference), replace = T, prob = databees$abundance)
# stirling1$nullflower <- sample(dataflowers$depth, length(stirling1$difference), replace = T, prob = dataflowers$abundance)
# stirling1 <- stirling1 %>% mutate(nulldiff=nullflower-nullbees)
# 
# stirling2 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "2" & site == "Lord Stirling")
# databees<-stirling2 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-stirling2 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# stirling2$nullbees <- sample(databees$tongue, length(stirling2$difference), replace = T, prob = databees$abundance)
# stirling2$nullflower <- sample(dataflowers$depth, length(stirling2$difference), replace = T, prob = dataflowers$abundance)
# stirling2 <- stirling2 %>% mutate(nulldiff=nullflower-nullbees)
# 
# stirling3 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "3" & site == "Lord Stirling")
# databees<-stirling3 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-stirling3 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# stirling3$nullbees <- sample(databees$tongue, length(stirling3$difference), replace = T, prob = databees$abundance)
# stirling3$nullflower <- sample(dataflowers$depth, length(stirling3$difference), replace = T, prob = dataflowers$abundance)
# stirling3 <- stirling3 %>% mutate(nulldiff=nullflower-nullbees)
# 
# stirling4 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "4" & site == "Lord Stirling")
# databees<-stirling4 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-stirling4 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# stirling4$nullbees <- sample(databees$tongue, length(stirling4$difference), replace = T, prob = databees$abundance)
# stirling4$nullflower <- sample(dataflowers$depth, length(stirling4$difference), replace = T, prob = dataflowers$abundance)
# stirling4 <- stirling4 %>% mutate(nulldiff=nullflower-nullbees)
# 
# stirling5 <- dplyr::filter(alldata, beewider=="true" & sampling_round == "5" & site == "Lord Stirling")
# databees<-stirling5 %>%
#   group_by(bee) %>%
#   summarize(tongue=mean(tongue_length.tongue),abundance=n())
# dataflowers<-stirling5 %>%
#   group_by(plant_gs) %>%
#   summarize(depth=mean(depth),abundance=n())
# stirling5$nullbees <- sample(databees$tongue, length(stirling5$difference), replace = T, prob = databees$abundance)
# stirling5$nullflower <- sample(dataflowers$depth, length(stirling5$difference), replace = T, prob = dataflowers$abundance)
# stirling5 <- stirling5 %>% mutate(nulldiff=nullflower-nullbees)

nullpersiteandtime <- dplyr::bind_rows(stirling1,stirling2,stirling3,stirling4,stirling5,ias1,ias2,ias3,ias4,ias5,baldpate1,baldpate2,baldpate3,baldpate4,baldpate5,urwa1,urwa2,urwa3,urwa4,urwa5,coldsoil1,coldsoil2,coldsoil3,coldsoil4,coldsoil5,foxhill1,foxhill2,foxhill3,foxhill4,foxhill5)

filtered <- dplyr::filter(nullpersiteandtime, !site=="Featherbed"& !site=="D&R Greenway"& !is.na(sampling_round))

 
 
filtered %>%
  ggplot(aes(x=difference),alpha=0.5)+
  geom_histogram(alpha=0.5,fill="red")+
  geom_histogram(aes(x=nulldiff),alpha=0.5)+
  facet_wrap(~ sampling_round+site)+
  theme_classic()+geom_vline(xintercept=0)

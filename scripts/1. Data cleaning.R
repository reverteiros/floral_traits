
# install.packages("devtools")
# devtools::install_github("ibartomeus/BeeIT")
library(BeeIT)
library(tidyverse)

############# Read IT data
traits<-read.csv("data/wlab_db_5-31-18_3-21 PM_specimen_level_traits.csv")
# Look for more recent/ publicly available version of these data. 

## Create vector of complete bee species name pasting genus and species
traits$bee <- paste(traits$genus, traits$species, sep = "_")

## There are NULLs that promote error. Remove them
traits<-traits[traits$ITlength_mm!="NULL",]
## Clean sex data. There are many classes
traits<-traits[-which(traits$sex == "unknown"|traits$sex == "NULL"),]

#do we want to just use workers since that is the majority of the data? 
# I think this is the right move. 
traits$sex[which(traits$sex=="queen"|traits$sex=="worker")]<-"female"
traits$sex <- droplevels(traits$sex)

## Variable IT distance is still a factor. Convert to numeric
traits$ITlength_mm <- as.numeric(as.character(traits$ITlength_mm))

# ## check intra-species variability in IT
# nITperbee <- summarize(group, IT_mm=mean(ITlength_mm),CV=(sd(ITlength_mm)/mean(ITlength_mm)*100),n=n())
# 
# plot(nITperbee$CV~nITperbee$n,xlim=c(0,15))
# plot(nITperbee$CV~nITperbee$n)


#### read new bee its dataset
newbeedata<-read.table("data/bee_its.txt",header = T)

newbeedata$ITlength_mm <- as.numeric(as.character(newbeedata$ITlength_mm))

traits <- dplyr::bind_rows(traits, newbeedata)

traits$bee <- as.character(traits$bee)
#fix spelling mistakes. Note this is on the $bee column so "species" column will still be wrong
traits$bee[which(traits$bee=="Lasioglossum_coerulum")]<-"Lasioglossum_coeruleum"
traits$bee[which(traits$bee=="Lasioglossum_nigroviride")]<-"Lasioglossum_nigroviridae"
traits$bee[which(traits$bee=="Melissodes_tridonis")]<-"Melissodes_trinodis"


## Create a table grouping per bee
 
## Calculate mean IT per each previous group
nITperbee <- group_by(traits, bee) %>% 
  summarize(IT_mm=mean(ITlength_mm))


######### Read male bee dataset
male<-read.csv("data/2016_male_bee_dataset.csv")

# ## Clean sex data. There are many classes
# male<-male[-which(male$bee_sex == "Unidentified"|male$bee_sex == ""),]
# male$bee_sex[which(male$bee_sex=="m")]<-"M"
# male$bee_sex <- as.character(male$bee_sex)
# 
# ## Change names of variable to be the same as the traits dataframe.
# ## Change sex names also
# male$bee_sex[which(male$bee_sex=="M")]<-"male"
# male$bee_sex[which(male$bee_sex=="F")]<-"female"
# male$sex <- male$bee_sex


### could get some matches. need to clean up traits table a lot
#nymphaearum=oceanicum
#hichensi=weemsi
#most melissodes spp. might have incorrect gender right now (e.g. disponsa instead of the correct disponsus)

# ## database of IT  separate by bee and sex
# group <- group_by(traits, bee, sex)
# nITperbee2 <- summarize(group, measured=n())
# 
# ## database of michael separate by bee and sex
# group <- group_by(male, bee, bee_sex)
# michaeldata <- summarize(group, abundance=n())
# maleIT <- michaeldata %>%rename(sex=bee_sex) %>%  left_join(nITperbee2, by=(c("bee","sex")))
# 
# sum(table(maleIT$measured)) #147




########### change bee names that are wrong
male$bee <- as.character(male$bee)
male$bee[which(male$bee=="Stelis_louisae")]<-"Stelis_louisiae"
male$bee[which(male$bee=="Lasioglossum_birkmannii")]<-"Lasioglossum_birkmanni"
male$bee[which(male$bee=="Coelioxys_octodentatus")]<-"Coelioxys_octodentata"
male$bee[which(male$bee=="Coelioxys_alternatus")]<-"Coelioxys_alternata"
male$bee[which(male$bee=="Coelioxys_modestus")]<-"Coelioxys_modesta"
male$bee[which(male$bee=="Lasioglossum_nigroviride")]<-"Lasioglossum_nigroviridae"




## database of traits. separate by bee
nITperbee3 <- traits %>%
  group_by(bee) %>% 
  summarize(measures=n())

## database of michael separate by bee
maleIT <- male %>%
  group_by(bee) %>%
  summarize(abundance=n()) %>% 
  left_join(nITperbee3, by=(c("bee")))

#### How many species are in the dataset?
sum(table(maleIT$abundance)) # 160
### How many species do we have IT data?
sum(table(maleIT$measures))  # 148


############# Read new floral traits database
flowersmeasured <- read.table("data/flowers.txt",header = T) %>%
  group_by(Species) %>%
  summarize(depth=mean(depth),width=mean(width))

generaldata3$bee_family[generaldata3$bee_genus=="Triepeolus"]<-"Apidae"



############# Read floral traits database
floraltraits_original <- read.csv("data/floraltraits.csv")
floraltraits <- data.frame(floraltraits_original$genus_species,floraltraits_original$depth,floraltraits_original$width)
names(floraltraits) <- c("Species","depth","width")

## join two floral databases
flowerstotal <- dplyr::bind_rows(floraltraits, flowersmeasured)
names(flowerstotal) <- c("plant_gs","depth","width")

# check no duplicates
length(unique(flowerstotal$plant_gs))
length(flowerstotal$plant_gs)

## add the data to the bee dataset

male$plant_gs <- paste(male$plant_genus, male$plant_species, sep = "_")

male$plant_gs[which(male$plant_gs=="Eutrochium maculatum_")]<-"Eutrochium_maculatum"
############### include the extrafloral nectaries of chammaecrista fasciculata
male$plant_gs[which(male$plant_gs=="Chamaecrista_fasciculata_nectary")]<-"Chamaecrista_fasciculata"

michaelflowers <- male %>%
  group_by(plant_gs) %>%
  summarize(abundance=n()) %>% 
  left_join(flowerstotal, by=(c("plant_gs")))

#### How many species are in the dataset?
length(unique(michaelflowers$plant_gs)) # 112
### How many species do we have floral traits data?
sum(table(michaelflowers$depth))  # 60


# # Which species we lack data, and how many visits do they have
# flowersnodata <- michaeldata %>% anti_join(flowersmeasured, by=(c("genus_species")))
# # from which species do we have more than 80 visits and worth measuring
# dplyr::filter(flowersnodata, abundance > 80)


# #### bee head size
# names(traits)
# ## There are NULLs that promote error. Remove them
# traits<-traits[traits$head_width!="NULL",]
# ## Variable IT distance is still a factor. Convert to numeric
# traits$head_width <- as.numeric(as.character(traits$head_width))
# 
# ## Create a table grouping per bee
# group <- group_by(traits, bee)
# ## Calculate mean IT per each previous group
# nheadwidthperbee <- summarize(group, measured=n())
# 
# group <- group_by(male, bee)
# michaeldata <- summarize(group, abundance=n())
# maleIT <- michaeldata %>% left_join(nheadwidthperbee, by=(c("bee")))
# 
# names(traits)
# traits %>%
#   ggplot(aes(x=ITlength_mm, as.numeric(head_width)))+
#   geom_point(aes(color=genus))+
#   theme_classic()
# 
# 
# ### Bombus
# bumblebees <- traits %>%
#   filter(genus =="Bombus")
# 
# linealmodelbombus <- lm(bumblebees$head_width~bumblebees$ITlength_mm)
# summary(linealmodelbombus)
# 
# bumblebees %>%
#   ggplot(aes(x=head_width, as.numeric(ITlength_mm)))+
#   geom_point()+
#   theme_classic()
# 
# ### Xylocopas
# xylocopa <- traits %>%
#   filter(genus =="Xylocopa")
# 
# linealmodelxylocopa <- lm(otherbees$ITlength_mm~otherbees$head_width)
# summary(linealmodelxylocopa)
# 
# xylocopa %>%
#   ggplot(aes(x=ITlength_mm, as.numeric(head_width)))+
#   geom_point()+
#   theme_classic()
# 
# ### Other bees
# otherbees <- traits %>%
#   filter(.,genus !="Bombus" & genus!="Xylocopa")
# 
# linealmodelotherbees <- lm(otherbees$head_width~otherbees$ITlength_mm)
# summary(linealmodelotherbees)
# 
# otherbees %>%
#   ggplot(aes(x=as.numeric(head_width), ITlength_mm))+
#   geom_point(aes(color=genus))+
#   theme_classic()


################ create a final dataset for graphs
# add IT
generaldata2 <- male %>% left_join(nITperbee, by=(c("bee")))

## lasioglossum hitchemsi-weemsi we say that has the same IT as l. mitchelli
generaldata2$IT_mm[which(generaldata2$bee=="Lasioglossum_hitchensi_weemsi")]<-"1.0013641"
generaldata2$IT_mm <- as.numeric(generaldata2$IT_mm)

# add flower traits
generaldata3 <- generaldata2 %>% left_join(flowerstotal, by=(c("plant_gs")))


## check family data

generaldata3$bee_family[generaldata3$bee_genus=="Triepeolus"]<-"Apidae"
generaldata3$bee_family[generaldata3$bee_genus=="Ceratina"]<-"Apidae"
generaldata3$bee_family[generaldata3$bee_genus=="Nomada"]<-"Apidae"
generaldata3$bee_family[generaldata3$bee_genus=="Lasioglossum"]<-"Halictidae"
generaldata3$bee_family[generaldata3$bee_genus=="Hylaeus"]<-"Colletidae"
generaldata3$bee_family[generaldata3$bee_genus=="Dufouria"]<-"Halictidae"
generaldata3$bee_family[generaldata3$bee_genus=="Coelioxys"]<-"Megachilidae"

# remove other bugs
generaldata3<-generaldata3[-which(generaldata3$bee_genus == "Anacrabro"),]
#check to see that all remaining records have a family
beeswithoutfamily <- dplyr::filter(generaldata3, is.na(bee_family))
table((beeswithoutfamily$bee_genus))#if it's 0 is good

# Calculate bee proboscis length and body size
Out <- ITconverter(IT = generaldata3$IT_mm, family = generaldata3$bee_family)

generaldata3$body_mass <- Out$body_mass
generaldata3$tongue_length.tongue <- Out$tongue_length.tongue # tongue lengths all estimated?


#drop some yucky columns
# generaldata<-generaldata %>% select %>% rename("plant_gs"="genus_species")

# ## time variables
# 
# generaldata$boutstart<- as.POSIXct(strptime(generaldata$boutstart, format="%Y-%m-%d %H:%M:%OS"))
# generaldata$boutend= as.POSIXct(strptime(generaldata$boutend, format="%Y-%m-%d %H:%M:%OS"))
# 
# generaldata<-generaldata %>% group_by_all() %>% summarize(midbout=mean(c(boutstart, boutend), na.rm=T))

## drop unused plots, all male bees, doubtful interactions, interactions without data in tongue or depth

# New variables
# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends


generaldata <- generaldata3 %>%
  dplyr::filter(bee_sex != "M" & site!="Featherbed"& site!="D&R Greenway" & keep!="D" & !is.na(sampling_round) &!is.na(tongue_length.tongue) & !is.na(depth) ) %>% 
  mutate(difference=tongue_length.tongue-depth) %>%
  mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm*1.29)) %>%
  mutate(IT_improved=if_else((bee_genus == "Bombus"), IT_mm*0.78, IT_mm)) %>% 
  mutate(beewider=if_else(IT_improved>width, "true", "false")) %>% 
  mutate(newdifference=if_else(beewider== "true", difference, 0)) %>%## Assume differences of 0 for small bees that can crawl in (in a different variable, no problem with the data) 
  mutate(newdepth=if_else(beewider== "true", depth, 0)) 

## database of michael separate by bee
datameasures <-  generaldata %>%
  group_by(bee) %>% 
  summarize(abundance=n(),tongue=mean(tongue_length.tongue))

write.table(datameasures,"C:\\Users\\535388\\OneDrive - UMONS\\R folders\\floral_traits\\Table_bee_sp.txt")

#### How many bee species are in the dataset?
sum(table(datameasures$abundance)) # 128


## database of michael separate by plant
datameasuresflowers <-  generaldata %>%
  group_by(plant_gs) %>% 
  summarize(abundance=n(),depth=mean(depth))

hist(datameasures$tongue,breaks=26)

write.table(datameasuresflowers,"C:\\Users\\535388\\OneDrive - UMONS\\R folders\\floral_traits\\Table_plant_sp.txt")


mean(generaldata$difference)

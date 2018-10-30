
require(devtools)
library(BeeIT)
library(tidyverse)

############# Read IT database
traits<-read.csv("data/wlab_db_5-31-18_3-21 PM_specimen_level_traits.csv")
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
traits$bee[which(traits$bee=="Coelioxis_alternata")]<-"Coelioxys_alternata"
traits$bee[which(traits$bee=="Coelioxis_octodentata")]<-"Coelioxys_octodentata"
traits$bee[which(traits$bee=="Coelioxis_obtusiventris")]<-"Coelioxys_obtusiventris"
traits$bee[which(traits$bee=="Coelioxis_sayi")]<-"Coelioxys_sayi"
traits$bee[which(traits$bee=="Holpitis_spoliata")]<-"Hoplitis_spoliata"
traits$bee[which(traits$bee=="Holpitis_producta")]<-"Hoplitis_producta"
traits$bee[which(traits$bee=="Lasioglossum_coerulum")]<-"Lasioglossum_coeruleum"
traits$bee[which(traits$bee=="Lasioglossum_cressonni")]<-"Lasioglossum_cressonii"
traits$bee[which(traits$bee=="Lasioglossum_nigroviride")]<-"Lasioglossum_nigroviridae"
traits$bee[which(traits$bee=="Melissodes_tridonis")]<-"Melissodes_trinodis"
traits$bee[which(traits$bee=="Melissodes_trinodus")]<-"Melissodes_trinodis"
traits$bee[which(traits$bee=="Megachile_xylocopioides")]<-"Megachile_xylocopoides"



## Create a table grouping per bee
 
## Calculate mean IT per each previous group
nITperbee <- group_by(traits, bee) %>% summarize(IT_mm=mean(ITlength_mm))


######### Read male bee dataset
male<-read.csv("data/2016_male_bee_dataset.csv")
# #### since we are not separing by sex, is this necessary???
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
male$bee[which(male$bee=="Lasioglossum_birkmanni")]<-"Lasioglossum_birkmannii"
male$bee[which(male$bee=="Coelioxys_octodentatus")]<-"Coelioxys_octodentata"
male$bee[which(male$bee=="Coelioxys_alternatus")]<-"Coelioxys_alternata"




## database of traits. separate by bee
nITperbee3 <- group_by(traits, bee) %>% summarize(measured=n())

## database of michael separate by bee
group2 <- group_by(male, bee)
michaeldata <- summarize(group2, abundance=n())



maleIT <- michaeldata %>% left_join(nITperbee3, by=(c("bee")))

#### How many species are in the dataset?
sum(table(maleIT$abundance)) # 160
### How many species do we have IT data?
sum(table(maleIT$measured))  # 147


############# Read new floral traits database
newfloraltraits<-read.table("data/flowers.txt",header = T)
group <- group_by(newfloraltraits, Species)

flowersmeasured <- summarize(group,depth=mean(depth),width=mean(width))

############# Read floral traits database
floraltraits<-read.csv("data/floraltraits.csv")
floraltraits <- data.frame(floraltraits$genus_species,floraltraits$depth,floraltraits$width)
names(floraltraits) <- c("Species","depth","width")

## join two floral databases
flowerstotal <- dplyr::bind_rows(floraltraits, flowersmeasured)
names(flowerstotal) <- c("plant_gs","depth","width")

## add the data to the bee dataset

male$plant_gs <- paste(male$plant_genus,male$plant_species,sep="_")

male$plant_gs[which(male$plant_gs=="Eutrochium maculatum_")]<-"Eutrochium_maculatum"

group <- group_by(male, plant_gs)
michaeldata <- summarize(group, abundance=n())
michaelflowers <- michaeldata %>% left_join(flowerstotal, by=(c("plant_gs")))

#### How many species are in the dataset?
sum(table(michaelflowers$abundance)) # 112
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


################ create a final dataset for graphs
# add IT
generaldata <- male %>% left_join(nITperbee, by=(c("bee")))

## lasioglossum hitchemsi-weemsi we say that has the same IT as l. mitchelli
generaldata$IT_mm[which(generaldata$bee=="Lasioglossum_hitchensi_weemsi")]<-"1.0013641"
generaldata$IT_mm <- as.numeric(generaldata$IT_mm)

# add flower traits
generaldata <- generaldata %>% left_join(flowerstotal, by=(c("plant_gs")))


## check family data

generaldata$bee_family[generaldata$bee_genus=="Triepeolus"]<-"Apidae"
generaldata$bee_family[generaldata$bee_genus=="Ceratina"]<-"Apidae"
generaldata$bee_family[generaldata$bee_genus=="Nomada"]<-"Apidae"
generaldata$bee_family[generaldata$bee_genus=="Lasioglossum"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_genus=="Hylaeus"]<-"Colletidae"
generaldata$bee_family[generaldata$bee_genus=="Dufouria"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_genus=="Coelioxys"]<-"Megachilidae"

# remove other bugs
generaldata<-generaldata[-which(generaldata$bee_genus == "Anacrabro"),]
#check to see that all remaining records have a family
beeswithoutfamily <- dplyr::filter(generaldata, is.na(bee_family))
table(droplevels(beeswithoutfamily$bee_genus))

# Calculate bee proboscis length and body size
Out <- ITconverter(IT = generaldata$IT_mm, family = generaldata$bee_family)

generaldata$body_mass <- Out$body_mass
generaldata$tongue_length.tongue <- Out$tongue_length.tongue


#drop some yucky columns
# generaldata<-generaldata %>% select %>% rename("plant_gs"="genus_species")

# ## time variables
# 
# generaldata$boutstart<- as.POSIXct(strptime(generaldata$boutstart, format="%Y-%m-%d %H:%M:%OS"))
# generaldata$boutend= as.POSIXct(strptime(generaldata$boutend, format="%Y-%m-%d %H:%M:%OS"))
# 
# generaldata<-generaldata %>% group_by_all() %>% summarize(midbout=mean(c(boutstart, boutend), na.rm=T))

## drop unused plots, all male bees, doubtful interactions, interactions without data in tongue or depth

generaldata <- droplevels(dplyr::filter(generaldata, bee_sex != "M" & site!="Featherbed"& site!="D&R Greenway" & keep!="D" & !is.na(sampling_round) & !is.na(depth)&!is.na(tongue_length.tongue)))


# New variables
generaldata$difference <- generaldata$tongue_length.tongue-generaldata$depth
# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends
generaldata<-generaldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72))
generaldata<-generaldata %>% mutate(beewider=if_else(IT_improved>width, "true", "false"))

## database of michael separate by bee
datameasures <-  generaldata %>%
  group_by(bee) %>% 
  summarize(abundance=n())

#### How many bee species are in the dataset?
sum(table(datameasures$abundance)) # 128



## Assume differences of 0 for small bees that can crawl in (in a different variable, no problem with the data)
generaldata <- generaldata%>% 
  mutate(newdifference=if_else(beewider== "true", difference, 0))

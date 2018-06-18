

library(plyr)
library(dplyr)

############# Read IT database
traits<-read.csv("data/wlab_db_5-31-18_3-21 PM_specimen_level_traits.csv")
## Create vector of complete bee species name pasting genus and species
traits$bee <- paste(traits$genus, traits$species, sep = "_")

## There are NULLs that promote error. Remove them
traits<-traits[traits$ITlength_mm!="NULL",]
## Clean sex data. There are many classes
traits<-traits[-which(traits$sex == "unknown"|traits$sex == "NULL"),]
traits$sex[which(traits$sex=="queen"|traits$sex=="worker")]<-"female"
traits$sex <- droplevels(traits$sex)
## Variable IT distance is still a factor. Convert to numeric
traits$ITlength_mm <- as.numeric(as.character(traits$ITlength_mm))

## Create a table grouping per bee and sex
group <- group_by(traits, bee)
## Calculate mean IT per each previous group
nITperbee <- summarize(group, IT_mm=mean(ITlength_mm))


######### Read male bee dataset
male<-read.csv("data/2016_male_bee_dataset.csv")
## Clean sex data. There are many classes
male<-male[-which(male$bee_sex == "Unidentified"|male$bee_sex == ""),]
male$bee_sex[which(male$bee_sex=="m")]<-"M"
male$bee_sex <- as.character(male$bee_sex)

## Change names of variable to be the same as the traits dataframe.
## Change sex names also
male$bee_sex[which(male$bee_sex=="M")]<-"male"
male$bee_sex[which(male$bee_sex=="F")]<-"female"
male$sex <- male$bee_sex


### could get some matches. need to clean up traits table a lot
#nymphaearum=oceanicum
#hichensi=weemsi
#most melissodes spp. might have incorrect gender right now (e.g. disponsa instead of the correct disponsus)

## database of IT  separate by bee and sex
group <- group_by(traits, bee, sex)
nITperbee2 <- summarize(group, measured=n())

## database of michael separate by bee and sex
group <- group_by(male, bee, bee_sex)
michaeldata <- summarize(group, abundance=n())
maleIT <- michaeldata %>%rename(sex=bee_sex) %>%  left_join(nITperbee2, by=(c("bee","sex")))

sum(table(maleIT$measured)) #147

## mow the same but not separing by sex
## database of traits. separate by bee and sex
group <- group_by(traits, bee)
nITperbee3 <- summarize(group, measured=n())

## database of michael separate by bee and sex
group <- group_by(male, bee)
michaeldata <- summarize(group, abundance=n())
maleIT <- michaeldata %>% left_join(nITperbee3, by=(c("bee")))



#### How many species are in the dataset?
sum(table(maleIT$abundance)) # 164
### How many species do we have IT data?
sum(table(maleIT$measured))  # 122



############# Read floral traits database
floraltraits<-read.csv("data/floraltraits.csv")

# create column with plant genus and species together
male$genus_species <- paste(male$plant_genus, male$plant_species, sep = "_")

group <- group_by(floraltraits, genus_species)
flowersmeasured <- summarize(group, measured=n())

group <- group_by(male, genus_species)
michaeldata <- summarize(group, abundance=n())
michaelflowers <- michaeldata %>% left_join(flowersmeasured, by=(c("genus_species")))

#michaelflowers<-michaelflowers[michaelflowers$measured!="NA",]

#### How many species are in the dataset?
sum(table(michaelflowers$abundance)) # 113
### How many species do we have floral traits data?
sum(table(michaelflowers$measured))  # 44


sum(whichflowers$abundance[complete.cases(whichflowers$abundance)])


# Which species we lack data, and how many visits do they have
flowersnodata <- michaeldata %>% anti_join(flowersmeasured, by=(c("genus_species")))
# from which species do we have more than 80 visits and worth measuring
dplyr::filter(flowersnodata, abundance > 80)

# 
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


################ create a final dataset for graphs
# add IT
generaldata <- male %>% left_join(nITperbee, by=(c("bee")))

# add flower traits
generaldata <- generaldata %>% left_join(floraltraits, by=(c("genus_species")))

#drop some yucky columns
generaldata<-generaldata %>% select (-c("sex","plant_code.x", "plant_species.x", "plant_genus.x", "plant_code.y", "plant_genus.y", "plant_species.y")) %>% rename("plant_gs"="genus_species")

### add bee families
# traittab<-read.csv("data/wlab_db_5-31-18_3-21 PM_species_traits.csv")
# withfam<-left_join(generaldata, traittab %>% select(genus, species, Family), by=c("bee_genus"="genus","bee_species"="species" ))
# 

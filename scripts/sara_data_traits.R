

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
group <- group_by(traits, bee, sex)
## Calculate mean IT per each previous group
nITperbee <- summarize(group, measured=n())


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

#so the deal here is that there are multiple "right"values per bee, so this join results in for each unique ID in the male bee dataset, 0-[# measured specimens] matches on the right. I think there might be a way get merge to default to just the first match, but I used your "meanITpergroup" d.f. instead

#maleIT<- merge(male, traits, by=("bee"), all.x=TRUE, all.y=F, sort=FALSE)


## database of IT  separate by bee and sex
group <- group_by(traits, bee, sex)
nITperbee <- summarize(group, measured=n())

## database of michael separate by bee and sex
group <- group_by(male, bee, sex)
michaeldata <- summarize(group, abundance=n())
maleIT <- michaeldata %>% left_join(nITperbee, by=(c("bee","sex")))

sum(table(maleIT$measured))

## mow the same but not separing by sex
## database of traits. separate by bee and sex
group <- group_by(traits, bee)
nITperbee <- summarize(group, measured=n())

## database of michael separate by bee and sex
group <- group_by(male, bee)
michaeldata <- summarize(group, abundance=n())
maleIT <- michaeldata %>% left_join(nITperbee, by=(c("bee")))



#### How many species are in the dataset?
sum(table(maleIT$abundance)) # 166
### How many species do we have IT data?
sum(table(maleIT$measured))  # 123



############# Read floral traits database
floraltraits<-read.csv("data/floraltraits.csv")

# create column with plant genus and species together
male$genus_species <- paste(male$plant_genus, male$plant_species, sep = "_")

group <- group_by(floraltraits, genus_species)
flowersmeasured <- summarize(group, measured=n())

group <- group_by(male, genus_species)
michaeldata <- summarize(group, abundance=n())
whichflowers <- michaeldata %>% left_join(flowersmeasured, by=(c("genus_species")))

whichflowers<-whichflowers[whichflowers$measured!="NA",]

#### How many species are in the dataset?
sum(table(whichflowers$abundance)) # 113
### How many species do we have floral traits data?
sum(table(whichflowers$measured))  # 44


sum(whichflowers$abundance[complete.cases(whichflowers$abundance)])
whichflowers$measured
whichflowers$genus_species


# Which species we lack data, and how many visits do they have
flowersnodata <- michaeldata %>% anti_join(flowersmeasured, by=(c("genus_species")))

hist(flowersnodata$abundance)
table(flowersnodata$abundance)
sort(flowersnodata)

dplyr::filter(flowersnodata, abundance > 80)


#### bee head size
names(traits)
## There are NULLs that promote error. Remove them
traits<-traits[traits$head_width!="NULL",]
## Variable IT distance is still a factor. Convert to numeric
traits$head_width <- as.numeric(as.character(traits$head_width))

## Create a table grouping per bee
group <- group_by(traits, bee)
## Calculate mean IT per each previous group
nheadwidthperbee <- summarize(group, measured=n())

group <- group_by(male, bee)
michaeldata <- summarize(group, abundance=n())
maleIT <- michaeldata %>% left_join(nheadwidthperbee, by=(c("bee")))

maleIT$measured

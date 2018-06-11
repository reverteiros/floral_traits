
unloadNamespace("vegan")
unloadNamespace("merTools")
unloadNamespace("blme")
unloadNamespace("arm")
unloadNamespace("lme4")
unloadNamespace("phytools")
unloadNamespace("ape")
unloadNamespace("MASS")
library(vegan)
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)

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
meanITpergroup <- summarize(group, ITlength_mm = mean(ITlength_mm))


######### Read male bee dataset
male<-read.csv("data/2016_male_bee_dataset.csv")
## Clean sex data. There are many classes
male<-male[-which(male$bee_sex == "Unidentified"|male$bee_sex == ""),]
male$bee_sex[which(male$bee_sex=="m")]<-"M"

#wasn't working ebefore because attempting to introduce new levels to a factor variable. I actually don't know how to do that in a general sense... in a limited # of factors you can say that levels(variable)<-c("whatever","you", "want"). I just convert to character though

male$bee_sex <- as.character(male$bee_sex)
## Change names of variable to be the same as the traits dataframe.
## Change sex names also
#########################################################################
#### problems start here:-->

male$bee_sex[which(male$bee_sex=="M")]<-"male"
male$bee_sex[which(male$bee_sex=="F")]<-"female"
male$sex <- male$bee_sex
head(male)

#so the deal here is that there are multiple "right"values per bee, so this join results in for each unique ID in the male bee dataset, 0-[# measured specimens] matches on the right. I think there might be a way get merge to default to just the first match, but I used your "meanITpergroup" d.f. instead

#maleIT<- merge(male, traits, by=("bee"), all.x=TRUE, all.y=F, sort=FALSE)

maleIT<-male %>% left_join(meanITpergroup, by=(c("bee", "sex")))

?as.POSIXct


# Convert boutstart and boutend to time data

maleIT$boutstart<- as.POSIXct(strptime(maleIT$boutstart, format="%Y-%m-%d %H:%M:%OS"))
maleIT$boutend= as.POSIXct(strptime(maleIT$boutend, format="%Y-%m-%d %H:%M:%OS"))

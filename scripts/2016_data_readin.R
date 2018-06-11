### read in, merge, and clean 2016 male bee project data
# also grab sociality for all spp. from winfree SQL db
#current version is from dpc rare_ES repo 9/25/17

#in case I've run other stuff unload packages
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

library(lubridate)
library(tidyverse)

  
#read in and clean data
#first determinations sheet (uniqueIDs to keep)
dets<-read.csv("data/determinations.csv")
#drop rows that are missing uniqueID
dets<-dets[dets$uniqueID!="",]
#fix dumb spellling errors!!!!
dets$genus[dets$genus=="Lasioglosssum"]<-"Lasioglossum"

#second, pinning log (drop unmatched uniqueIDs)
pin<-read.csv("data/pinning_data.csv")
pin$uniqueID<-paste("MR", pin$uniqueID, sep="")
# head(pin)
pin$GEN.SPE[which(pin$GEN.SPE=="nectary")]<-"NECTARY"
nodet<-pin[-which(pin$uniqueID%in%dets$uniqueID | pin$basicID == "Apis"| pin$basicID== "fly" | pin$basicID== "JUNKROW" | pin$basicID== "wasp"| pin$basicID== "non-bee"),]
nodet<-nodet %>% 
    dplyr::group_by(general_ID) %>% 
    dplyr::summarize(remaining=n())

#43 BEES without determinations... some bombus id'd when pinned could be entered. 

# write.csv(nodet, "data/no_det.csv")

#third read in the traits
traits<-read.csv("data/traits.csv")


#write.csv(nodet, "data/bees_without_determinations_20170711.csv")
### check how many records are in dets but not in pinning (should be 0)

 # length(dets$uniqueID)
 # length(dets$uniqueID %in% pin$uniqueID)
#same, 20348
XerDat<-droplevels(merge(dets, pin, all.x = T, all.y = T, by="uniqueID"))
# drop confirmed non-bee
#XerDat[4549,"basicID"]<-"wasp"
XerDat<-XerDat %>%  mutate(basicID=replace(basicID,genus== "chrisidid"|genus=="crabronid"|genus=="Crabro","wasp"))  

XerDat<-XerDat[XerDat$basicID != "Apis"&XerDat$genus!="Apis" & XerDat$basicID!= "fly"& XerDat$basicID!= "JUNKROW" & XerDat$basicID!= "wasp" & XerDat$basicID!= "spider" & XerDat$basicID!= "non-bee"&XerDat$genus!="wasp",]

XerDat[which(XerDat$GEN.SPE=="TRI PRA "), "GEN.SPE"]<-"TRI PRA"

### weird dplyr thing: these don't return identical objects... there's 200ish NA rows that dplyr ignores.
# XerDat[XerDat$site_full=="",]
# XerDat %>% filter(site_full=="")

#drop values without sites, NA rows
XerDat<-XerDat%>%filter(site_full!="")%>%droplevels()

#collapse affinis
XerDat$species<-as.character(XerDat$species)
XerDat$species[grep("aff", XerDat$species)] <- "affinis_modestus"
XerDat$species[grep("modestus", XerDat$species)] <- "affinis_modestus"
XerDat$species[XerDat$species=="disponsus"]<-"desponsus"
XerDat$species[XerDat$species=="portera"]<-"porterae"
XerDat[XerDat$uniqueID=="MR15731","genus"]<-"Hylaeus"
XerDat[XerDat$uniqueID=="MR15731","species"]<-"sp"



XerDat$species<-as.factor(XerDat$species)
# levels(XerDat$species)
# str(XerDat)

#make dattime objects for sampling bout start and end times. Eventually will want to add sampling rounds to this, but that will wait
# levels(XerDat$date)
XerDate<-XerDat%>%
    mutate(boutstart=paste(textdate, start), boutend=paste(textdate, end))
# XerDate[c(1:5), "boutstart"]

XerDate$startdatetime<- as.POSIXct(strptime(XerDate$boutstart, format="%d %B, %Y %R"))
XerDate$enddatetime= as.POSIXct(strptime(XerDate$boutend, format="%d %B, %Y %R"))
XerDate$starttime<- as.POSIXct(strptime(XerDate$start, format="%R"))
XerDate$endtime<- as.POSIXct(strptime(XerDate$end, format="%R"))
XerDate$sround<-as.POSIXct(strptime(XerDate$textdate, format="%d %B, %Y"))

#add sampling rounds to file too
srounds<-read.csv("data/dates_for_rounds.csv")
srounds<-srounds%>%mutate(sround=paste(year, month, day, sep="_"))
srounds$sround<- as.POSIXct(strptime(srounds$sround, format="%Y_%B_%e"))

XerDate<-left_join(XerDate, srounds, by=c("sround", "site_full"))

## convert times that are wrong based on am/pm errors (could be some 6's and 7s that are ambiguous)
XerDate[which(XerDate$starttime<strptime("06:30:00", format="%R")),"starttime"]<-XerDate[which(XerDate$starttime<strptime("06:30:00", format="%R")),"starttime"]+hours(12)
XerDate[which(XerDate$endtime<strptime("07:00:00", format="%R")),"endtime"]<-XerDate[which(XerDate$endtime<strptime("07:00:00", format="%R")),"endtime"]+hours(12)


#Two days where pm sampling didn't get turned over above:
XerDate[which(XerDate$site_full %in% c("Fox Hill", "IAS")&(XerDate$sround =="2016-06-29 GMT"|XerDate$sround=="2016-06-09 GMT")&XerDate$starttime<strptime("7:10:00", format="%R")), "endtime"]<-XerDate[which(XerDate$site_full %in% c("Fox Hill", "IAS")&(XerDate$sround =="2016-06-29 GMT"|XerDate$sround=="2016-06-09 GMT")&XerDate$starttime<strptime("7:10:00", format="%R")), "endtime"]+hours(12)

XerDate[which(XerDate$site_full %in% c("Fox Hill", "IAS")&(XerDate$sround =="2016-06-29 GMT"|XerDate$sround=="2016-06-09 GMT")&XerDate$starttime<strptime("7:10:00", format="%R")), "starttime"]<-XerDate[which(XerDate$site_full %in% c("Fox Hill", "IAS")&(XerDate$sround =="2016-06-29 GMT"|XerDate$sround=="2016-06-09 GMT")&XerDate$starttime<strptime("7:10:00", format="%R")), "starttime"]+hours(12)


#standardize names

XerDate<-XerDate[XerDate$species!="sp",]

XerDate$species <-as.character(XerDate$species)
XerDate$genus <- as.character(XerDate$genus)

XerDate$species[XerDate$genus=="Lasioglossum" & XerDate$species== "ilinoense" ] <- "illinoense"

XerDate$species[XerDate$genus=="Melissodes" & XerDate$species== "bimaculata" ] <- "bimaculatus"
XerDate$species[XerDate$genus=="Melissodes" & XerDate$species== "denticulata" ] <- "denticulatus"
XerDate$species[XerDate$genus=="Melissodes" & XerDate$species== "druriella" ] <- "druriellus"
XerDate$species[XerDate$genus=="Melissodes" & XerDate$species== "illata" ] <- "illatus"
XerDate$species[XerDate$genus=="Melissodes" & XerDate$species== "subillata" ] <- "subillatus"
XerDate$species[XerDate$genus=="Melissodes" & XerDate$species== "desponsa" ] <- "desponsus"
XerDate$species[XerDate$genus=="Melissodes" & XerDate$species== "nivea" ] <- "niveus"
XerDate$species[XerDate$genus=="Lasioglossum" & XerDate$species== "paradmiratum" ] <- "paradmirandum"
XerDate$species[XerDate$genus=="Megachile" & XerDate$species== "rotunda" ] <- "rotundata"
XerDate$genus<-gsub("Hylaues", "Hylaeus", XerDate$genus)

XerDate$genus<-gsub("Hylaues ", "Hylaeus", XerDate$genus)

XerDate$genus<-gsub("Hylaeus ", "Hylaeus", XerDate$genus)


#these are lumping taxa

#XerDate$species[XerDate$genus == "Halictus" & c(XerDate$species=="ligatus"|XerDate$species=="poeyi")] <- "ligatus_poeyi"
#XerDate$species <- gsub("dupla_sensu_lato","calcarata_dupla_mikmaqi",XerDate$species) # remove _ish
XerDate$species[XerDate$genus=="Hylaeus" & c(XerDate$species=="affinis" |XerDate$species=="modestus" |XerDate$species=="affinis_like" | XerDate$species== "affinis/modestus"| XerDate$species== "modestus/affinis"| XerDate$species== "modestus_like"|XerDate$species=="species A") ] <- "affinis_modestus"
XerDate$species[XerDate$genus=="Heriades" & XerDate$species=="carinata"]<- "carinatus"
XerDate$species[XerDate$genus=="Heriades" & XerDate$species=="variolosa"]<- "variolosus"
#XerDate$species[XerDate$genus=="Lasioglossum" & c(XerDate$species=="pilosum" |XerDate$species=="leucomum"|XerDate$species=="leucocomum")]<- "leucocomum_pilosum" #having a very quirky problem where I am getting in error when I add "pilosum_leucomom". Remove "_" and it works. This is not a problem for Ceratina though
XerDate$species[XerDate$genus=="Lasioglossum" & c(XerDate$species=="hitchensi" |XerDate$species=="weemsi"|XerDate$species=="hitchensi or weemsi")]<- "hitchensi_weemsi"
XerDate$species[XerDate$genus=="Nomada" & c(XerDate$species=="sayi" |XerDate$species=="illinoensis")]<- "sayi_illinoensis"
XerDate$species <- gsub("pilosum_leucomum", "leucocomum_pilosum", XerDate$species)
#########################################
### CONTROVERSIAL MOVE???###########
########################################
#Taxonomy decision... analyze all the cfs as if they weren't cf
XerDate$species <- gsub(" cf.*", "", XerDate$species)

#and add sociality

XerDate<-left_join(XerDate, traits, by=c("genus"="genus", "species"="species"))%>%
    dplyr::select(c(1:46, Sociality))
# str(XerDate)

#add sociality for concatenated spp complexes
traits[traits$species=="leucocomum", c("genus","Sociality")]
XerDate$Sociality[which(XerDate$species=="affinis_modestus")]<-'Solitary'
XerDate$Sociality[which(XerDate$species=="mikmaqi")]<-'fac_social'
XerDate$Sociality[which(XerDate$species=="hitchensi_weemsi")]<-as.factor("Eusocial")
XerDate$Sociality[which(XerDate$species=="leucocomum_pilosum")]<-as.factor("Eusocial")
XerDate$Sociality[which(XerDate$species=="nymphaearum")]<-as.factor("Eusocial")
XerDate$Sociality[which(XerDate$species=="persimilis")]<-as.factor("Eusocial")
XerDate$Sociality[which(XerDate$species=="wilmattae")]<-as.factor("Solitary")
XerDate$Sociality[which(XerDate$species=="abrupta")]<-as.factor("Solitary")
XerDate$Sociality[which(XerDate$species=="birkmanni")]<-as.factor("Solitary")
XerDate$Sociality[which(XerDate$species=="fedorica")]<-as.factor("Solitary")
XerDate$Sociality[which(XerDate$genus=="Lithurgus")]<-as.factor("Solitary")
XerDate$Sociality[which(XerDate$genus=="Heriades")]<-as.factor("Solitary")

#clean up plant names
XerDate[which(XerDate$GEN.SPE=="Nectary"), "GEN.SPE"]<-"NECTARY"
XerDate$GEN.SPE <- gsub("MEL OFF ","MEL OFF",XerDate$GEN.SPE) 
XerDate$GEN.SPE <- gsub("POT MIL","POT ARG",XerDate$GEN.SPE)
#Add plant taxonomy
plants<-read.csv("data/plant_names.csv")[,c(1:4)]

names(plants)<-c("plant_code", "plant_genus", "plant_species", "plant_family")

# #check to see what percent of plant species are considered native by USDA
# plants<-plants %>% mutate(`Scientific.Name`=paste(plant_genus, plant_species, sep=" "))
# usda<-read.csv("data/USDA.csv")

# isnative<-left_join(plants, usda)
# 
# table(isnative$N.I)
# Invasive==31, Native==67, NI==1

XerDate<-left_join(XerDate, plants, by=c("GEN.SPE"="plant_code"))
XerDate[XerDate$species=="", "species"]<-"sp"
XerDate<-XerDate%>% mutate(bee=paste(XerDate$genus, XerDate$species, sep="_")) %>% 
    dplyr::select(uniqueID,bee_sex=M.F, bee_genus=genus, bee_species=species, bee=bee, plant_code=GEN.SPE, plant_species, plant_genus, plant_family, sociality=Sociality,  site=site_full, sampling_round=round, sday=sround, boutstart=starttime, boutend=endtime, keep, Bombus,latitude, longitude, det=det_by, TN,taxonomy_notes=notes, collector=collector_full,basicID, fdate=textdate, reason_to_drop, pinner, )
#here's a dumb one: update sex
XerDate[which(XerDate$bee_sex=="m"), "bee_sex"]<-"M"

##############################
### clean up det column #####
XerDate$det<-as.character(XerDate$det)
XerDate[XerDate$det=="AM", "det"]<-"A. Matthews"
XerDate[XerDate$det=="CK", "det"]<-"C. Kanterman"
XerDate[XerDate$det=="JC", "det"]<-"J. Criscione"
XerDate[XerDate$det=="JG", "det"]<-"J. Gibbs"
XerDate[XerDate$det=="Joel", "det"]<-"J. Gardner"
XerDate[XerDate$det=="KH", "det"]<-"K. Himmler"
XerDate[XerDate$det=="KL", "det"]<-"K. Londono"
XerDate[XerDate$det=="MR", "det"]<-"M. Roswell"
XerDate[XerDate$det=="RL", "det"]<-"R. Letchinger"
XerDate[XerDate$det=="SV", "det"]<-"S. Villanueva"
XerDate[XerDate$det=="TB", "det"]<-"T. Bennet"
XerDate[XerDate$det=="TH", "det"]<-"T. Harrison"
<<<<<<< HEAD

write.csv(XerDate,"data/2016_male_bee_dataset.csv", row.names = F)
=======
XerDate[XerDate$det=="", "det"]<-"J. Gardner"

# write.csv(XerDate,"data/2016_male_bee_dataset.csv", row.names = F)
>>>>>>> b7f348dc4f5e27d2680bba6b2e2c54d951faef97

# phen<-XerDate %>% group_by(plant_genus, plant_species, sround) %>% summarize(abund=n())
# phen %>% filter(sum(abund)>100) %>%  ggplot(aes(sround, reorder(plant_species, as.numeric(sround)), size=abund))+geom_point()+theme_classic()+theme(axis.text.x=element_text(angle=90))
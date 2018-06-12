
require(devtools)
library(BeeIT)

source("scripts/sara_data_traits.R")

## we need family data
generaldata$bee_family <- generaldata$bee_genus
generaldata$bee_family <- as.character(generaldata$bee_family)

generaldata$bee_family[generaldata$bee_family=="Xylocopa"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Triepeolus"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Stelis"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Sphecodes"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_family=="sand wasp"]<-"Others"
generaldata$bee_family[generaldata$bee_family=="Ptilothrix"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Pseudoanthidium"]<-"Others"
generaldata$bee_family[generaldata$bee_family=="Osmia"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Nomada"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Melissodes"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Megachile"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Lithurgus"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Lasioglossum"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_family=="Hoplitis"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Heriades"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Halictus"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_family=="Hylaeus"]<-"Colletidae"
generaldata$bee_family[generaldata$bee_family=="Dufouria"]<-"Others"
generaldata$bee_family[generaldata$bee_family=="Coelioxys"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Ceratina"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Calliopsis"]<-"Andrenidae"
generaldata$bee_family[generaldata$bee_family=="Bombus"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Augochloropsis"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_family=="Augochlorella"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_family=="Augochlora"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_family=="Anthophora"]<-"Apidae"
generaldata$bee_family[generaldata$bee_family=="Agapostemon"]<-"Halictidae"
generaldata$bee_family[generaldata$bee_family=="Anacrabro"]<-"Others"
generaldata$bee_family[generaldata$bee_family=="Andrena"]<-"Andrenidae"
generaldata$bee_family[generaldata$bee_family=="Anthidiellum"]<-"Megachilidae"
generaldata$bee_family[generaldata$bee_family=="Anthidium"]<-"Megachilidae"

# Remove other bugs
generaldata<-generaldata[-which(generaldata$bee_family == "Others"),]
table(generaldata$bee_family)

# Calculate bee proboscis length and body size
Out <- ITconverter(IT = generaldata$IT_mm, family = generaldata$bee_family)

generaldata$body_mass <- Out$body_mass
generaldata$tongue_length.tongue <- Out$tongue_length.tongue


## Plots only for the plants from which we have data on traits

subsetgeneraldata <- generaldata[which(generaldata$depth != "NA"),]


plot(subsetgeneraldata$tongue_length.tongue~subsetgeneraldata$depth,ylab="Tongue length (mm)", xlab="Flower depth (mm)")

hist(subsetgeneraldata$tongue_length.tongue, xlab="Tongue length (mm)",main="")
hist(subsetgeneraldata$depth, xlab="Flower depth (mm)",main="")

## this script is doing a lot of small little things...like getting the total number of species for results, etc.
# load raw data
tongue<-read.csv('data/bees_it_tongue.csv',header=T)
visitdata<-read.csv('data/12-15_specimens.csv',header=T)
ftraits<-read.csv('data/fullfmeas.csv',header=T)

#plot of raw data
quartz()
plot(joes_data$depth, joes_data$meantongue, xlab="Corolla Depth (mm)", ylab="Tongue Length (mm)")

#histogram of corolla depth
hist(ftraits$depth, main="", xlab="Corolla Depth (mm)", 
     ylab="Number of Species")
#histogram of log(corolla depth)
logftable<-log(ftraits$depth)
hist(logftable, main="", xlab="log(corolla depth)", ylab="Number of Species")

#histogram of log(tongue length)
hist(log(tongue$meantongue), main="", xlab="log(tongue length)", ylab="Number of Species")
#histogram of tongue length
hist(tongue$meantongue, main="", xlab="Mean Tongue Length (mm)", ylab="Number of Species")

###figuring out how many hours we spent sampling
measvisits<-read.csv('data/measvisits.csv')
#need to get year_site_treatment_round
ssc<-paste(measvisits$sc, measvisits$round, sep = "_")
sitedate<-length(unique(ssc))
#now sitedate is a number that is equal to the number of unique year_site_treatment_round
#I am going to assume that there are only 2 transects in a treatment/control ... so I'm ignoring natcig which has 4
((2*sitedate)*2*10)/60


# looking at the difference in the flower species we measured and the species we observed 
visitdata$genus_species<-paste(visitdata$plant_genus, visitdata$plant_species, sep = "_")
unique(visitdata$genus_species)
setdiff(ftraits$genus_species, visitdata$genus_species)

#189 bee species
unique(visitdata$plant_code)
#272 flower species
unique(visitdata$site)
#16 sites
unique(joes_data$gs)
#137 bees w/o filter
#129 bees w/ filter
unique(joes_data$genus_species)
#66 flowers w/o filter
#66 flower w/ filter

## 11587 originally
## filtered = 8165 visits (>70% of total original visits)
## unfiltered = 9157
## 992 removed by filter

         
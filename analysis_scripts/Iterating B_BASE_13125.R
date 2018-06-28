### I always try to have something at the top that describes what the whole script is supposed to accomplish, and any metadata like who wrote it.



##Iterating B
require(plyr)
library(plyr)


### in Rstudio you can hide stuff held in curly brackets. convenient for legibility
### but now I see that this is a scratchpad for your work flow. label:

### this is a combination of fake code and notes to map out simulation:
{
#vector<-ITs of all X species   (X gets changed out for each bee species)
#Y= some flower                 (comment in/out different plant species)
#flower<-Y                     
#length(plant_code[flower])     (how many bees of species X landed on a given flower)
#randomly apply length # of ITs for flower
#take mean IT for flower species (call this variable Z)

#iterate this 100x

#take the mean and sd of 100 Z values for flower species
#compare the actual mean IT for flower species  (ITbyf sorted by bee species)

#have this done for each flower species
}

### choose a species and create subsetted object
speciesIT<-bombusIT[which(bombusIT$species.x=="bimaculatus"),]
speciesIT<-droplevels(speciesIT)


#fspecies<-bombusIT[which(bombusIT$plant_code=="MOFI"),]
#fspecies<-droplevels(fspecies)


### MR: SD's are NA, I assume for singeltons...
### why round to 2 decimals? not that it matters.... Oh, and length should be an integer anyways.

BFnew<-bombusIT
BFNEWER<-ddply(BFnew, .(species.x,plant_code),summarize,
      mean = round(mean(Itlength_mm),2),
      sd = round(sd(Itlength_mm),2),
      length = round(length(Itlength_mm),2)
)


rsample<-(bombusIT[sample(1:5,1,replace=TRUE),])

as.numeric(BFNEWER$length)
#length(BFNEWER[which(BFNEWER$species.x=="impatiens","length")])

#this doesn't do anything
rsample<-(bombusIT[sample(1:,,replace=TRUE)])


bombusIT[which(bombusIT$species.x=="impatiens"),]

impIT<-bombusIT[bombusIT$species.x=="impatiens","Itlength_mm"]
bimacIT<-bombusIT[bombusIT$species.x=="bimaculatus","Itlength_mm"]
grisIT<-bombusIT[bombusIT$species.x=="griseocollis","Itlength_mm"]
#visits<-length(bombusIT)


#creating a vector that is the length of impatiens visits to diff flower species with 
#numeric values that are equal to the length of imp visits for each flw species
### could run impIT and lengthcall vectors in parallel?

lengthcall<-BFNEWER[BFNEWER$species.x=="impatiens","length"]
str(BFNEWER$length)

#flength<-NULL
  #for(i in BFNEWER$plant_code){
    #for(j in 1:100){
      
    

### playing with loops!
{
storage<-data.frame()
x<-1

flength<-NULL
  for(i in BFNEWER$plant_code){
    for(j in impIT){
      for(k in 1:100){
        storage[x,1]<-i
        storage[x,2]<-j
        storage[x,3]<-k
        x<-x+1
      }}}
flength 


newdf<-NULL
x<-0
for (i in 1:10){
  newdf[i]<-x
  x<-x+11
  print("joe is confused")
}

while(x<10){
  x<-x+1
  print("joe bets that x is still less than ten")
  print(x-1)
}
}

rsample<-sample(impIT,flength,replace=T)
rsample
mean(rsample)

mikevec<-sample(1:length(bombusIT$species.x=="impatiens"), 5, replace=T)
length(bombusIT$species.x=="impatiens")
length(impIT)


## what we actually want to look at
storage<-data.frame()
x<-1
# for each row in BFNEWER, which has number of visits for each species-species interaction
for (i in 1:nrow(BFNEWER)){
  #Just do it 100x
  for (j in 1:100){
  #Sample IT lengths of a given bee species number of times that species interacted with each flower sp. with replacement 
    nsample<-sample(impIT,BFNEWER[i,5],replace=T)
    storage[x,1]<-(BFNEWER[i,1])
    storage[x,2]<-(BFNEWER[i,2])
    storage[x,3]<-length(nsample)
    storage[x,4]<-mean(nsample)
    storage[x,5]<-sd(nsample,na.rm=F)
#storage[x,6]<-obsmean
#storage[x,7]<-obssd
x<-x+1
}}
storage 

##create histogram for each species,species pair mean IT
##do this for interesting pairs first
##then put observed mean onto simulated histogram and see where it lies


#loop now populates a list called storage, goes for all spp.
storage<-list()

for (i in 1:nrow(BFNEWER)){
  x<-1
  storage[[i]]<-data.frame()
  for (j in 1:100){
    nsample<-sample(bimacIT, size=BFNEWER[i,5],replace=T)
    storage[[i]][x,1]<-length(nsample)
    storage[[i]][x,2]<-mean(nsample)
    storage[[i]][x,3]<-sd(nsample,na.rm=F)
    x<-x+1
  }
}  
  hist(storage[[2]][,2],main='IT of Bimaculatus visiting MOFI',xlab='IT length')
  
  abline(v=BFNEWER[2,3],col="red")
  #abline(v=3.36,col="blue")
simmean<-mean(storage[[51]][,2])
simmean
obsmean<-(BFNEWER[2,3])
obsmean
simsd<-sd(storage[[2]][,2])
simsd

simmean-obsmean
simsd+(BFNEWER[2,4])

#str(nsample)
}

??nsample
?abline

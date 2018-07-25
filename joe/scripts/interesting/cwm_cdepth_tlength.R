#This script is going to take the community weighted mean (CWM) tongue length of visiting bee species on each flower present for each site/date
measvisits<-read.csv('data/measvisits.csv')

#taking a cwm for each flower species present at each sitedate
## combined means that treatment and control combined   
measvisits$combined<-paste(measvisits$year, measvisits$site, sep="_")
measvisits$combined<-as.factor(measvisits$combined)
measvisits$round<-as.factor(measvisits$round)
measvisits$genus_species<-as.factor(measvisits$genus_species)

fspecies<-data.frame(matrix(ncol=7,nrow=100))
a<-0
for(i in levels(measvisits$combined)){
  for(j in levels(measvisits$round)){
    sitedate<-droplevels(measvisits[which(measvisits$combined==i & measvisits$round==j),])
    for(k in levels(sitedate$genus_species)){
      flrsub<-droplevels(sitedate[which(sitedate$genus_species==k),])
      cwm<-mean(flrsub$meantongue)
      n<-length(flrsub$meantongue)
      vari<-var(flrsub$meantongue)
      depth<-mean(flrsub$depth)
      a<-a+1
      fspecies[a,]<-cbind(i, j, k, cwm, n, vari, depth)
    }}}

#make a plot for each site date showing unweighted mean tongue and mean corolla depth for each flower species
fspecies[,"sitedate"]<-paste(fspecies$X1, fspecies$X2, sep="_")
head(fspecies)
names(fspecies)<-c("site", "date", "plant", "cwm", "n", "variance", "depth", "sitedate")
fspecies
str(fspecies)
fspecies$cwm<-as.numeric(fspecies$cwm)
fspecies$depth<-as.numeric(fspecies$depth)

#drop cirsium vulgare because it's too big
fspecies<-droplevels(fspecies[which(fspecies$plant!="Cirsium_vulgare"),])

# plot(fspecies$depth, fspecies$cwm, main="average visitor tongue length by corolla depth", xlab="corolla depth (mm)", ylab="average tongue length of visiting bees (mm)")
# summary(lm(fspecies$cwm~fspecies$depth))
# abline(lm(fspecies$cwm~fspecies$depth))
# 
# plot(log(fspecies$depth+1), fspecies$cwm)
# summary(lm(fspecies$cwm~log(fspecies$depth+1)))
# abline(lm(fspecies$cwm~(log(fspecies$depth+1))))

#going to do the same thing but with unweighted tongue length
fspecies<-data.frame(matrix(ncol=7,nrow=100))
a<-0
for(i in levels(measvisits$combined)){
  for(j in levels(measvisits$round)){
    sitedate<-droplevels(measvisits[which(measvisits$combined==i & measvisits$round==j),])
    for(k in levels(sitedate$genus_species)){
      flrsub<-droplevels(sitedate[which(sitedate$genus_species==k),])
      tongue<-mean(unique(flrsub$meantongue))
      n<-length(unique(flrsub$meantongue))
      vari<-var(unique(flrsub$meantongue))
      depth<-mean(flrsub$depth)
      a<-a+1
      fspecies[a,]<-cbind(i, j, k, tongue, n, vari, depth)
    }}}
#make a plot for each site date showing unweighted mean tongue and mean corolla depth for each flower species
fspecies[,"sitedate"]<-paste(fspecies$X1, fspecies$X2, sep="_")
head(fspecies)
names(fspecies)<-c("site", "date", "plant", "unw_tongue", "n", "variance", "depth", "sitedate")
fspecies
str(fspecies)
fspecies$unw_tongue<-as.numeric(fspecies$unw_tongue)
fspecies$depth<-as.numeric(fspecies$depth)
#drop cirsium vulgare because it's too big
fspecies<-droplevels(fspecies[which(fspecies$plant!="Cirsium_vulgare"),])

#quartz()
plot(fspecies$depth, fspecies$uww_tongue, main="unweighted mean visitor tongue length by corolla depth", xlab="corolla depth (mm)", ylab="unweighted mean tongue length of visiting bees (mm)")
summary(lm(fspecies$unw_tongue~fspecies$depth))
abline(lm(fspecies$unw_tongue~fspecies$depth))

plot(log(fspecies$depth+1), fspecies$unw_tongue)
summary(lm(fspecies$unw_tongue~log(fspecies$depth+1)))
abline(lm(fspecies$unw_tongue~(log(fspecies$depth+1))))


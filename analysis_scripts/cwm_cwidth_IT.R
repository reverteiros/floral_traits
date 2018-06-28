#This script is going to take the community weighted mean (CWM) and unweighted mean IT of visiting bee species on each flower present for each site/date
measvisits<-read.csv('data/measvisits.csv')

#taking a cwm for each flower species present at each sitedate
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
      cwm<-mean(flrsub$meanIT)
      n<-length(flrsub$meanIT)
      vari<-var(flrsub$meanIT)
      width<-mean(flrsub$width)
      a<-a+1
      fspecies[a,]<-cbind(i, j, k, cwm, n, vari, width)
    }}}

#make a table that includes cwm IT for each flower present at each sitedate
fspecies[,"sitedate"]<-paste(fspecies$X1, fspecies$X2, sep="_")
head(fspecies)
names(fspecies)<-c("site", "date", "plant", "cwm", "n", "variance", "width", "sitedate")
fspecies
str(fspecies)
fspecies$cwm<-as.numeric(fspecies$cwm)
fspecies$width<-as.numeric(fspecies$width)

# plot(fspecies$width, fspecies$cwm, main="average visitor IT by corolla width", xlab="corolla width (mm)", ylab="average IT of visiting bees (mm)")
# summary(lm(fspecies$cwm~fspecies$width))
# abline(lm(fspecies$cwm~fspecies$width))
# 
# plot(log(fspecies$width+1), fspecies$cwm)
# summary(lm(fspecies$cwm~log(fspecies$width+1)))
# abline(lm(fspecies$cwm~(log(fspecies$width+1))))

#going to do the same thing but with unweighted IT
fspecies<-data.frame(matrix(ncol=7,nrow=100))
a<-0
for(i in levels(measvisits$combined)){
  for(j in levels(measvisits$round)){
    sitedate<-droplevels(measvisits[which(measvisits$combined==i & measvisits$round==j),])
    for(k in levels(sitedate$genus_species)){
      flrsub<-droplevels(sitedate[which(sitedate$genus_species==k),])
      IT<-mean(unique(flrsub$meanIT))
      n<-length(unique(flrsub$meanIT))
      vari<-var(unique(flrsub$meantongue))
      width<-mean(flrsub$width)
      a<-a+1
      fspecies[a,]<-cbind(i, j, k, IT, n, vari, width)
    }}}

#make a plot for each site date showing unweighted mean tongue and mean corolla depth for each flower species
fspecies[,"sitedate"]<-paste(fspecies$X1, fspecies$X2, sep="_")
head(fspecies)
names(fspecies)<-c("site", "date", "plant", "unw_IT", "n", "variance", "width", "sitedate")
fspecies
str(fspecies)
fspecies$unw_IT<-as.numeric(fspecies$unw_IT)
fspecies$width<-as.numeric(fspecies$width)

#quartz()
plot(fspecies$width, fspecies$uww_IT, main="unweighted mean visitor IT by corolla width", xlab="corolla width (mm)", ylab="unweighted mean IT of visiting bees (mm)")
summary(lm(fspecies$unw_IT~fspecies$width))
abline(lm(fspecies$unw_IT~fspecies$width))

plot(log(fspecies$width+1), fspecies$unw_IT)
summary(lm(fspecies$unw_IT~log(fspecies$width+1)))
abline(lm(fspecies$unw_IT~(log(fspecies$width+1))))




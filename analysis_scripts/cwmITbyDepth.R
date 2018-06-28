##looking at cwm of IT related to corolla depth
measvisits<-read.csv('data/measvisits.csv')

#taking a cwm for each flower species present at each sitedate
measvisits$sc<-as.factor(measvisits$sc)
measvisits$round<-as.factor(measvisits$round)
measvisits$genus_species<-as.factor(measvisits$genus_species)
fspecies<-data.frame(matrix(ncol=5,nrow=100))
a<-0
for(i in levels(measvisits$sc)){
  for(j in levels(measvisits$round)){
    sitedate<-droplevels(measvisits[which(measvisits$sc==i & measvisits$round==j),])
    for(k in levels(sitedate$genus_species)){
      flrsub<-droplevels(sitedate[which(sitedate$genus_species==k),])
      cwmIT<-mean(flrsub$meanIT)
      depth<-mean(flrsub$depth)
      a<-a+1
      fspecies[a,]<-cbind(i, j, k, cwmIT, depth)
    }}}


head(fspecies)
head(sitedate)


#make a plot for each site date showing cwm and mean corolla depth for each flower species
fspecies[,"sitedate"]<-paste(fspecies$X1, fspecies$X2, sep="_")
head(fspecies)
names(fspecies)<-c("site", "date", "plant", "cwmIT", "depth", "sitedate")
fspecies
str(fspecies)
fspecies$cwm<-as.numeric(fspecies$cwmIT)
fspecies$depth<-as.numeric(fspecies$depth)
#drop cirsium vulgare because it's too big
fspecies<-droplevels(fspecies[which(fspecies$plant!="Cirsium_vulgare"),])

plot(fspecies$depth, fspecies$cwmIT, main="average visitor IT by corolla depth", xlab="corolla depth (mm)", ylab="mean IT of visiting bees (mm)")
summary(lm(fspecies$cwm~fspecies$depth))
abline(lm(fspecies$cwm~fspecies$depth))

plot(log(fspecies$depth+1), fspecies$cwm)
summary(lm(fspecies$cwm~log(fspecies$depth+1)))
abline(lm(fspecies$cwm~(log(fspecies$depth+1))))




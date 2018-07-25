# looking at tongue length related to corolla width
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
      cwmtongue<-mean(flrsub$meantongue)
      width<-mean(flrsub$width)
      a<-a+1
      fspecies[a,]<-cbind(i, j, k, cwmtongue, width)
    }}}

head(fspecies)
head(sitedate)


#make a plot for each site date showing cwm and mean corolla depth for each flower species
fspecies[,"sitedate"]<-paste(fspecies$X1, fspecies$X2, sep="_")
head(fspecies)
names(fspecies)<-c("site", "date", "plant", "cwmtongue", "width", "sitedate")
fspecies
str(fspecies)
fspecies$cwmtongue<-as.numeric(fspecies$cwmtongue)
fspecies$depth<-as.numeric(fspecies$width)
#drop cirsium vulgare because it's too big
fspecies<-droplevels(fspecies[which(fspecies$plant!="Cirsium_vulgare"),])

plot(fspecies$depth, fspecies$cwm, main="average visitor tongue length by corolla width", xlab="corolla width (mm)", ylab="mean tongue length of visiting bees (mm)")
summary(lm(fspecies$cwmtongue~fspecies$width))
abline(lm(fspecies$cwmtongue~fspecies$width))

plot(log(fspecies$width+1), fspecies$cwmtongue)
summary(lm(fspecies$cwm~log(fspecies$width+1)))
abline(lm(fspecies$cwm~(log(fspecies$width+1))))




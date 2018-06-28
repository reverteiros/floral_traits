# cwm of IT vs width
## same analysis as cwm_depth_tlength ...but going to do it for IT and width

measvisits<-read.csv('data/measvisits.csv')

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
      width<-mean(flrsub$width)
      a<-a+1
      fspecies[a,]<-cbind(i, j, k, cwmIT, width)
    }}}

fspecies[,"sitedate"]<-paste(fspecies$X1, fspecies$X2, sep="_")
head(fspecies)
names(fspecies)<-c("site", "date", "plant", "cwmIT", "width", "sitedate")
fspecies
str(fspecies)
fspecies$cwm<-as.numeric(fspecies$cwmIT)
fspecies$depth<-as.numeric(fspecies$width)

#drop centaurea cyanus because it's too big
fspecies<-droplevels(fspecies[which(fspecies$plant!="Centaurea_cyanus"),])

plot(fspecies$depth, fspecies$cwm, main="average visitor IT by corolla width", xlab="corolla width (mm)", ylab="mean IT of visiting bees (mm)")
summary(lm(fspecies$cwm~fspecies$depth))
abline(lm(fspecies$cwm~fspecies$depth))

plot(log(fspecies$depth+1), fspecies$cwm)
summary(lm(fspecies$cwm~log(fspecies$depth+1)))
abline(lm(fspecies$cwm~(log(fspecies$depth+1))))


# this script is going to look at the unweighted / cwm IT of all the flowers present at one sitedate...for now, you need to run the entirety of cwm_cwidth_IT before running this script for unweighted IT
require(plyr)
require(lattice)
quartz()
fspecies$sitedate<-as.factor(fspecies$sitedate)
#subset so that only sitedates with more than 2 points are considered...this is called sd
i<-"2013_FHT_2"
fspecies$sitedate<-as.factor(fspecies$sitedate)
sd<-data.frame()
for(i in levels(fspecies$sitedate)){
  sdnew<-fspecies[which(fspecies$sitedate==i),]
  if(length(sdnew$sitedate)>2){
    sd<-rbind(sd, sdnew)
  }}
sd<-droplevels(sd)
write.csv(sd, 'data/sd.csv')
#make plots for CWM for each sitedate that has more than 2 points
xyplot(cwm~width|sitedate, data=sd,
       par.strip.text=list(cex=.75),
       main="Community Weighted Visitor IT by Corolla Width",
       xlab="Corolla Width (mm)",
       ylab="IT (mm)",
)
# now generate the spearman ranks for each of the sitedate plots 
rwct<-0
sd$sitedate<-as.factor(sd$sitedate)
spearman<-data.frame(matrix(nrow=90, ncol=2))
for(i in unique(levels(sd$sitedate))){
  print(i)
  test<-sd[which(sd$sitedate==i),]
  spear<-cor(test$cwm, test$width, method = "spearman")
  newrow<-as.vector(c(i, spear))
  rwct<-rwct+1
  spearman[rwct,]<-newrow 
  i
}
spearman
write.csv(spearman, 'data/cwm_IT_spearman.csv')

# Make plots for unw_IT instead of cwm
 xyplot(unw_IT~width|sitedate, data=sd,
       par.strip.text=list(cex=.75),
       main="Unweighted Visitor IT by Corolla Width",
       xlab="Corolla Width (mm)",
       ylab="IT (mm)",
 )
 # now generate the spearman ranks for each of the sitedate plots 
 rwct<-0
 sd$sitedate<-as.factor(sd$sitedate)
 spearman<-data.frame(matrix(nrow=90, ncol=2))
 for(i in unique(levels(sd$sitedate))){
   print(i)
   test<-sd[which(sd$sitedate==i),]
   spear<-cor(test$unw_IT, test$width, method = "spearman")
   newrow<-as.vector(c(i, spear))
   rwct<-rwct+1
   spearman[rwct,]<-newrow 
   i
 }
 spearman
 write.csv(spearman, 'data/unw_IT_spearman.csv')
# make plots for all sitedates...including those with less than 2 points
# sitedate=fspecies$sitedate
# xyplot(unw_IT~width|sitedate, data=fspecies)
# xyplot(cwm~width|sitedate, data=fspecies)
# this script is going to look at the unweighted / cwm tongue length of all the flowers at one site date...for now...you need to run the entirety of cwm_cdepth_tlength 
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

#make plots for each sitedate that has more than 2 points...for unw_tongue
xyplot(unw_tongue~depth|sitedate, data=sd,
        par.strip.text=list(cex=.75),
        main="Unweighted Visitor Tongue Length by Corolla Depth",
        xlab="Corolla Depth (mm)",
        ylab="Tongue Length (mm)",
 )

# # now generate the spearman ranks for each of the sitedate plots 
rwct<-0
sd$sitedate<-as.factor(sd$sitedate)
spearman<-data.frame(matrix(nrow=90, ncol=2))
for(i in unique(levels(sd$sitedate))){
  print(i)
  test<-sd[which(sd$sitedate==i),]
  spear<-cor(test$unw_tongue, test$depth, method = "spearman")
  newrow<-as.vector(c(i, spear))
  rwct<-rwct+1
    spearman[rwct,]<-newrow 
    i
}
spearman
write.csv(spearman, 'data/unw_tongue_spearman.csv')

# Making plots for cwm instead of unw_tongue
quartz()
 xyplot(cwm~depth|sitedate, data=sd,
        par.strip.text=list(cex=.75),
        main="Community Weighted Visitor Tongue Length by Corolla Depth",
        xlab="Corolla Depth (mm)",
        ylab="Tongue Length (mm)",
 )

# now generate spearman rank correlations for each of the sitedate plots 
rwct<-0
sd$sitedate<-as.factor(sd$sitedate)
spearman<-data.frame(matrix(nrow=90, ncol=2))
for(i in unique(levels(sd$sitedate))){
  print(i)
  test<-sd[which(sd$sitedate==i),]
  spear<-cor(test$cwm, test$depth, method = "spearman")
  newrow<-as.vector(c(i, spear))
  rwct<-rwct+1
  spearman[rwct,]<-newrow 
  i
}
spearman
write.csv(spearman, 'data/cwm_tongue_spearman_filter.csv')

# make plots for all sitedates...including those with less than 2 points
# sitedate=fspecies$sitedate
# xyplot(unw_tongue~depth|sitedate, data=fspecies)

## making a plot for only one sitedate
quartz()
sd<-droplevels(sd[which(sd$sitedate=="2013_FH_3"),])
xyplot(cwm~depth|sitedate, data=sd, pch =20,
       par.strip.text=list(cex=.75),
       main="Community Weighted Visitor Tongue Length by Corolla Depth",
       xlab="Corolla Depth (mm)",
       ylab="Tongue Length (mm)",
)

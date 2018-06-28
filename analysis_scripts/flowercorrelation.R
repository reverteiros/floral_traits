#merging pres/abs with flower counts
counts<-read.csv("counts.csv",header=T)
head(counts)
str(counts)
presabs<-read.csv("presabs.csv",header=T)
head(presabs)

#remember all=T for merge commands, but not sure why
abund<-merge(counts,presabs, all=T)
#alternate way to do this is just bind, which makes order all the counts, then all the pres-abs
#bound<-rbind(counts, presabs)
#unsure of what new d.f. looks like though
head(abund)
#getting rid of none flowers
newabund<-abund[abund$plant_genus!="none",]
str(newabund)

#subset for specific site/date for counts
counts$site<-as.factor(counts$site)
counts$round<-as.factor(counts$round)
sitedate<-list()
a<-0
for(i in levels(counts$site)){
  for (j in levels(counts$round)){
    a<-a+1
    sitedate[[a]]<-counts[which(counts$site==i & counts$round==j),]
}}
sitedate[[5]][which(sitedate[[5]]$plant_genus=="Trifolium"),]
counts

#summing the counts and number of quadrats for each flower species
meltedab = melt(tapply(sitedate[[1]]$count))

library(doBy)
vects<-summaryBy(count~plant_species, data=counts, FUN=c(length, sum))
cor(vects$count.length, vects$count.sum)
#Michael doesn't know how to use ddply.
#vects<-ddply(counts, counts$plant_species, length=length(counts$count), sum=sum(counts$count))

#creating a new function to use in lapply

#this just checks to see if there are four rows in each data frame. If so, then this function does the summary, if not, it returns NULL. lapply works now.
joesfun=function(x){
  if (!is.na(x$year[4]))  summaryBy(data=x,count~plant_species,  FUN=c(length, sum)) 

}

#creating function to get rid of nones in a sitedate
none<-function(x){
  x[which(x$plant_code!="none"),]
}

#now joesfun(counts) is the same as vects
#lapply not working because some of the elements of sitedate are NULL. specifically, sitedate[[6]] and sitedate[[10]]. The error it gives means that you are trying to apply the function to a null element; not sure why you're getting those besides perhaps unentered data? 

##
testinglapply<-lapply(sitedate, FUN=joesfun)
testinglapply
vects
?NA
sitedate[[17]]

sitedate
lapply(sitedate, FUN=joesfun)
?lapply
str(sitedate)


#just going to do the function for manually entered sitedates
sd1<-joesfun(none(sitedate[[1]]))
cor1<-cor(sd1$count.length,sd1$count.sum)

sd2<-joesfun(none(sitedate[[2]]))
cor2<-cor(sd2$count.length,sd2$count.sum)

sd3<-joesfun(none(sitedate[[3]]))
cor3<-cor(sd3$count.length,sd3$count.sum)
#correlation is zero for sitedate[[3]]
cor3
sd3
sd4<-joesfun(none(sitedate[[4]]))
cor4<-cor(sd4$count.length,sd4$count.sum)
#negative correlation for sitedate[[4]]

sd5<-joesfun(none(sitedate[[5]]))
cor5<-cor(sd5$count.length,sd5$count.sum)

#sd6 not working ... sitedate[[6]] has no length 
#sd6<-joesfun(sitedate[[6]])
#cor6<-cor(sd6$count.length,sd6$count.sum)

sd7<-joesfun(none(sitedate[[7]]))
cor7<-cor(sd7$count.length,sd7$count.sum)

sd8<-joesfun(none(sitedate[[8]]))
cor8<-cor(sd8$count.length,sd8$count.sum)

sd9<-joesfun(none(sitedate[[9]]))
cor9<-cor(sd9$count.length,sd9$count.sum)

#sd10 not working ... sitedate[[10]] has no length
#sd10<-joesfun(sitedate[[10]])
#cor10<-cor(sd10$count.length,sd10$count.sum)

sd11<-joesfun(none(sitedate[[11]]))
cor11<-cor(sd11$count.length,sd11$count.sum)

sd12<-joesfun(none(sitedate[[12]]))
cor12<-cor(sd12$count.length,sd12$count.sum)

sd13<-joesfun(none(sitedate[[13]]))
cor13<-cor(sd13$count.length,sd13$count.sum)

sd14<-joesfun(none(sitedate[[14]]))
cor14<-cor(sd14$count.length,sd14$count.sum)

sd15<-joesfun(sitedate[[15]])
cor15<-cor(sd15$count.length,sd15$count.sum)

sd16<-joesfun(none(sitedate[[16]]))
cor16<-cor(sd16$count.length,sd16$count.sum)

#take mean of all the correlations
#had to take out cor3 because it was 0
mean(c(cor1, cor2, cor4, cor5, cor7, cor8, cor9, cor11, cor12, cor13, cor14, cor15, cor16))
print(c(cor1, cor2, cor3, cor4, cor5, cor7, cor8, cor9, cor11, cor12, cor13, cor14, cor15, cor16))


sitedate[[6]]
sitedate[[6]]$year[4]

sitedate[[1]][which(sitedate[[1]]$plant_code!="none"),]



  


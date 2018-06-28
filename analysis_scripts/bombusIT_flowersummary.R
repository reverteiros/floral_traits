#for function ddply
require(plyr)
library(plyr)
#for function errbar (plotting)
require(Hmisc)
library(Hmisc)
library(doBy)
require(ggplot2)
?ddply
bombusIT<-read.csv('bombusIT_withplants.csv')
bombusIT$Itlength_mm<-as.numeric(as.character(bombusIT$Itlength_mm))
bombusIT<-bombusIT[which(bombusIT$Itlength_mm>0 & bombusIT$sex.x=="f"),]

## this is to run the analysis with single species

#impatiens
#bombusIT<-bombusIT[which(bombusIT$species.x=="impatiens"),]
#griseocollis

# bombusIT<-bombusIT[which(bombusIT$species.x=="griseocollis"),]

# #bimaculatus
# 
<<<<<<< HEAD
bombusIT<-bombusIT[which(bombusIT$species.x=="impatiens"),]
=======
#bombusIT<-bombusIT[which(bombusIT$species.x=="bimaculatus"),]
>>>>>>> 606bd849aa2ff766b228bc5e437c53768244739a
droplevels(bombusIT)

## trying to do this with date instead

ITbyd<-bombusIT[order(bombusIT$fdate),] 
ITbyd$datelevel<-as.numeric(as.factor(bombusIT$fdate))

plot(ITbyd$datelevel, ITbyd$Itlength_mm)
summary(lm(ITbyd$Itlength_mm~(ITbyd$datelevel)))
mean(ITbyd$Itlength_mm)
abline(lm(ITbyd$Itlength_mm~(ITbyd$datelevel)))

##by round instead of date
plot(ITbyd$round, ITbyd$Itlength_mm)
summary(lm(ITbyd$Itlength_mm~(ITbyd$round)))
mean(ITbyd$Itlength_mm)
abline(lm(ITbyd$Itlength_mm~(ITbyd$round)))
#list(beeIT$Genus)
# ?ddply
# ITbyf<-ddply(bombusIT, .(Genus), summarize,
#       mean = mean(Itlength_mm),
#       sd = sd(Itlength_mm))
?summaryBy
ITbyf<-summaryBy(bombusIT$Itlength_mm~bombusIT$Genus, data=bombusIT, FUN= c(mean,sd, length))
#this is really for round
#ITbyf<-summaryBy(bombusIT$Itlength_mm~bombusIT$round, data=bombusIT, FUN= c(mean,sd, length))
names(ITbyf)<-c("plant_genus", "IT_mean", "IT_sd", "length")
ITbyf$IT_sd[is.na(ITbyf$IT_sd)==T] = 0
#ITbyf$plant_genus[is.null(ITbyf$plant_genus)==T] = "unknown"
ITbyf
# levels(ITbyf$plant_genus)
# ITbyf<-ITbyf[c(1:29),]

ITsummary<-ITbyf[order(ITbyf$IT_mean),] 
ITsummary$ordered<-factor(ITsummary$plant_genus, as.character(ITsummary$plant_genus))
?errbar
ITbyf
#Heres the command to plot things; currently xyplot from lattice
#this does flowers
xyplot(ITsummary$IT_mean~ITsummary$ordered,scales=list(x=list(rot=45)), cex=log(ITsummary$length), xlab="Plant Genus", ylab="mean IT of visiting Bombus")
# #this does round
# xyplot(ITsummary$IT_mean~ITsummary$ordered, cex=log(ITsummary$length), xlab="collection round", ylab="mean Bombus IT")

?xyplot

#Heres just some data exploration in a more statistical, rather than graphical way
mean(bombusIT$Itlength_mm)
sd(bombusIT$Itlength_mm)
fit <- aov(bombusIT$Itlength_mm~bombusIT$Genus+bombusIT$round) 
fit
summary(fit)
<<<<<<< HEAD
=======

>>>>>>> 606bd849aa2ff766b228bc5e437c53768244739a

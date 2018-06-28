#### This script is going to plot the raw paired tongue lengths and corolla depths, but color coordinate quantiles of each corolla depth
## install necessary packages
#install.packages("dplyr")
require(dplyr)
# load visitation data
joes_data<-read.csv("data/measvisits.csv")
#drop the unused columns to remove NA
joes_data<-joes_data[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
joes_data$year<-factor(joes_data$year)
joes_data$round<-factor(joes_data$round)
joes_data<-joes_data[complete.cases(joes_data),]

# only looking at tongue lengths < 9 
##(that is where most of the zero slopes come from)
joes_data<-joes_data[joes_data$meantongue<9,]

## getting rid of Cirscium vulgare
# joes_data<-droplevels(joes_data[which(joes_data$genus_species!="Cirsium_vulgare"),])
## set which quantiles you want to look at
p <- c(.6, .8)
## calculate the quantiles for each corolla depth
quants<-
  joes_data %>% group_by(depth) %>% do(data.frame(p=p, tongue=quantile(.$meantongue, probs=p)))
quants
## now plot the raw data
quartz()
plot(jitter(joes_data$depth), joes_data$meantongue, main = "Quantiles for each Corolla Depth", xlab = "Corolla Depth (mm)", ylab = "Mean Tongue Length (mm)", pch=".", cex=3)
## overlay the colored quantiles on the raw data
points(jitter(quants$depth), jitter(quants$tongue), col=(30*p), pch = ".", cex=6)

########################################################################################

#### make the same plot, but for only 1 specified site 
#### **this assumes that lines 1-11 have already been run**
## first need to add a column with year, site, and round
joes_data$sitedate<-paste(joes_data$year, joes_data$site, joes_data$round, sep="_")
## set which quantiles you want to look at
p <- c(.05, .15, .25, .5)
## calculate the quantiles for each corolla depth
joes_data<-joes_data[which(joes_data$sitedate=="2013_FH_3"),]
quants<-
  joes_data %>% group_by(depth) %>% do(data.frame(p=p, tongue=quantile(.$meantongue, probs=p)))
quartz()
plot(jitter(joes_data$depth), joes_data$meantongue, main = "Quantiles for each Corolla Depth", xlab = "Corolla Depth (mm)", ylab = "Mean Tongue Length (mm)", pch=".", cex=3)
## overlay the colored quantiles on the raw data
points(jitter(quants$depth), quants$tongue, col=(100*p)^0.5, pch = ".", cex=6)

########################################################################################

# This part of the script is what I originally did ... it just plots all the data and colors every data point based on the quantiles of the entire data set
### quantiles not calculated for each corolla depth
require(ggplot2)
require(plyr)
require(lattice)

# plotting 7 quantiles calculated from the entire data set
quartz()
quantile(joes_data$meantongue, c( .0, .10, .15, .20, .25, .33, .40, .41, .42, .45, .50, .66, .9, .99))

ggplot(data=NULL, aes(x=joes_data$depth, y=joes_data$meantongue, col=cut(joes_data$meantongue, quantile(joes_data$meantongue, c(.0, .25, .33, .40, .42, .50, .90))))) + 
  geom_point(size=7) + scale_colour_manual(values= c("red", "orange", "yellow", "green", "blue", "purple")) + theme(legend.title=element_blank())
# COLOR LEGEND
# red = 0 - 25%
# orange = 25 - 33%
# yellow = 33 - 40%
# green = 40 - 42%
# blue = 42 - 50%
# purple = 50 - 90%
# black = 90 - 99%





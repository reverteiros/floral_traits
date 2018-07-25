#I am going to make box and whisker plots of tongue length related to corolla depth as well as head width related to corolla width

#getting the mean corolla depth and width for each flower species
## discs and rays are treated as different species of flowers ... for now
fmeas<-read.csv('FlrMeas_2015.csv')
noNA_fmeas<-fmeas[which(fmeas$c_depth1_mm!="NA"),]
head(noNA_fmeas)
meandepth<-aggregate(noNA_fmeas$c_depth1_mm, list(noNA_fmeas$plant_code), mean)
meandepth
hist(log(1+meandepth$x))


meanwidth<-aggregate(fmeas$c_width_1_mm, list(fmeas$plant_code), mean)
str(meanwidth)
hist(meanwidth$x)



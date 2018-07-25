#looking at the relationship between corolla depth and width
fullfmeas<-read.csv('data/fullfmeas.csv')

#remove Cirsium vulgare because its depth is an outlier....visually
fullfmeas<-droplevels(fullfmeas[which(fullfmeas$genus_species!="Cirsium_vulgare"),])

plot(fullfmeas$depth, fullfmeas$width, main="corolla width by corolla depth", xlab="corolla depth (mm)", ylab="corolla width (mm)")
summary(lm(fullfmeas$width~fullfmeas$depth))
abline(lm(fullfmeas$width~fullfmeas$depth))

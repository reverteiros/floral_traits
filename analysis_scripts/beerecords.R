beecount<-read.csv("beecount.csv")
axis(1, at=c(1,10,100,1000), label=c(1,10,100,1000), pos=0)
hist(log10(beecount$count), main="Records of Bee Species Visitation", 
     xlab="log(number of records)",ylab="Frequency")


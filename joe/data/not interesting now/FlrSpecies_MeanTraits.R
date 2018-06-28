##the purpose of this script is to make a table of average trait measurements for flowers
fmeas<-read.csv("FlrMeas_2015.csv")
str(fmeas)

#take out unnecessary columns
fmeas<-droplevels(fmeas)
fmeas<-fmeas[,c("plant_code","depth","width")] 
 a 
#remove any NAs in widths and depths
fmeas<-fmeas[which(fmeas$depth!='NA'&fmeas$width!='NA'),]

#take average depth and width for each plant_code
nfmeas=NULL
for(i in fmeas$plant_code){
  nfmeas=mean(fmeas$depth, fmeas$width)
}

storage 

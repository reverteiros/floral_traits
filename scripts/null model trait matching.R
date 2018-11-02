#generates null model that each be interacts with each flower spp. with probability~number of interactions that flower participated in at that site-round

library(tidyverse)
source("scripts/traits.R")


## set number of iterations
iterations <- 99
dat<-generaldata %>% mutate(sr=paste(sampling_round, site))
out<-vector("list", length(unique(dat$sr)))
            
# read and manipulate data

### for Parallel computing on Mac/Linux, comment out for windows or fix
library(parallel)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK", outfile="", XDR=F)
clusterExport(cl=cl, varlist=ls(), envir=environment())
out<-parLapply(cl, 1:length(unique(dat$sr)), function(y){
#this needs to be turned back on for windows
# out<-lapply(1:length(unique(dat$sr)), function(y){
  sub <- dat %>% filter(sr==unique(dat$sr)[y])

### create objects at the species level, with abundance, and mean traits 
databees<-sub %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue), IT=mean(IT_improved),abundance=n()) 

dataflowers<-sub %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth), width=mean(width),abundance=n())

filtered <- dplyr::inner_join(sub,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

datamatrix<-map_dfr(lapply(1:iterations,function(z){
  species <- lapply(1:length(databees$bee),function(x){
    a <- dataflowers[sample(1:length(dataflowers$depth), databees$abundance[x], replace = T, prob = dataflowers$abundance),]
    b <- databees$tongue[x]- a$depth
    c<-(databees$IT[x]-a$width)>0
    tozero<-b*as.numeric(c)
    tona<-ifelse(b<0, ifelse(c, b, NA),b)
    return(data.frame(raw_t_minuts_d=b, remove=tona, zero=tozero,
                      tongue=rep(databees$tongue[x],length(b)), bee=rep(databees$bee[x], length(b)), iter=rep(z, length(b))))
  })
  k <- map_dfr(species,rbind)
  return(k)
}), rbind)

# datamatrix$tongue <- (k$tongue)
# 
# # Add variables to dataset
# k <- dplyr::left_join(k,databees,"tongue")
# datamatrix$bee <- k$bee
# datamatrix$difference <- filtered$difference
# datamatrix$newdifference <- filtered$newdifference

return(dplyr::mutate(datamatrix, sr=unique(dat$sr)[y]))
})
stopCluster(cl)



####################### Join entire dataset##################################

datatotal <- bind_rows(out)

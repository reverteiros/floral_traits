#generates null model that each be interacts with each flower spp. with probability~number of interactions that flower participated in at that site-round

library(tidyverse)
source("scripts/traits.R")

#consider making something parallel here because this takes a really long time to run

## set number of iterations
iterations <- 999
dat<-generaldata %>% mutate(sr=paste(sampling_round, site))
out<-vector("list", length(unique(dat$sr)))
            
# read and manipulate data
out<-lapply(1:length(unique(dat$sr)), function(y){
  sub <- dat %>% filter(sr==unique(dat$sr)[y])

### create objects at the species level, with abundance, and mean traits 
databees<-sub %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-sub %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(sub,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
    return(data.frame(b,tongue=rep(databees$tongue[x],length(b))))
    print(a)
  })
  k <- map_dfr(species,rbind)
  datamatrix[,i] <- (k$b)
}

datamatrix$tongue <- (k$tongue)

# Add variables to dataset
k <- dplyr::left_join(k,databees,"tongue")
datamatrix$bee <- k$bee
datamatrix$difference <- filtered$difference
datamatrix$newdifference <- filtered$newdifference

return(dplyr::mutate(datamatrix, sr=unique(dat$sr)[y]))
})



####################### Join entire dataset##################################

datatotal <- bind_rows(out)

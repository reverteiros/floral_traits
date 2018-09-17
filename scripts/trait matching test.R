
source("scripts/traits.R")

library(ggplot2)
library(purrr)

# read and manipulate data
alldata <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

### create objects at the species level, with abundance, and mean traits (subset bee species with more than 5 individuals collected to avoid species with high errors due to small size)
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) %>%
  dplyr::filter(., abundance > 4)

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model, so matrix dimensions 14201*1000 (include a column for bee tongue length)
iterations <- 999
datamatrix <- matrix(ncol = (iterations+1),nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- a - databees$tongue[x]
    return(data.frame(b,tongue=rep(databees$tongue[x],length(b))))
  })
  k <- map_dfr(species,rbind)
  datamatrix[,i] <- (k$b)
}

datamatrix$tongue <- (k$tongue)

# Check the model works well
# hist(datamatrix[,656])
# plot(datamatrix$tongue, datamatrix[,646])

# Add variables to dataset
k <- dplyr::left_join(k,databees,"tongue")
datamatrix$bee <- k$bee
datamatrix$difference <- filtered$difference

# Mean and SD of the null models
datamatrix$mean <- apply(datamatrix[,1:(iterations)],1,FUN=mean)
datamatrix$sd <- apply(datamatrix[,1:(iterations)],1,FUN=sd)

resumdataframe <- data.frame(datamatrix$mean,datamatrix$sd,datamatrix$difference,datamatrix$tongue,datamatrix$bee)
names(resumdataframe) <- c("mean","sd","difference","tongue","bee")

resumdataframe <- resumdataframe %>% mutate(UB=mean+2*sd,LB=mean-2*sd)
resumdataframe <- resumdataframe %>% mutate(significant=if_else(difference > UB | difference < LB, TRUE, FALSE))

table(resumdataframe$significant)

traits<-resumdataframe %>%
  group_by(bee,significant,tongue) %>%
  summarize(abundance=n())

traits <- traits%>%tidyr::spread(significant, abundance)

names(traits) <- c("bee","tongue","fal","tru")

traits$bee <- factor(traits$bee, levels = traits$bee[order(traits$tongue)])



traits <- traits%>%mutate(percent=(tru/(fal+tru)))

traits$percent[which(is.na(traits$percent))]<-"0"

traits$percent <- as.numeric(as.character(traits$percent))

traits %>% ggplot(aes(bee, percent)) +
  geom_bar(stat = "identity") 

traits %>% ggplot(aes(tongue, percent)) +
  geom_point() 




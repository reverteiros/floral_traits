
source("scripts/traits.R")

library(ggplot2)
library(purrr)

# read and manipulate data
alldata <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))
alldata$difference <- alldata$tongue_length.tongue-alldata$depth


# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends
alldata<-alldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72)) %>% mutate(beewider=if_else(IT_improved>width, "true", "false")) 


######## Choose one of the following options: 
## Assume differences of 0 for small bees that can crawl in
alldata <- alldata%>% 
  mutate(newdifference=if_else(beewider== "true", difference, 0))

## Eliminate small bees that can crawl in
alldata <- alldata %>% 
  dplyr::filter(., beewider== "true")


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
datamatrix <- matrix(ncol = (iterations),nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x] - a
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
datamatrix$plant_gs <- filtered$plant_gs
datamatrix$depth <- filtered$depth


means <- numeric(1000)
sds <- numeric(1000)
means[1000] <- mean(datamatrix[,1002])
sds[1000] <- sd(datamatrix[,1002])

for(i in 1:iterations){
  means[i] <- mean(datamatrix[,i])
  sds[i] <- sd(datamatrix[,i])
}

hist(means[1:999])
abline(v=means[1000])
hist(sds[1:999])
abline(v=sds[1000])
## mean is in the random distribution, but observed sd is very different than random sds.
## observed sd is way smaller than random, indicating some trait matching.


## do it for each bee species, to see if some tongues trait match more
databytongue<-datamatrix[order(datamatrix$bee),] 
databytongue$bee <- factor(databytongue$bee)
databytongue$beenumeric <- as.numeric(databytongue$bee)

observed<-databytongue %>%
  group_by(bee) %>%
  summarize(mean_obs=mean(difference),sd_obs=sd(difference),abundance=n())

observed <- dplyr::left_join(observed, databees,by=c("bee","abundance"))


########### MEANS
meanpersp <- matrix(ncol = 999,nrow = 77)
meanpersp <- as.data.frame(meanpersp)

for(i in 1:77){
  filtre <- dplyr::filter(databytongue, beenumeric == i)
  filtre2 <- filtre[,1:999]
  meanpersp[i,] <- apply(filtre2,2,mean)
  
}

meanpersp$bee <- unique(databytongue$bee)


# Mean,  and quantiles of the null models
meanpersp$quantile975 <- apply(meanpersp[,1:999],1,quantile,probs=c(.975))
meanpersp$quantile25 <- apply(meanpersp[,1:999],1,quantile,probs=c(.025))

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datamatrixtable <- dplyr::left_join(meanpersp,observed,"bee")

# Plot graph with means of proportion of flowers longer than tongues derived from null model with error bars and real data
datamatrixtable %>%
  ggplot(aes(x=tongue))+
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=mean_obs),col="red") +
  labs(y="Mean difference per bee sp (mm)",x="Bee tongue length (mm)") +
  theme_classic()
# sometimes the plot gives error, run the previous line and the plot again and is fine



########### SD
sdpersp <- matrix(ncol = 999,nrow = 77)
sdpersp <- as.data.frame(sdpersp)

for(i in 1:77){
  filtre <- dplyr::filter(databytongue, beenumeric == i)
  filtre2 <- filtre[,1:999]
  sdpersp[i,] <- apply(filtre2,2,sd)
  
}

sdpersp$bee <- unique(databytongue$bee)


# Mean,  and quantiles of the null models
sdpersp$quantile975 <- apply(sdpersp[,1:999],1,quantile,probs=c(.975))
sdpersp$quantile25 <- apply(sdpersp[,1:999],1,quantile,probs=c(.025))

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datamatrixtable <- dplyr::left_join(sdpersp,observed,"bee")

# Plot graph with means of proportion of flowers longer than tongues derived from null model with error bars and real data
datamatrixtable %>%
  ggplot(aes(x=tongue))+
  # geom_point(aes(y=sd)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=sd_obs),col="red") +
  labs(y="SD difference per bee sp (mm)",x="Bee tongue length (mm)") +
  theme_classic()


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
nullmodel <- matrix(ncol = (iterations),nrow = sum(databees$abundance))
nullmodel <- as.data.frame(nullmodel)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x] - a
    return(data.frame(b,tongue=rep(databees$tongue[x],length(b))))
  })
  k <- map_dfr(species,rbind)
  nullmodel[,i] <- (k$b)
}

nullmodel$tongue <- (k$tongue)

# Check the model works well
# hist(nullmodel[,656])
# plot(nullmodel$tongue, nullmodel[,646])

# Add variables to dataset
k <- dplyr::left_join(k,databees,"tongue")
nullmodel$bee <- k$bee
nullmodel$difference <- filtered$difference
nullmodel$plant_gs <- filtered$plant_gs
nullmodel$depth <- filtered$depth

############### Test for trait matching with the entire network 
####(approach similar to Sazatornil et al 2016)
means <- numeric(iterations+1)
sds <- numeric(iterations+1)
means[iterations+1] <- mean(nullmodel[,(iterations+3)])#add observed value
sds[iterations+1] <- sd(nullmodel[,(iterations+3)])#add observed value

# generate means and sd of each entire null network, 999 replicates
for(i in 1:iterations){
  means[i] <- mean(nullmodel[,i])
  sds[i] <- sd(nullmodel[,i])
}

#Graphs to test if observed values are different from random
hist(means[1:iterations],main="Mean difference (tongue length minus flower depth, mm)",xlab="",ylab="")# run both lines together or does not work
abline(v=means[iterations+1])
hist(sds[1:iterations],xlim=c(5,7),main=" SD difference (tongue length minus flower depth, mm)",xlab="",ylab="")
abline(v=sds[iterations+1])
## mean is in the random distribution, but observed sd is very different than random sds.
## observed sd is way smaller than random, indicating some trait matching.


######### Test for trait matching at the species level. 
## Do it for each bee species, to see if some tongues trait match more

nullperbee<-nullmodel[order(nullmodel$bee),] 
nullperbee$bee <- factor(nullperbee$bee)
nullperbee$beenumeric <- as.numeric(nullperbee$bee)

observed<-nullperbee %>%
  group_by(bee) %>%
  summarize(mean_obs=mean(difference),sd_obs=sd(difference),abundance=n())

observed <- dplyr::left_join(observed, databees,by=c("bee","abundance"))


########### MEANS
meanpersp <- matrix(ncol = 999,nrow = 77)
meanpersp <- as.data.frame(meanpersp)

for(i in 1:77){
  filtre <- dplyr::filter(nullperbee, beenumeric == i)
  filtre2 <- filtre[,1:999]
  meanpersp[i,] <- apply(filtre2,2,mean)
}

meanpersp$bee <- unique(databytongue$bee)

# Quantiles of the null models
meanpersp$quantile975 <- apply(meanpersp[,1:999],1,quantile,probs=c(.975))
meanpersp$quantile25 <- apply(meanpersp[,1:999],1,quantile,probs=c(.025))

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datameans <- dplyr::left_join(meanpersp,observed,"bee")

# Plot graph with means of proportion of flowers longer than tongues derived from null model with error bars and real data
ggplot(datameans, aes(y=mean_obs, x=tongue)) + 
  geom_point(position=position_dodge(width=0.3), size=1.5,color="red") +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + coord_flip()+
  labs(y=" Mean difference (tongue length minus flower depth, mm)",x="Bee tongue length (mm)") +
  theme_classic()


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
datasd <- dplyr::left_join(sdpersp,observed,"bee")

# Plot graph with sd of proportion of flowers longer than tongues derived from null model with error bars and real data
ggplot(datasd, aes(y=sd_obs, x=tongue)) + 
  geom_point(position=position_dodge(width=0.3), size=1.5,color="red") +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + coord_flip()+
  labs(y=" SD difference (tongue length minus flower depth, mm)",x="Bee tongue length (mm)") +
  theme_classic()


#### Plot mean difference vs SD per each bee species 
ggplot(observed, aes(y=mean_obs, x=sd_obs)) + 
  geom_jitter(height=0.1) + 
  theme_classic() + coord_flip() +
  labs(y="Mean difference (tongue length minus flower depth, mm)", x="SD difference (mm)")


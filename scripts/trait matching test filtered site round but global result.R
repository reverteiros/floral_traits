# this script generates simulated visits for each bee species based on the flower visit frequencies at all site rounds at which the focal bee was detected. 
# it produces a bunch of plots for the differences between bee tongue length and flower corolla for the observed and simulated data.
# to do: look at mean vs. SD difference in simulated data. 

source("scripts/traits.R")

library(ggplot2)
library(purrr)


## Choose at the beginning if we want to remove small bees that can crawl in
generaldata <- generaldata %>% 
  dplyr::filter(., beewider== "true")


source("scripts/null model trait matching.R")

# Filter species that appear less than 5 times
datatotalbees<-datatotal %>%
  group_by(bee) %>%
  summarize(abundance=n()) %>%
  dplyr::filter(., abundance > 4)

datatotal <- dplyr::inner_join(datatotal, datatotalbees, by = "bee")

## Now we have the null model distributions per each interaction with the flowers that each individual bee can face at the site-round that was present, it's time to work with the data. 

############### Test for trait matching with the entire network 
####(approach similar to Sazatornil et al 2016)
means <- numeric(iterations+2)
sds <- numeric(iterations+2)
means[iterations+1] <- mean((datatotal[,(iterations+3)]))#add observed value
sds[iterations+1] <- sd(datatotal[,(iterations+3)])#add observed value
means[iterations+2] <- mean((datatotal[,(iterations+4)]))#add observed value assuming 0 difference for small bees
sds[iterations+2] <- sd(datatotal[,(iterations+4)])#add observed value assuming 0 difference for small bees

# generate means and sd of each entire null network, 999 replicates
for(i in 1:iterations){
  means[i] <- mean(datatotal[,i])
  sds[i] <- sd(datatotal[,i])
}

#Graphs to test if observed values are different from random
hist(means[1:iterations],main="Mean difference (tongue length minus flower depth, mm)",xlab="",ylab="")
abline(v=means[iterations+1])
# run both lines together or doesn't work
hist(sds[1:iterations],xlim=c(4,7),main=" SD difference (tongue length minus flower depth, mm)",xlab="",ylab="")
abline(v=sds[iterations+1])
## mean is in the random distribution, but observed sd is very different than random sds.
## observed sd is way smaller than random, indicating some trait matching.


#Graphs to test if observed values are different from random assuming 0 difference for small bees 
hist(means[1:iterations],xlim=c(-1,1),main="Mean difference (tongue length minus flower depth, mm)",xlab="",ylab="")
abline(v=means[iterations+2])
# run both lines together or doesn't work
hist(sds[1:iterations],xlim=c(4,7),main=" SD difference (tongue length minus flower depth, mm)",xlab="",ylab="")
abline(v=sds[iterations+2])



######### Test for trait matching at the species level. 
## Do it for each bee species, to see if some tongues trait match more

nullperbee<-datatotal[order(datatotal$bee),] 
nullperbee$bee <- factor(nullperbee$bee)
nullperbee$beenumeric <- as.numeric(nullperbee$bee)

observed<-nullperbee %>%
  group_by(bee) %>%
  summarize(mean_obs=mean(difference),sd_obs=sd(difference),mean_newdif_obs=mean(newdifference),sd_newdif_obs=sd(newdifference),abundance=n())

observed <- dplyr::left_join(observed, databees,by=c("bee","abundance"))


########### MEANS
meanpersp <- matrix(ncol = 999,nrow = nrow(observed))
meanpersp <- as.data.frame(meanpersp)

for(i in 1:nrow(observed)){
  filtre <- dplyr::filter(nullperbee, beenumeric == i)
  filtre2 <- filtre[,1:999]
  meanpersp[i,] <- apply(filtre2,2,mean)
}

meanpersp$bee <- unique(observed$bee)

# Quantiles of the null models
meanpersp$quantile975 <- apply(meanpersp[,1:999],1,quantile,probs=c(.975))
meanpersp$quantile25 <- apply(meanpersp[,1:999],1,quantile,probs=c(.025))
meanpersp$mean <- apply(meanpersp[,1:999],1,mean)
meanpersp$sd <- apply(meanpersp[,1:999],1,sd)

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datameans <- dplyr::left_join(meanpersp,observed,"bee")

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datameans$tongue <- NULL

# Generate a matrix with each bee one time, with tongue length of each
databeesall<-generaldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

# Include tongue length to dataframe
datamatrixmeans <- dplyr::left_join(datameans,databeesall,"bee")

# Plot graph with means of proportion of flowers longer than tongues derived from null model with error bars and real data
ggplot(datamatrixmeans, aes(y=mean_obs, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  labs(y=" Mean difference (tongue length minus flower depth, mm)",x="Bee tongue length (mm)") +
  theme_classic()

# Plot graph with means of proportion of flowers longer than tongues derived from null model with error bars and real data
ggplot(datamatrixmeans, aes(y=mean_newdif_obs, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  labs(y=" Mean difference (tongue length minus flower depth, mm)",x="Bee tongue length (mm)") +
  theme_classic()


########### SD
sdpersp <- matrix(ncol = 999,nrow = nrow(observed))
sdpersp <- as.data.frame(sdpersp)

for(i in 1:nrow(observed)){
  filtre <- dplyr::filter(nullperbee, beenumeric == i)
  filtre2 <- filtre[,1:999]
  sdpersp[i,] <- apply(filtre2,2,sd)
}

sdpersp$bee <- unique(nullperbee$bee)

# Mean,  and quantiles of the null models
sdpersp$quantile975 <- apply(sdpersp[,1:999],1,quantile,probs=c(.975))
sdpersp$quantile25 <- apply(sdpersp[,1:999],1,quantile,probs=c(.025))
sdpersp$sd <- apply(sdpersp[,1:999],1,sd)
sdpersp$mean <- apply(sdpersp[,1:999],1,mean)

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datasd <- dplyr::left_join(sdpersp,observed,"bee")

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datasd$tongue <- NULL

# Include tongue length to dataframe
datamatrixsd <- dplyr::left_join(datasd,databeesall,"bee")

# Plot graph with sd of proportion of flowers longer than tongues derived from null model with error bars and real data
ggplot(datamatrixsd, aes(y=sd_obs, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  labs(y=" SD difference (tongue length minus flower depth, mm)",x="Bee tongue length (mm)") +
  theme_classic()

ggplot(datamatrixsd, aes(y=sd_newdif_obs, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  labs(y=" SD difference (tongue length minus flower depth, mm)",x="Bee tongue length (mm)") +
  theme_classic()



########### OK, we see that some observed values are far away from the 2.5% and the 97.5% quantiles. Kind of looks like the sign of the difference between the observed value and the 2.5% and 97.5% quantiles depends on the tongue of the bees, in that short-tongued bees "trait match more" = the observed value tend to be above the 97.5% quantile, while long-tongued bees tend to be under the 2.5% quantile. Let's check that:
datamatrixmeans$zmean <- (datamatrixmeans$mean_obs - meanpersp$mean) / meanpersp$sd

ggplot(datamatrixmeans, aes(y=zmean, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  theme_bw(base_size=16) + 
  labs(y="Z score mean",x="Bee tongue length (mm)") +
  theme_classic() +
  geom_smooth(method=lm)+
  geom_hline(yintercept = -1.96)+
  geom_hline(yintercept = 1.96)


a <- lm(log(datamatrixmeans$zmean)~log(datamatrixmeans$tongue))
summary(a)

datamatrixmeans$zmeannewdif <- (datamatrixmeans$mean_newdif_obs - meanpersp$mean) / meanpersp$sd

ggplot(datamatrixmeans, aes(y=zmeannewdif, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  theme_bw(base_size=16) + 
  labs(y="Z score mean",x="Bee tongue length (mm)") +
  theme_classic() +
  geom_hline(yintercept = -1.96)+
  geom_smooth(method=lm)+
  geom_hline(yintercept = 1.96)

a <- lm(log(datamatrixmeans$zmeannewdif)~log(datamatrixmeans$tongue))
summary(a)

### same for SD
datamatrixsd$zsd <- (datamatrixsd$sd_obs - sdpersp$mean) / sdpersp$sd

ggplot(datamatrixsd, aes(y=zsd, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  theme_bw(base_size=16) + 
  labs(y="Z score sd",x="Bee tongue length (mm)") +
  theme_classic() +
  geom_smooth(method=lm)+
  geom_hline(yintercept = -1.96)+
  geom_hline(yintercept = 1.96)

a <- lm(log(datamatrixsd$zsd)~log(datamatrixsd$tongue))
summary(a)

datamatrixsd$zsdnewdif <- (datamatrixsd$sd_newdif_obs - sdpersp$mean) / sdpersp$sd

ggplot(datamatrixsd, aes(y=zsdnewdif, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  theme_bw(base_size=16) + 
  labs(y="Z score sd",x="Bee tongue length (mm)") +
  theme_classic() +
  geom_smooth(method=lm)+
  geom_hline(yintercept = -1.96)+
  geom_hline(yintercept = 1.96)

a <- lm(log(datamatrixsd$zsdnewdif)~log(datamatrixsd$tongue))
summary(a)



#### Plot mean difference vs SD per each bee species 
observed %>% mutate(absdif=abs(mean_obs)) %>% ggplot(aes(x=abs(mean_obs), y=sd_obs)) + 
  geom_jitter(height=0.1) + 
  theme_classic()+
  geom_smooth(method=lm)+
  labs(y="Mean difference (tongue length minus flower depth, mm)", x="SD difference (mm)")

a <- lm(abs(observed$mean_obs)~(observed$sd_obs))
summary(a)


observed %>% mutate(absdif=abs(mean_newdif_obs)) %>% ggplot(aes(x=abs(mean_newdif_obs), y=sd_newdif_obs)) + 
  geom_jitter(height=0.1) + 
  theme_classic()+
  geom_smooth(method=lm)+
  labs(y="Mean difference (tongue length minus flower depth, mm)", x="SD difference (mm)")

a <- lm(abs(observed$mean_newdif_obs)~(observed$sd_newdif_obs))
summary(a)


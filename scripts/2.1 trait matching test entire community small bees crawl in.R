
source("scripts/1. Data cleaning.R")

library(ggbeeswarm)
library(ggplot2)
library(purrr)
library(ggExtra)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)



### create objects at the species level, with abundance, and mean traits (subset bee species with 5 individuals collected or more to avoid species with high errors due to small size)
databees <- generaldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) %>%
  dplyr::filter(., abundance > 4)

dataflowers <- generaldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(newdepth),abundance=n())

filtered <- dplyr::inner_join(generaldata,databees, by = "bee")
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
# hist(nullmodel[,696])
# plot(nullmodel$tongue, nullmodel[,646])

# Add variables to dataset
k <- dplyr::left_join(k,databees,"tongue")
nullmodel$bee <- k$bee
nullmodel$difference <- filtered$newdifference
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
hist(means[1:iterations],xlim=c(0,1),main="Tongue length - flower depth, mm",xlab="",ylab="")# run both lines together or does not work
abline(v=means[iterations+1])
hist(sds[1:iterations],xlim=c(4,6),main=" SD Tongue length - flower depth, mm)",xlab="",ylab="")
abline(v=sds[iterations+1])
## mean is in the random distribution, but observed sd is very different than random sds.
## observed sd is way smaller than random, indicating some trait matching.

datagraphs <- nullmodel[298:1002] %>%
  select(V298,difference)

ggplot(datagraphs, aes(x=V298))+
  geom_density(fill="lightblue",alpha=0.6)+
  geom_density(aes(x=difference), fill="pink",alpha=0.6)+
  theme_classic()+
  labs(x="Tongue length - flower depth, mm", y="Density")+
  geom_vline(xintercept = 0)



######### Test for trait matching at the species level. 
## Do it for each bee species, to see if some tongues trait match more

nullperbee<-nullmodel[order(nullmodel$bee),] 
nullperbee$bee <- factor(nullperbee$bee)
nullperbee$beenumeric <- as.numeric(nullperbee$bee)

observed<-nullperbee %>%
  group_by(bee) %>%
  summarize(mean_obs=mean(difference),sd_obs=sd(difference),abundance=n())%>%
  dplyr::left_join(databees,by=c("bee","abundance"))


########### MEANS
meanpersp <- matrix(ncol = 999,nrow = 78) # 78 is the number of bee species with 5 or more individuals collected
meanpersp <- as.data.frame(meanpersp)

for(i in 1:78){
  filtre <- dplyr::filter(nullperbee, beenumeric == i)
  filtre2 <- filtre[,1:999]
  meanpersp[i,] <- apply(filtre2,2,mean)
}

meanpersp$bee <- unique(nullperbee$bee)

# Quantiles of the null models
meanpersp$quantile975 <- apply(meanpersp[,1:999],1,quantile,probs=c(.975))
meanpersp$quantile25 <- apply(meanpersp[,1:999],1,quantile,probs=c(.025))

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datameans <- dplyr::left_join(meanpersp,observed,"bee")

# Plot graph with means of proportion of flowers longer than tongues derived from null model with error bars and real data
ggplot(datameans, aes(y=mean_obs, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  #geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  #coord_flip()+
  labs(y="Tongue length - flower depth, mm",x="Bee tongue length (mm)") +
  theme_classic()

ggplot(datameans, aes(y=bee, x=mean_obs)) + 
  geom_point(position=position_dodge(width=0.3), size=1.5,color="red") +
  geom_errorbar(aes(xmin=quantile25, xmax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  #coord_flip()+
  labs(y="Bee species",x="Tongue length - flower depth, mm") +
  theme_classic()



######### create graph separating the bees as visiting flowers in the range of expected, longer or shorter than expected

datameans <- datameans %>%
  mutate(higherthan975 = ifelse(mean_obs>quantile975,T,F))%>%
  mutate(higherthan25=ifelse(mean_obs>quantile25,T,F))%>%
  mutate(group = case_when(higherthan975 & higherthan25 ~ "3.Visit flowers shorter than expected", # both tests: group A
                           !higherthan975 & higherthan25 ~ "2.Visit flowers in the range of expected", # one test: group B
                           !higherthan975 & !higherthan25 ~ "1.Visit flowers longer than expected" # neither test: group C
  )) %>%
  mutate(range_width = quantile975-quantile25)


table(datameans$group)



a <- ggplot(datameans, aes(y=mean_obs, x=tongue)) + 
  geom_point(size=1.5) +
  #geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  #coord_flip()+
  labs(y="Tongue length - flower depth, mm",x="Bee tongue length (mm)") +
  theme_classic()

# b <- ggplot(datameans, aes(y=mean_obs, x=tongue)) + 
#   geom_point(size=1.5,color="red") +
#   geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
#   theme_bw(base_size=16) +
#   theme(legend.position = "none") + 
#   #coord_flip()+
#   labs(y="Tongue length - flower depth, mm",x="Bee tongue length (mm)") +
#   theme_classic()

b <- ggplot(datameans, aes(y=mean_obs, x=tongue)) + 
  geom_point(aes(colour=group),size=3) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), position = position_dodge(0.3)) +
  theme_bw(base_size=16) + 
  #coord_flip()+
  labs(y="Tongue length - flower depth, mm",x="Bee tongue length (mm)") +
  theme_classic()+ 
  theme(legend.position = "none") 

c <- ggplot(datameans, aes(x=group, y=tongue)) + 
  geom_beeswarm(aes(colour=group),size=3)+
  # geom_point(aes(colour=group),size=3) +
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none") 


ggarrange(ggarrange(a,b, ncol = 2, labels = c("A", "B")),c, ncol = 1, nrow = 2)



ggplot(datameans, aes(x=group, y=abundance)) + 
  geom_beeswarm(aes(colour=group),size=3)+
  # geom_point(aes(colour=group),size=3) +
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none") 


ggplot(datameans, aes(x=range_width, y=abundance)) + 
  # geom_beeswarm(aes(colour=group),size=3)+
  geom_point() +
  theme_classic()

ggplot(datameans, aes(x=range_width, y=tongue)) + 
  # geom_beeswarm(aes(colour=group),size=3)+
  geom_point() +
  theme_classic()

ggplot(datameans, aes(x=abundance, y=tongue)) + 
  # geom_beeswarm(aes(colour=group),size=3)+
  geom_point() +
  theme_classic()


########### same with SD. I prefer not to use it because the SD is linked to sample size more than anything






# this script generates simulated visits for each bee species based on the flower visit frequencies at all site rounds at which the focal bee was detected. 
# it produces a bunch of plots for the differences between bee tongue length and flower corolla for the observed and simulated data.
# to do: look at mean vs. SD difference in simulated data. 

source("scripts/traits.R")

library(ggplot2)
library(purrr)

#run null model
source("scripts/null model trait matching.R")

# Filter species that appear less than 5 times
obs<-dat %>%
  mutate(big=(IT_improved-width)>0, zeroed=ifelse(difference<0, difference*big, difference),deleted=ifelse(difference<0, ifelse(big, difference, NA), difference), iter=rep(7777777, length(dat$bee)))  %>% 
  group_by(bee) %>%
  # summarize(raw_mismatch=mean(difference<0), zeroed=mean(zeroed<0), deleted=mean(deleted<0, na.rm=T), abundance=n()) %>%
  filter(length(uniqueID) > 4) %>% select(bee, iter, difference, big, zeroed, deleted)

upr<-function(x){quantile(x, .975, na.rm=T)}
lwr<-function(x){quantile(x, .025, na.rm=T)}
av<-function(x){mean(x, na.rm=T)}
std<-function(x){sd(x, na.rm=T)}
z<-function(obs, xpctd, xvar){(obs-xpctd)/xvar}


#combine null and observed
# combo <- dplyr::inner_join(datatotal, obs, by = "bee")
comb<-bind_rows(obs, datatotal %>% transmute(bee, iter, difference=raw_t_minuts_d, deleted=remove, zeroed=zero, tongue, iter, sr))


nmod <- comb %>% select(-sr) %>% 
  group_by(bee, tongue, iter) %>%
  summarize_all(funs(av, std))
## Now we have the null model distributions per each interaction with the flowers that each individual bee can face at the site-round that was present, it's time to work with the data. 




######### Test for trait matching at the species level. 
## Do it for each bee species, to see if some tongues trait match more

bysp<-nmod %>% filter(iter!=7777777) %>% 
  group_by(bee, tongue) %>% summarize_all(funs(av, std, upr, lwr))%>% 
  right_join(nmod %>% filter(iter==7777777), by="bee")
########### MEANS


rawmean<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=difference_av_av)) +
  geom_errorbar(aes(ymin=difference_av_lwr, ymax=difference_av_upr)) +
  geom_point(aes(y=difference_av),size=1.5,color="red") +
  scale_x_log10()+
  labs(y="mean(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

rawmeanz<-bysp %>% ggplot(aes(tongue.x, z(difference_av,difference_av_av, difference_av_std))) + 
  geom_point() +
  scale_x_log10()+
  labs(y="z-score(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  theme_classic()

zeromean<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=zeroed_av_av)) +
  geom_errorbar(aes(ymin=zeroed_av_lwr, ymax=zeroed_av_upr)) +
  geom_point(aes(y=zeroed_av),size=1.5,color="red") +
  scale_x_log10()+
  labs(y="mean(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

zeromeanz<-bysp %>% ggplot(aes(tongue.x, z(zeroed_av,zeroed_av_av, zeroed_av_std))) + 
  geom_point() +
  scale_x_log10()+
  labs(y="z-score(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  theme_classic()


rawsd<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=difference_std_av)) +
  geom_errorbar(aes(ymin=difference_std_lwr, ymax=difference_std_upr)) +
  geom_point(aes(y=difference_std),size=1.5,color="red") +
  scale_x_log10()+
  labs(y="sd(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

zerosd<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=zeroed_std_av)) +
  geom_errorbar(aes(ymin=zeroed_std_lwr, ymax=zeroed_std_upr)) +
  geom_point(aes(y=zeroed_std),size=1.5,color="red") +
  scale_x_log10()+
  labs(y="mean(tongue-corolla), mm",x="bee tongue length (mm)") +
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
############### Test for trait matching with the entire network 
####(approach similar to Sazatornil et al 2016)
means <- numeric(iterations+2)
sds <- numeric(iterations+2)
means[iterations+1] <- mean((datatotal[,(iterations+3)]))#add observed value
sds[iterations+1] <- sd(datatotal[,(iterations+3)])#add observed value
means[iterations+2] <- mean((datatotal[,(iterations+4)]))#add observed value assuming 0 difference for small bees
sds[iterations+2] <- sd(datatotal[,(iterations+4)])#add observed value assuming 0 difference for small bees


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



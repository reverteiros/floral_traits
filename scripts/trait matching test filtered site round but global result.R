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

rawsdz<-bysp %>% ggplot(aes(tongue.x, z(difference_std,difference_std_av, difference_std_std))) + 
  geom_point() +
  scale_x_log10()+
  labs(y="z-score(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  # geom_smooth(method="glm")+
  theme_classic()

zerosd<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=zeroed_std_av)) +
  geom_errorbar(aes(ymin=zeroed_std_lwr, ymax=zeroed_std_upr)) +
  geom_point(aes(y=zeroed_std),size=1.5,color="red") +
  scale_x_log10()+
  labs(y="mean(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

zerosdz<-bysp %>% ggplot(aes(tongue.x, z(zeroed_std,zeroed_std_av, zeroed_std_std))) + 
  geom_point() +
  scale_x_log10()+
  labs(y="z-score(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  theme_classic()


#look at, e.g. correlation between effect size for SD or mean (tongue-corolla) from tongue
# a <- lm(z(difference_std,difference_std_av, difference_std_std)~log(tongue.x), data=bysp)
# plot(a)
# summary(a)
# 
# b <- lm(z(difference_av,difference_av_av, difference_av_std)~log(tongue.x), data=bysp)
# plot(b)
# summary(b)

#### Plot mean difference vs SD per each bee species 
k<-obs %>% group_by(bee) %>% summarize(ave=abs(av(difference)), varx=std(difference))
  
bysp %>% ggplot(aes(abs(difference_av_av), difference_std_av))+
  geom_point()+
  # geom_errorbar(aes(x=abs(difference_av_av),ymin=difference_std_lwr, ymax=difference_std_upr))+
  # geom_errorbarh(aes(y=difference_std_av,xmin=abs(difference_av_lwr), xmax=abs(difference_av_upr)))+
  geom_point(aes(ave, varx), data=k,color="red")+ 
  # scale_x_continuous(limits=c(0,8))+
  # geom_jitter(height=0.1) + 
  theme_classic()+
  # geom_smooth(method=lm)+
  labs(x="Mean (tongue-corolla), mm", y="sd(tongue-corolla), mm")

c <- lm(varx~ave, data=(obs %>% group_by(bee) %>% summarize(ave=abs(av(difference)), varx=std(difference))))
plot(c)
summary(c)

# bysp %>% ggplot()+ 
#   # geom_jitter(height=0.1) + 
#   geom_point()+
#   theme_classic()+
#   geom_smooth(method=lm)+
#   labs(x="Mean difference (tongue length minus flower depth, mm)", y="SD difference (mm)")

d <- lm(difference_std_av~abs(difference_av_av), data=bysp[-75,])
plot(d)
summary(d)

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



# this script generates simulated visits for each bee species based on the flower visit frequencies at all site rounds at which the focal bee was detected. 
# it produces a bunch of plots for the differences between bee tongue length and flower corolla for the observed and simulated data.
# to do: look at mean vs. SD difference in simulated data. 

source("scripts/traits.R")

library(cowplot)

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
  geom_point(aes(y=difference_av),color="red") +
  scale_x_log10()+
  scale_y_continuous(limits=c(-14,10))+
  labs(y="mean(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

rawmeanz<-bysp %>% ggplot(aes(tongue.x, z(difference_av,difference_av_av, difference_av_std))) + 
  geom_point() +
  scale_x_log10()+
  scale_y_continuous(limits=c(-22,22))+
  labs(y="z-score(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  theme_classic()

zeromean<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=zeroed_av_av)) +
  geom_errorbar(aes(ymin=zeroed_av_lwr, ymax=zeroed_av_upr)) +
  geom_point(aes(y=zeroed_av),color="red") +
  scale_x_log10()+
  scale_y_continuous(limits=c(-14,10))+
  labs(y="mean(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

zeromeanz<-bysp %>% ggplot(aes(tongue.x, z(zeroed_av,zeroed_av_av, zeroed_av_std))) + 
  geom_point() +
  scale_x_log10()+
  scale_y_continuous(limits=c(-22,22))+
  labs(y="z-score(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  theme_classic()

#####
#fig 3
#####
pdf(file="figures/fig3.pdf", 9,8)
plot_grid(rawmean, rawmeanz, zeromean, zeromeanz, ncol=2, labels="auto")
dev.off()


rawsd<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=difference_std_av)) +
  geom_errorbar(aes(ymin=difference_std_lwr, ymax=difference_std_upr)) +
  geom_point(aes(y=difference_std),color="red") +
  scale_x_log10()+
  scale_y_continuous(limits=c(0,14))+
  labs(y="sd(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

rawsdz<-bysp %>% ggplot(aes(tongue.x, z(difference_std,difference_std_av, difference_std_std))) + 
  geom_point() +
  scale_x_log10()+
  scale_y_continuous(limits=c(-15,15))+
  labs(y="z-score: sd(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  # geom_smooth(method="glm")+
  theme_classic()

zerosd<-bysp %>% ggplot(aes(tongue.x)) + 
  geom_point(aes(y=zeroed_std_av)) +
  geom_errorbar(aes(ymin=zeroed_std_lwr, ymax=zeroed_std_upr)) +
  geom_point(aes(y=zeroed_std),color="red") +
  scale_y_continuous(limits=c(0,14))+
  scale_x_log10()+
  labs(y="mean(tongue-corolla), mm",x="bee tongue length (mm)") +
  theme_classic()

zerosdz<-bysp %>% ggplot(aes(tongue.x, z(zeroed_std,zeroed_std_av, zeroed_std_std))) + 
  geom_point() +
  scale_x_log10()+
  scale_y_continuous(limits=c(-15,15))+
  labs(y="z-score: sd(tongue-corolla), mm",x="bee tongue length (mm)") +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-1.96)+
  geom_hline(yintercept=1.96)+
  theme_classic()

pdf(file="figures/fig4.pdf", 9,8)
plot_grid(rawsd, rawsdz, zerosd, zerosdz, ncol=2, labels="auto")
dev.off()
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
  
mean_variance_scaling<-bysp %>% ggplot(aes(abs(difference_av_av), difference_std_av))+
  # geom_point()+
  # geom_errorbar(aes(x=abs(difference_av_av),ymin=difference_std_lwr, ymax=difference_std_upr))+
  # geom_errorbarh(aes(y=difference_std_av,xmin=abs(difference_av_lwr), xmax=abs(difference_av_upr)))+
  geom_point(aes(ave, varx), data=k,color="red")+ 
  scale_x_continuous(limits=c(0,12))+
  scale_y_continuous(limits=c(0,8))+
  # geom_jitter(height=0.1) + 
  theme_classic()+
  # geom_smooth(aes(ave, varx), data=k,method=lm)+
  labs(x="Mean (tongue-corolla), mm", y="sd(tongue-corolla)")

mean_variance_null<-bysp %>% ggplot(aes(abs(difference_av_av), difference_std_av))+
  geom_point()+
  # geom_errorbar(aes(x=abs(difference_av_av),ymin=difference_std_lwr, ymax=difference_std_upr))+
  # geom_errorbarh(aes(y=difference_std_av,xmin=abs(difference_av_lwr), xmax=abs(difference_av_upr)))+
  # geom_point(aes(ave, varx), data=k)+#,color="red")+ 
  scale_x_continuous(limits=c(0,12))+
  scale_y_continuous(limits=c(0,8))+
  # geom_jitter(height=0.1) + 
  theme_classic()+
  # geom_smooth(method=lm)+
  labs(x="Mean (tongue-corolla), mm", y="sd(tongue-corolla)")

################
### fig 5
#######
pdf(file="figures/fig5.pdf",9,4)
plot_grid(mean_variance_scaling, mean_variance_null, labels="auto")
dev.off()

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


#Graphs to test if observed values are different from random

global_hist<-comb %>% mutate(datasrc=iter<7777777) %>% ggplot(aes(difference,fill=datasrc))+ 
  scale_fill_discrete(labels=c("observed", "null model"), name="")+
  # geom_freqpoly(stat="density")+
  geom_density(alpha=0.5, kernel="epanechnikov", n=60)+
  labs(x="tongue length minus flower depth, mm", y="visit \"density\" (height proportional to visit frequency)")+
  # theme(legend.title = element_blank())+
  theme_classic()

pdf(file="figures/fig1.pdf")
print(global_hist)
dev.off()


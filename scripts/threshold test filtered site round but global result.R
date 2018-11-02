#This is the script for generating plots for observed and expected proportion of interactions with forbidden flowers

#for making multi-panel plots
library(cowplot)

#make data
source("scripts/traits.R")

#generate null
###################
### WARNING######
# this may not run as-is on any machine AND at 999 iterations takes a very long time. 
source("scripts/null model trait matching.R")

## Now we have the null model distributions per each interaction with the flowers that each individual bee can face at the site-round that was present, it's time to work with the data. 

# Ask for the proportion of cells in each iteration in the null model and for each bee species in which flower - tongue >0 (this way 0 or negative means flower was accessible)
datamatrixtable <- datatotal %>% select(-sr) %>% 
  group_by(bee, tongue, iter) %>%
  summarize_all(function(x){(mean(x<=0, na.rm=T))})

# Mean, SD and quantiles of the null models 
upr<-function(x){quantile(x, .975, na.rm=T)}
lwr<-function(x){quantile(x, .025, na.rm=T)}
av<-function(x){mean(x, na.rm=T)}
std<-function(x){sd(x, na.rm=T)}
bysp<-datamatrixtable %>% group_by(bee, tongue) %>% summarize_all(funs(av, std, upr, lwr))

# Generate a matrix with each bee one time, with tongue length of each
obs<-generaldata %>%
  mutate(big=(IT_improved-width)>0, zeroed=ifelse(difference<0, difference*big, difference),deleted=ifelse(difference<0, ifelse(big, difference, NA), difference))  %>% 
  group_by(bee) %>%
  summarize(raw_mismatch=mean(difference<0), zeroed=mean(zeroed<0), deleted=mean(deleted<0, na.rm=T), abundance=n()) %>%
  filter(abundance > 4)

# Include observations 
datamatrixtable2 <- dplyr::right_join(bysp,obs,by="bee")



# Plot proportion of times that flower are longer than tongues, black arrows are error bars from null models and red dots are observed values
a<-datamatrixtable2 %>%
  ggplot(aes(x=tongue))+
  geom_errorbar(aes(ymin=raw_t_minuts_d_lwr, ymax=raw_t_minuts_d_upr), colour="black") +
  geom_point(aes(y=raw_t_minuts_d_av),col="black") +
  geom_point(aes(y=raw_mismatch),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  scale_x_log10()+
  theme_classic() 

#where crawl-in bee interactions get a floor of 0
b<-datamatrixtable2 %>%
  ggplot(aes(x=tongue))+
  # geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=zero_lwr, ymax=zero_upr), colour="black") +
  geom_point(aes(y=zero_av),col="black") +
  geom_point(aes(y=zeroed),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  scale_x_log10()+
  theme_classic() 

#crawlers eliminated
c<-datamatrixtable2 %>%
  ggplot(aes(x=tongue))+
  # geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=remove_lwr, ymax=remove_upr), colour="black") +
  geom_point(aes(y=remove_av),col="black") +
  geom_point(aes(y=deleted),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  scale_x_log10()+
  theme_classic() 

pdf(file="figures/Threshold.pdf", 8,3,pointsize = 2 )
plot_grid(a,b,c, ncol=3, labels="auto")
dev.off()


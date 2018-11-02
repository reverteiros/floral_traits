#This is the script for generating plots for observed and expected proportion of interactions with forbidden flowers
source("scripts/traits.R")

library(ggplot2)
library(purrr)

#This was done incorrectly, I think
# Removing small bees is not something we want to do before generating the null model, 
#it's a filter we would put on afterwards to both null and observed, I think. Hmm. This is tricky. 
## Definitely doesn't seem exactly right to remove some interactions from null but not others. 
## Choose at the beginning if we want to remove small bees that can crawl in
# generaldata <- generaldata %>% 
#   dplyr::filter(., beewider== "true")
# 
# 
# generaldata$difference <- generaldata$depth-generaldata$tongue_length.tongue
# 
# generaldata <- generaldata%>% 
#   mutate(newdifference=if_else(beewider== "true", difference, 0))
# 

#I think the null models are the same. How about switch to 'null model trait matching.R'?
# source("scripts/null model threshold test.R")
source("scripts/null model trait matching.R")

## Now we have the null model distributions per each interaction with the flowers that each individual bee can face at the site-round that was present, it's time to work with the data. 

# Ask for the proportion of cells in each iteration in the null model and for each bee species in which the difference between flower and tongue favours the flower (is positive)
datamatrixtable <- datatotal %>%
  group_by(bee) %>%
  summarize_all(function(x){(mean(x>0))})

# Mean, SD and quantiles of the null models 
#this needs to be more flexible to match # iterations!
datamatrixtable$mean <- apply(datamatrixtable[,2:(iterations+1)],1,FUN=mean)
datamatrixtable$sd <- apply(datamatrixtable[,2:(iterations+1)],1,FUN=sd)
datamatrixtable$quantile975 <- apply(datamatrixtable[,2:1000],1,quantile,probs=c(.975))
datamatrixtable$quantile25 <- apply(datamatrixtable[,2:1000],1,quantile,probs=c(.025))

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datamatrixtable$tongue <- NULL

# Generate a matrix with each bee one time, with tongue length of each
databeesall<-generaldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) %>%
  filter(abundance > 4)

# Include tongue length to dataframe
datamatrixtable <- dplyr::right_join(datamatrixtable,databeesall,"bee")

# Plot proportion of times that flower are longer than tongues, black arrows are error bars from null models and red dots are observed values
datamatrixtable %>%
  ggplot(aes(x=tongue))+
  # geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=difference),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  theme_classic() 

# Plot proportion of times that flower are longer than tongues, black arrows are error bars from null models and red dots are observed values. In this case we consider the small bees that can crawl in a difference of 0, so flowers are not longer than tongues
datamatrixtable %>%
  ggplot(aes(x=tongue))+
  # geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=newdifference),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  theme_classic() 


# ## If we want to separet it at the site-round level
# 
# datamatrixtable %>%
#   ggplot(aes(x=tongue))+
#   geom_point(aes(y=mean)) +
#   geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
#   geom_point(aes(y=difference),col="red") +
#   labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
#   theme_classic() +
#   facet_wrap(~round+site, ncol=6)
# 
# datamatrixtable %>%
#   ggplot(aes(x=tongue))+
#   geom_point(aes(y=mean)) +
#   geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
#   geom_point(aes(y=newdifference),col="red") +
#   labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
#   theme_classic() +
#   facet_wrap(~round+site, ncol=6)



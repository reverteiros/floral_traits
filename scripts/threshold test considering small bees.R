
source("scripts/traits.R")

library(ggplot2)
library(purrr)

# read and manipulate data
alldata <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))
alldata$difference <- alldata$depth-alldata$tongue_length.tongue


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
  dplyr::filter(., abundance > 5)

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
datamatrix$newdifference <- filtered$newdifference

# Ask for the proportion of cells in each iteration in the null model and for each bee species in which the difference between flower and tongue favours the flower (is positive)
datamatrixtable <- datamatrix %>%
  group_by(bee) %>%
  summarize_all(function(x){(mean(x>0))})

# Mean, SD and quantiles of the null models
datamatrixtable$mean <- apply(datamatrixtable[,2:(iterations+1)],1,FUN=mean)
datamatrixtable$sd <- apply(datamatrixtable[,2:(iterations+1)],1,FUN=sd)
datamatrixtable$quantile975 <- apply(datamatrixtable[,2:1000],1,quantile,probs=c(.975))
datamatrixtable$quantile25 <- apply(datamatrixtable[,2:1000],1,quantile,probs=c(.025))

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datamatrixtable$tongue <- NULL
datamatrixtable <- dplyr::left_join(datamatrixtable,databees,"bee")

# Plot graph with means of proportion of flowers longer than tongues derived from null model with error bars and real data
datamatrixtable %>%
  ggplot(aes(x=tongue))+
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=newdifference),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  theme_classic()
# sometimes the plot gives error, run the previous line and the plot again and is fine
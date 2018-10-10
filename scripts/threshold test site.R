
source("scripts/traits.R")

library(ggplot2)
library(purrr)

dataglobal <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))
dataglobal$difference <- dataglobal$depth-dataglobal$tongue_length.tongue

# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends
dataglobal<-dataglobal %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72)) %>% mutate(beewider=if_else(IT_improved>width, "true", "false")) 


######## Choose one of the following options: 
## Assume differences of 0 for small bees that can crawl in
dataglobal <- dataglobal%>% 
  mutate(newdifference=if_else(beewider== "true", difference, 0))

## Eliminate small bees that can crawl in
dataglobal <- dataglobal %>% 
  dplyr::filter(., beewider== "true")

################ Baldpate #################################################

## set number of iterations
iterations <- 999

# read and manipulate data
alldata <- dplyr::filter(dataglobal, !is.na(depth)&!is.na(tongue_length.tongue)&site=="Baldpate")
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

### create objects at the species level, with abundance, and mean traits (subset bee species with more than 5 individuals collected to avoid species with high errors due to small size)
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) %>%
  dplyr::filter(., abundance > 3)

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model, so matrix dimensions 14201*1000 (include a column for bee tongue length)
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
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

datamatrixtable1 <- dplyr::mutate(datamatrixtable, site="Baldpate")





################ Cold Soil ################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal, !is.na(depth)&!is.na(tongue_length.tongue)&site=="Cold Soil")
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

### create objects at the species level, with abundance, and mean traits (subset bee species with more than 5 individuals collected to avoid species with high errors due to small size)
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  %>%
  dplyr::filter(., abundance > 3)

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model, so matrix dimensions 14201*1000 (include a column for bee tongue length)
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
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

datamatrixtable2 <- dplyr::mutate(datamatrixtable, site="Cold Soil")




################ Fox Hill #################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal, !is.na(depth)&!is.na(tongue_length.tongue)&site=="Fox Hill")
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

### create objects at the species level, with abundance, and mean traits (subset bee species with more than 5 individuals collected to avoid species with high errors due to small size)
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) %>%
  dplyr::filter(., abundance > 3)

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model, so matrix dimensions 14201*1000 (include a column for bee tongue length)
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
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

datamatrixtable3 <- dplyr::mutate(datamatrixtable, site="Fox Hill")





################ IAS - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal, !is.na(depth)&!is.na(tongue_length.tongue)&site=="IAS")
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

### create objects at the species level, with abundance, and mean traits (subset bee species with more than 5 individuals collected to avoid species with high errors due to small size)
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  %>%
  dplyr::filter(., abundance > 3)

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model, so matrix dimensions 14201*1000 (include a column for bee tongue length)
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
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

datamatrixtable4 <- dplyr::mutate(datamatrixtable, site="IAS")




################ Lord Stirling #####################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal, !is.na(depth)&!is.na(tongue_length.tongue)&site=="Lord Stirling")
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

### create objects at the species level, with abundance, and mean traits (subset bee species with more than 5 individuals collected to avoid species with high errors due to small size)
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  %>%
  dplyr::filter(., abundance > 3)

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model, so matrix dimensions 14201*1000 (include a column for bee tongue length)
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
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

datamatrixtable5 <- dplyr::mutate(datamatrixtable, site="Lord Stirling")




################ URWA ######################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal, !is.na(depth)&!is.na(tongue_length.tongue)&site=="URWA")
alldata$difference <- alldata$depth-alldata$tongue_length.tongue

### create objects at the species level, with abundance, and mean traits (subset bee species with more than 5 individuals collected to avoid species with high errors due to small size)
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  %>%
  dplyr::filter(., abundance > 3)

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model, so matrix dimensions 14201*1000 (include a column for bee tongue length)
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
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

datamatrixtable6 <- dplyr::mutate(datamatrixtable, site="URWA")





datatotal <- dplyr::bind_rows(datamatrixtable1, datamatrixtable2, datamatrixtable3, datamatrixtable4, datamatrixtable5, datamatrixtable6)



datatotal %>%
  ggplot(aes(x=tongue))+
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=difference),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  theme_classic() +
  facet_wrap(~site, ncol=3)

datatotal %>%
  ggplot(aes(x=tongue))+
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=newdifference),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  theme_classic() +
  facet_wrap(~site, ncol=3)

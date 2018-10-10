
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

################ Baldpate - Round 1################################################################

## set number of iterations
iterations <- 999

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Baldpate")


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

datamatrixtable11 <- dplyr::mutate(datamatrixtable, site="Baldpate",round="1")



############################## Baldpate - Round 2##################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Baldpate")


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

datamatrixtable12 <- dplyr::mutate(datamatrixtable, site="Baldpate",round="2")



################ Baldpate - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Baldpate")


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

datamatrixtable13 <- dplyr::mutate(datamatrixtable, site="Baldpate",round="3")



################ Baldpate - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Baldpate")


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

datamatrixtable14 <- dplyr::mutate(datamatrixtable, site="Baldpate",round="4")



################ Baldpate - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Baldpate")


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

datamatrixtable15 <- dplyr::mutate(datamatrixtable, site="Baldpate",round="5")



################ Cold Soil - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Cold Soil")


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

datamatrixtable21 <- dplyr::mutate(datamatrixtable, site="Cold Soil",round="1")



################ Cold Soil - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Cold Soil")


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

datamatrixtable22 <- dplyr::mutate(datamatrixtable, site="Cold Soil",round="2")



################ Cold Soil - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Cold Soil")


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

datamatrixtable23 <- dplyr::mutate(datamatrixtable, site="Cold Soil",round="3")



################ Cold Soil - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Cold Soil")


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

datamatrixtable24 <- dplyr::mutate(datamatrixtable, site="Cold Soil",round="4")



################ Cold Soil - Round ################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Cold Soil")


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

datamatrixtable25 <- dplyr::mutate(datamatrixtable, site="Cold Soil",round="5")



################ Fox Hill - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Fox Hill")


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

datamatrixtable31 <- dplyr::mutate(datamatrixtable, site="Fox Hill",round="1")



################ Fox Hill - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Fox Hill")


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

datamatrixtable32 <- dplyr::mutate(datamatrixtable, site="Fox Hill",round="2")



################ Fox Hill - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Fox Hill")


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

datamatrixtable33 <- dplyr::mutate(datamatrixtable, site="Fox Hill",round="3")



################ Fox Hill - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Fox Hill")


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

datamatrixtable34 <- dplyr::mutate(datamatrixtable, site="Fox Hill",round="4")



################ Fox Hill - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Fox Hill")


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

datamatrixtable35 <- dplyr::mutate(datamatrixtable, site="Fox Hill",round="5")



################ IAS - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="IAS")


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

datamatrixtable41 <- dplyr::mutate(datamatrixtable, site="IAS",round="1")



################ IAS - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="IAS")


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

datamatrixtable42 <- dplyr::mutate(datamatrixtable, site="IAS",round="2")



################ IAS - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="IAS")


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

datamatrixtable43 <- dplyr::mutate(datamatrixtable, site="IAS",round="3")



################ IAS - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="IAS")


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

datamatrixtable44 <- dplyr::mutate(datamatrixtable, site="IAS",round="4")



################ IAS - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="IAS")


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

datamatrixtable45 <- dplyr::mutate(datamatrixtable, site="IAS",round="5")



################ Lord Stirling - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Lord Stirling")


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

datamatrixtable51 <- dplyr::mutate(datamatrixtable, site="Lord Stirling",round="1")



################ Lord Stirling - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Lord Stirling")


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

datamatrixtable52 <- dplyr::mutate(datamatrixtable, site="Lord Stirling",round="2")



################ Lord Stirling - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Lord Stirling")


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

datamatrixtable53 <- dplyr::mutate(datamatrixtable, site="Lord Stirling",round="3")



################ Lord Stirling - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Lord Stirling")


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

datamatrixtable54 <- dplyr::mutate(datamatrixtable, site="Lord Stirling",round="4")




################ Lord Stirling - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Lord Stirling")


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

datamatrixtable55 <- dplyr::mutate(datamatrixtable, site="Lord Stirling",round="5")



################ URWA - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="URWA")


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

datamatrixtable61 <- dplyr::mutate(datamatrixtable, site="URWA",round="1")



################ URWA - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="URWA")


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

datamatrixtable62 <- dplyr::mutate(datamatrixtable, site="URWA",round="2")



################ URWA - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="URWA")


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

datamatrixtable63 <- dplyr::mutate(datamatrixtable, site="URWA",round="3")



################ URWA - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="URWA")


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

datamatrixtable64 <- dplyr::mutate(datamatrixtable, site="URWA",round="4")



################ URWA - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="URWA")


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

datamatrixtable65 <- dplyr::mutate(datamatrixtable, site="URWA",round="5")


datatotal <- dplyr::bind_rows(datamatrixtable11, datamatrixtable12, datamatrixtable13, datamatrixtable14, datamatrixtable15, datamatrixtable21, datamatrixtable22, datamatrixtable23, datamatrixtable24, datamatrixtable25, datamatrixtable31, datamatrixtable32, datamatrixtable33, datamatrixtable34, datamatrixtable35, datamatrixtable41, datamatrixtable42, datamatrixtable43, datamatrixtable44, datamatrixtable45, datamatrixtable51, datamatrixtable52, datamatrixtable53, datamatrixtable54, datamatrixtable55, datamatrixtable61, datamatrixtable62, datamatrixtable63, datamatrixtable64, datamatrixtable65)


datamatrixtable %>%
  ggplot(aes(x=tongue))+
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=difference),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  theme_classic() +
  facet_wrap(~round+site, ncol=6)

datamatrixtable %>%
  ggplot(aes(x=tongue))+
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile975), colour="black", width=.1) +
  geom_point(aes(y=newdifference),col="red") +
  labs(y="Proportion of flowers > tongues",x="Bee tongue length (mm)") +
  theme_classic() +
  facet_wrap(~round+site, ncol=6)


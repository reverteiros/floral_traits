# this script generates simulated visits for each bee species based on the flower visit frequencies at all site rounds at which the focal bee was detected. 
# it produces a bunch of plots for the differences between bee tongue length and flower corolla for the observed and simulated data.
# to do: look at mean vs. SD difference in simulated data. 
source("scripts/traits.R")

library(ggplot2)
library(purrr)


dataglobal <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))
dataglobal$difference <- dataglobal$tongue_length.tongue-dataglobal$depth

# Modify bee IT with the estimate of the regression between head width and bee IT. Regressions apart for Bombus and Xylocopa, since they show different trends
dataglobal<-dataglobal %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72)) %>% mutate(beewider=if_else(IT_improved>width, "true", "false")) 


######## Choose one of the following options: 
## Assume differences of 0 for small bees that can crawl in
dataglobal <- dataglobal%>% 
  mutate(newdifference=if_else(beewider== "true", difference, 0))

## Eliminate small bees that can crawl in
dataglobal <- dataglobal %>% 
  dplyr::filter(., beewider== "true")


################ Baldpate - Round 1#############################################################

## set number of iterations
iterations <- 999

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Baldpate")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable11 <- dplyr::mutate(datamatrix, site="Baldpate",round="1")


############################## Baldpate - Round 2##################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Baldpate")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable12 <- dplyr::mutate(datamatrix, site="Baldpate",round="2")


################ Baldpate - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Baldpate")


### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable13 <- dplyr::mutate(datamatrix, site="Baldpate",round="3")


################ Baldpate - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Baldpate")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable14 <- dplyr::mutate(datamatrix, site="Baldpate",round="4")


################ Baldpate - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Baldpate")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable15 <- dplyr::mutate(datamatrix, site="Baldpate",round="5")


################ Cold Soil - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Cold Soil")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable21 <- dplyr::mutate(datamatrix, site="Cold Soil",round="1")


################ Cold Soil - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Cold Soil")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable22 <- dplyr::mutate(datamatrix, site="Cold Soil",round="2")


################ Cold Soil - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Cold Soil")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable23 <- dplyr::mutate(datamatrix, site="Cold Soil",round="3")


################ Cold Soil - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Cold Soil")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable24 <- dplyr::mutate(datamatrix, site="Cold Soil",round="4")


################ Cold Soil - Round ################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Cold Soil")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable25 <- dplyr::mutate(datamatrix, site="Cold Soil",round="5")


################ Fox Hill - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Fox Hill")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable31 <- dplyr::mutate(datamatrix, site="Fox Hill",round="1")


################ Fox Hill - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Fox Hill")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable32 <- dplyr::mutate(datamatrix, site="Fox Hill",round="2")


################ Fox Hill - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Fox Hill")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable33 <- dplyr::mutate(datamatrix, site="Fox Hill",round="3")


################ Fox Hill - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Fox Hill")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable34 <- dplyr::mutate(datamatrix, site="Fox Hill",round="4")


################ Fox Hill - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Fox Hill")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable35 <- dplyr::mutate(datamatrix, site="Fox Hill",round="5")


################ IAS - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="IAS")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable41 <- dplyr::mutate(datamatrix, site="IAS",round="1")


################ IAS - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="IAS")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable42 <- dplyr::mutate(datamatrix, site="IAS",round="2")


################ IAS - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="IAS")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable43 <- dplyr::mutate(datamatrix, site="IAS",round="3")


################ IAS - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="IAS")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable44 <- dplyr::mutate(datamatrix, site="IAS",round="4")


################ IAS - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="IAS")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable45 <- dplyr::mutate(datamatrix, site="IAS",round="5")


################ Lord Stirling - Round 1#############################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="Lord Stirling")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable51 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="1")


################ Lord Stirling - Round 2#############################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="Lord Stirling")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable52 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="2")


################ Lord Stirling - Round 3#######################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="Lord Stirling")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

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
    b <- databees$tongue[x]- a
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

datamatrixtable53 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="3")


################ Lord Stirling - Round 4#######################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="Lord Stirling")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable54 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="4")



################ Lord Stirling - Round 5##########################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="Lord Stirling")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable55 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="5")


################ URWA - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==1&site=="URWA")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable61 <- dplyr::mutate(datamatrix, site="URWA",round="1")


################ URWA - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==2&site=="URWA")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable62 <- dplyr::mutate(datamatrix, site="URWA",round="2")


################ URWA - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==3&site=="URWA")

### create objects at the species level, with abundance, and mean traits
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable63 <- dplyr::mutate(datamatrix, site="URWA",round="3")


################ URWA - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==4&site=="URWA")

### create objects at the species level, with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())  

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable64 <- dplyr::mutate(datamatrix, site="URWA",round="4")


################ URWA - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(dataglobal,  sampling_round==5&site=="URWA")

### create objects at the species level with abundance, and mean traits 
databees<-alldata %>%
  group_by(bee) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n())

dataflowers<-alldata %>%
  group_by(plant_gs) %>%
  summarize(depth=mean(depth),abundance=n())

filtered <- dplyr::inner_join(alldata,databees, by = "bee")
filtered <- filtered[order(filtered$bee),] 

## Create matrix to insert null models. 999 runs of the null model
datamatrix <- matrix(ncol = iterations,nrow = sum(databees$abundance))
datamatrix <- as.data.frame(datamatrix)

for(i in 1:iterations){
  species <- lapply(1:length(databees$bee),function(x){
    a <- sample(dataflowers$depth, databees$abundance[x], replace = T, prob = dataflowers$abundance)
    b <- databees$tongue[x]- a
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

datamatrixtable65 <- dplyr::mutate(datamatrix, site="URWA",round="5")


####################### Join entire dataset##################################

datatotal <- dplyr::bind_rows(datamatrixtable11, datamatrixtable12, datamatrixtable13, datamatrixtable14, datamatrixtable15, datamatrixtable21, datamatrixtable22, datamatrixtable23, datamatrixtable24, datamatrixtable25, datamatrixtable31, datamatrixtable32, datamatrixtable33, datamatrixtable34, datamatrixtable35, datamatrixtable41, datamatrixtable42, datamatrixtable43, datamatrixtable44, datamatrixtable45, datamatrixtable51, datamatrixtable52, datamatrixtable53, datamatrixtable54, datamatrixtable55, datamatrixtable61, datamatrixtable62, datamatrixtable63, datamatrixtable64, datamatrixtable65)

# Filter species that appear less than 5 times
datatotalbees<-datatotal %>%
  group_by(bee) %>%
  summarize(abundance=n()) %>%
  dplyr::filter(., abundance > 4)

datatotal <- dplyr::inner_join(datatotal, datatotalbees, by = "bee")

## Now we have the null model distributions per each interaction with the flowers that each individual bee can face at the site-round that was present, it's time to work with the data. 

############### Test for trait matching with the entire network 
####(approach similar to Sazatornil et al 2016)
means <- numeric(iterations+1)
sds <- numeric(iterations+1)
means[iterations+1] <- mean(abs(datatotal[,(iterations+3)]))#add observed value
sds[iterations+1] <- sd(datatotal[,(iterations+3)])#add observed value

# generate means and sd of each entire null network, 999 replicates
for(i in 1:iterations){
  means[i] <- mean(datatotal[,i])
  sds[i] <- sd(datatotal[,i])
}

#Graphs to test if observed values are different from random
hist(means[1:iterations],main="Mean difference (tongue length minus flower depth, mm)",xlab="",ylab="")# run both lines together or does not work
abline(v=means[iterations+1])
hist(sds[1:iterations],xlim=c(5,7),main=" SD difference (tongue length minus flower depth, mm)",xlab="",ylab="")
abline(v=sds[iterations+1])
## mean is in the random distribution, but observed sd is very different than random sds.
## observed sd is way smaller than random, indicating some trait matching.


######### Test for trait matching at the species level. 
## Do it for each bee species, to see if some tongues trait match more

nullperbee<-datatotal[order(datatotal$bee),] 
nullperbee$bee <- factor(nullperbee$bee)
nullperbee$beenumeric <- as.numeric(nullperbee$bee)

observed<-nullperbee %>%
  group_by(bee) %>%
  summarize(mean_obs=mean(difference),sd_obs=sd(difference),abundance=n())

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

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datameans <- dplyr::left_join(meanpersp,observed,"bee")

# when we did summary we had a column of tongues, but it turned to ones. Remove and insert again
datameans$tongue <- NULL

# Generate a matrix with each bee one time, with tongue length of each
databeesall<-dataglobal %>%
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


#### Plot mean difference vs SD per each bee species 
observed %>% mutate(absdif=abs(mean_obs)) %>% ggplot(aes(x=absdif, y=sd_obs)) + 
  geom_jitter(height=0.1) + 
  theme_classic()+
  geom_smooth(method=lm)+
  labs(y="Mean difference (tongue length minus flower depth, mm)", x="SD difference (mm)")



########### OK, we see that some observed values are far away from the 2.5% and the 97.5% quantiles. Kind of looks like the sign of the difference between the observed value and the 2.5% and 97.5% quantiles depends on the tongue of the bees, in that short-tongued bees "trait match more" = the observed value tend to be above the 97.5% quantile, while long-tongued bees tend to be under the 2.5% quantile. Let's check that:
datamatrixmeans$more975 <- datamatrixmeans$mean_obs - datamatrixmeans$quantile975
datamatrixmeans$less25 <- datamatrixmeans$mean_obs - datamatrixmeans$quantile25

datamatrixmeans$upper <- dplyr::if_else(datamatrixmeans$more975>0, datamatrixmeans$more975, 0)
datamatrixmeans$lower <- dplyr::if_else(datamatrixmeans$less25<0, datamatrixmeans$less25,0)

datamatrixmeans$variation <- datamatrixmeans$upper + datamatrixmeans$lower

# 
ggplot(datamatrixmeans, aes(y=variation, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  theme_bw(base_size=16) + 
  labs(y="Distance between the observed values and the confidence interval",x="Bee tongue length (mm)") +
  theme_classic()

### same for SD
datamatrixsd$more975 <- datamatrixsd$sd_obs - datamatrixsd$quantile975
datamatrixsd$less25 <- datamatrixsd$sd_obs - datamatrixsd$quantile25

datamatrixsd$upper <- dplyr::if_else(datamatrixsd$more975>0, datamatrixsd$more975, 0)
datamatrixsd$lower <- dplyr::if_else(datamatrixsd$less25<0, datamatrixsd$less25,0)

datamatrixsd$variation <- datamatrixsd$upper + datamatrixsd$lower

# 
ggplot(datamatrixsd, aes(y=variation, x=tongue)) + 
  geom_point(size=1.5,color="red") +
  theme_bw(base_size=16) + 
  labs(y="Distance between the observed values and the confidence interval",x="Bee tongue length (mm)") +
  theme_classic()

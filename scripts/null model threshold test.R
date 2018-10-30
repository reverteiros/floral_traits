
library(purrr)

## set number of iterations
iterations <- 999

################ Baldpate - Round 1#############################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==1&site=="Baldpate")

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

datamatrixtable11 <- dplyr::mutate(datamatrix, site="Baldpate",round="1")


############################## Baldpate - Round 2##################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==2&site=="Baldpate")

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

datamatrixtable12 <- dplyr::mutate(datamatrix, site="Baldpate",round="2")


################ Baldpate - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==3&site=="Baldpate")


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

datamatrixtable13 <- dplyr::mutate(datamatrix, site="Baldpate",round="3")


################ Baldpate - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==4&site=="Baldpate")

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

datamatrixtable14 <- dplyr::mutate(datamatrix, site="Baldpate",round="4")


################ Baldpate - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==5&site=="Baldpate")

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

datamatrixtable15 <- dplyr::mutate(datamatrix, site="Baldpate",round="5")


################ Cold Soil - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==1&site=="Cold Soil")

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

datamatrixtable21 <- dplyr::mutate(datamatrix, site="Cold Soil",round="1")


################ Cold Soil - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==2&site=="Cold Soil")

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

datamatrixtable22 <- dplyr::mutate(datamatrix, site="Cold Soil",round="2")


################ Cold Soil - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==3&site=="Cold Soil")

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

datamatrixtable23 <- dplyr::mutate(datamatrix, site="Cold Soil",round="3")


################ Cold Soil - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==4&site=="Cold Soil")

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

datamatrixtable24 <- dplyr::mutate(datamatrix, site="Cold Soil",round="4")


################ Cold Soil - Round ################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==5&site=="Cold Soil")

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

datamatrixtable25 <- dplyr::mutate(datamatrix, site="Cold Soil",round="5")


################ Fox Hill - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==1&site=="Fox Hill")

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

datamatrixtable31 <- dplyr::mutate(datamatrix, site="Fox Hill",round="1")


################ Fox Hill - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==2&site=="Fox Hill")

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

datamatrixtable32 <- dplyr::mutate(datamatrix, site="Fox Hill",round="2")


################ Fox Hill - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==3&site=="Fox Hill")

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

datamatrixtable33 <- dplyr::mutate(datamatrix, site="Fox Hill",round="3")


################ Fox Hill - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==4&site=="Fox Hill")

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

datamatrixtable34 <- dplyr::mutate(datamatrix, site="Fox Hill",round="4")


################ Fox Hill - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==5&site=="Fox Hill")

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

datamatrixtable35 <- dplyr::mutate(datamatrix, site="Fox Hill",round="5")


################ IAS - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==1&site=="IAS")

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

datamatrixtable41 <- dplyr::mutate(datamatrix, site="IAS",round="1")


################ IAS - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==2&site=="IAS")

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

datamatrixtable42 <- dplyr::mutate(datamatrix, site="IAS",round="2")


################ IAS - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==3&site=="IAS")

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

datamatrixtable43 <- dplyr::mutate(datamatrix, site="IAS",round="3")


################ IAS - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==4&site=="IAS")

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

datamatrixtable44 <- dplyr::mutate(datamatrix, site="IAS",round="4")


################ IAS - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==5&site=="IAS")

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

datamatrixtable45 <- dplyr::mutate(datamatrix, site="IAS",round="5")


################ Lord Stirling - Round 1#############################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==1&site=="Lord Stirling")

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

datamatrixtable51 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="1")


################ Lord Stirling - Round 2#############################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==2&site=="Lord Stirling")

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

datamatrixtable52 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="2")


################ Lord Stirling - Round 3#######################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==3&site=="Lord Stirling")

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

datamatrixtable53 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="3")


################ Lord Stirling - Round 4#######################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==4&site=="Lord Stirling")

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

datamatrixtable54 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="4")



################ Lord Stirling - Round 5##########################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==5&site=="Lord Stirling")

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

datamatrixtable55 <- dplyr::mutate(datamatrix, site="Lord Stirling",round="5")


################ URWA - Round 1################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==1&site=="URWA")

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

datamatrixtable61 <- dplyr::mutate(datamatrix, site="URWA",round="1")


################ URWA - Round 2################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==2&site=="URWA")

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

datamatrixtable62 <- dplyr::mutate(datamatrix, site="URWA",round="2")


################ URWA - Round 3################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==3&site=="URWA")

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

datamatrixtable63 <- dplyr::mutate(datamatrix, site="URWA",round="3")


################ URWA - Round 4################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==4&site=="URWA")

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

datamatrixtable64 <- dplyr::mutate(datamatrix, site="URWA",round="4")


################ URWA - Round 5################################################################

# read and manipulate data
alldata <- dplyr::filter(generaldata,  sampling_round==5&site=="URWA")

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

datamatrixtable65 <- dplyr::mutate(datamatrix, site="URWA",round="5")


####################### Join entire dataset##################################

datatotal <- dplyr::bind_rows(datamatrixtable11, datamatrixtable12, datamatrixtable13, datamatrixtable14, datamatrixtable15, datamatrixtable21, datamatrixtable22, datamatrixtable23, datamatrixtable24, datamatrixtable25, datamatrixtable31, datamatrixtable32, datamatrixtable33, datamatrixtable34, datamatrixtable35, datamatrixtable41, datamatrixtable42, datamatrixtable43, datamatrixtable44, datamatrixtable45, datamatrixtable51, datamatrixtable52, datamatrixtable53, datamatrixtable54, datamatrixtable55, datamatrixtable61, datamatrixtable62, datamatrixtable63, datamatrixtable64, datamatrixtable65)


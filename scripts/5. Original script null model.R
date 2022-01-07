# generates null model that each be interacts with each flower spp. with
# probability~number of interactions that flower participated in at that
# site-round

library(furrr)
source("scripts/1. Data cleaning.R")


## set number of iterations
iterations <- 99
boot_prop <- 1
# looks like subset the data, but maybe here to 100 percent?
dat<-sample_n(generaldata
              , floor(boot_prop * nrow(generaldata))) %>% 
  mutate(sr = paste(sampling_round, site))

            
# read and manipulate data
no_cores <- parallel::detectCores() - 1 # if set to 1, just runs sequentially

datatotal<-future_map_dfr(1:length(unique(dat$sr)), function(y){

  sub <- dat %>% filter(sr==unique(dat$sr)[y]) # one site-round per iteration

  ### create objects at the species level, with abundance and trait values 
  databees <- sub %>%
    group_by(bee) %>%
    summarize(tongue=mean(tongue_length.tongue) # mean could also be min or max
              , IT=mean(IT_improved)
              , abundance=n()) 
  
  dataflowers <- sub %>%
    group_by(plant_gs) %>%
    summarize(depth = mean(depth)
              , width = mean(width)
              , abundance = n())
  
  filtered <- dplyr::inner_join(sub,databees, by = "bee") %>% arrange(bee)
  
  ## Create matrix to insert null models. 999 runs of the null model
  # datamatrix <- matrix(ncol = iterations
  #                      , nrow = sum(databees$abundance))
  # datamatrix <- as.data.frame(datamatrix)
  # 
  datamatrix <- map_dfr(1:iterations,function(z){
    species <- map_dfr(1:length(databees$bee),function(x){
      a <- dataflowers[sample(1:length(dataflowers$depth), databees$abundance[x], replace = T, prob = dataflowers$abundance),]
      b <- databees$tongue[x]- a$depth
      c <- (databees$IT[x] - a$width) > 0
      tozero<-b*as.numeric(c)
      tona<-ifelse(b<0, ifelse(c, b, NA), b)
      return(data.frame(raw_t_minus_d=b
                        , remove=tona
                        , zero=tozero
                        , tongue=databees$tongue[x]
                        , bee=databees$bee[x]
                        , iter= z ))
    })
})
  
  # datamatrix$tongue <- (k$tongue)
  # 
  # # Add variables to dataset
  # k <- dplyr::left_join(k,databees,"tongue")
  # datamatrix$bee <- k$bee
  # datamatrix$difference <- filtered$difference
  # datamatrix$newdifference <- filtered$newdifference
  
  return(dplyr::mutate(datamatrix, sr=unique(dat$sr)[y]))
})





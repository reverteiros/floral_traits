# generates null model that each be interacts with each flower spp. with
# probability~number of interactions that flower participated in at that
# site-round

library(furrr)
source("scripts/1. Data cleaning.R")


## set number of iterations
iterations <- 999
boot_prop <- 1
# looks like subset the data, but maybe here to 100 percent?
dat<-sample_n(generaldata
              , floor(boot_prop * nrow(generaldata))) %>% 
  mutate(sr = paste(sampling_round, site))

            
# read and manipulate data
no_cores <- parallel::detectCores() - 1 # if set to 1, just runs sequentially
plan(strategy = "multiprocess", workers = no_cores)

datatotal<-future_map_dfr(1:length(unique(dat$sr)), function(y){
  sub <- dat %>% filter(sr==unique(dat$sr)[y]) # one site-round per iteration

  ### create objects at the species level, with abundance and trait values 
  databees <- sub %>%
    group_by(bee, sr) %>%
    summarize(tongue=mean(tongue_length.tongue) # mean could also be min or max
              , IT=mean(IT_improved)
              , abundance=n()) 
  
  dataflowers <- sub %>%
    group_by(plant_gs, sr) %>%
    summarize(depth = mean(depth)
              , width = mean(width)
              , abundance = n())
  
  filtered <- dplyr::inner_join(sub,databees, by = "bee") %>% arrange(bee)
  
  # in each iteration, resample all flowers from a given site-round for each bee
  # species according to number of bees. 
  datamatrix <- map_dfr(1:iterations,function(z){
   map_dfr(1:length(databees$bee),function(x){
     # resampled table of flowers with their trait vlaues
      a <- dataflowers[sample(1:length(dataflowers$depth)
                              , databees$abundance[x]
                              , replace = T
                              , prob = dataflowers$abundance), ]
      b <- databees$tongue[x]- a$depth
      c <- (databees$IT[x] - a$width) > 0
      # tozero_even_if_long<-b*as.numeric(c) # set the difference to zero when bee can crawl in
      tozero<-b * min(sum(as.numeric(b>0) * as.numeric(c)), 1) # set the difference to zero when bee can crawl in if tongue is short, but not if long
      tona<-ifelse(b<0 # is flower deeper than tongue
                   , ifelse(c # is bee wider than corolla
                            , b # if yes to both, then length difference
                            , NA) # if flower is too deep but bee can crawl in (weird)
                   , b) # just use observed difference when tongue is longer
      return(data.frame(difference = b 
                        , deleted = tona # i.e. remove the interactions for bees that have short tongues but narrow IT
                        , zeroed = tozero # set trait difference to 0 when bee can crawl in (applies even when corolla < tongue?!?!)
                        # , short_zero = tozero_even_if_long # feel like the other way makes more sense (only set to zero for )
                        , tongue = databees$tongue[x]
                        , bee = databees$bee[x]
                        , iter = z
                        , sr = databees$sr[x]
                        ))
      })
  })

  # datamatrix$tongue <- (k$tongue)
  # 
  # # Add variables to dataset
  # k <- dplyr::left_join(k,databees,"tongue")
  # datamatrix$bee <- k$bee
  # datamatrix$difference <- filtered$difference
  # datamatrix$newdifference <- filtered$newdifference
})




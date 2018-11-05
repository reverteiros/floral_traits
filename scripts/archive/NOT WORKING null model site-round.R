
source("scripts/traits.R")

library(ggplot2)
library(purrr)

# read and manipulate data
alldata <- dplyr::filter(generaldata, !is.na(depth)&!is.na(tongue_length.tongue))
alldata$difference <- alldata$depth-alldata$tongue_length.tongue
# 
# summarydata <- alldata %>%
#   group_by(site,sampling_round) %>%
#   summarize(abundance=n()) 

databees<-alldata %>%
  group_by(bee,site,sampling_round) %>%
  summarize(tongue=mean(tongue_length.tongue),abundance=n()) 

dataflowers<-alldata %>%
  group_by(plant_gs,site,sampling_round) %>%
  summarize(depth=mean(depth),abundance=n())

# a <- for(i in 1:iterations){
  

###############################################################
## I stop working on that because the code is not working properly. It is doing what it is supposed to do, the null model, but there is a problem when it creates the list of lists. It does not give properly the length of the number of bee species present at each site and round. I don't know how to fix it
# s<-1
# r<-1
# b<-1

d <- lapply(1:length(levels(alldata$site)),function(s) {
    lapply(1:length(unique(alldata$sampling_round)),function(r) {
      # alldatasite <- alldata%>%filter(site==levels(alldata$site)[s])
      # flowerssite <- dataflowers%>%filter(site==levels(dataflowers$site)[s])
      # beesite <- databees%>%filter(site==levels(databees$site)[s])
      alldataround <- alldata%>%filter(sampling_round==unique(alldata$sampling_round)[r]&site==levels(alldata$site)[s] )
      flowersround <- dataflowers%>%filter(sampling_round==unique(dataflowers$sampling_round)[r]&site==levels(dataflowers$site)[s])
      beesround <- databees%>%filter(sampling_round==unique(databees$sampling_round)[r]&site==levels(databees$site)[s])
      lapply(1:length(unique(beesround$bee)),function(b){
        alldatabee <- alldataround%>%filter(bee==unique(alldataround$bee)[b]) %>% droplevels()
        beesbee <- beesround%>%filter(bee==unique(beesround$bee)[b])
        sampledflowers <- sample(flowersround$depth, length(alldatabee$bee), replace = T, prob = flowersround$abundance)
        sampleddifferences <- sampledflowers - beesbee$tongue
        # print(sampleddifferences)
        # print(rep(beesbee$tongue,length(sampleddifferences)))
        return(
          data.frame(sampleddifferences,
                     tongue=rep(beesbee$tongue,length(sampleddifferences)),
                     bee=rep(beesbee$bee, length(sampleddifferences)),
                     site=rep(unique(alldata$site)[s], length(sampleddifferences)),
                     sampleround=rep(unique(alldata$sampling_round)[r], length(sampleddifferences)))
           )
        # length(alldatabee$bee)
        # length(sampledflowers)
      })
      # length(unique(alldataround$bee))
    })
  })


library(plyr)

bigout<-ldply(lapply(d, function(x){
  ldply(lapply(x, function(y){
    ldply(y)}))}))

write.csv(bigout,"data/site_round_threshold.csv", row.names=F)
# 
# alldatasite <- alldata%>%filter(site=="Cold Soil" & sampling_round=="1")
# flowerssite <- dataflowers%>%filter(site=="Cold Soil"& sampling_round=="1")
# beessite <- databees%>%filter(site=="Cold Soil"& sampling_round=="1")
# # lapply(1:length(unique(alldata$sampling_round)),function(x) {
# # lapply(1:length(unique(databees$bee)),function(x){
# sampledflowers <- sample(flowerssite$depth, nrow(alldatasite), replace = T, prob = flowerssite$abundance)
# sampleddifferences <- sampledflowers - alldatasite$tongue_length.tongue
# 
# hist(sampleddifferences)
# hist(c[[2]][[1]][[1]][[1]])
# length(sampleddifferences)
# length(c[[2]][[1]][[1]])

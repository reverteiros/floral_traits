
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

d <- lapply(1:length(levels(alldata$site)),function(x) {
  alldatasite <- alldata%>%filter(site==levels(alldata$site)[x])
  flowerssite <- dataflowers%>%filter(site==levels(dataflowers$site)[x])
  beesite <- databees%>%filter(site==levels(databees$site)[x])
    lapply(1:length(unique(alldata$sampling_round)),function(x) {
      alldataround <- alldatasite%>%filter(sampling_round==unique(alldatasite$sampling_round)[x])
      flowersround <- flowerssite%>%filter(sampling_round==unique(flowerssite$sampling_round)[x])
      beesround <- beesite%>%filter(sampling_round==unique(beesite$sampling_round)[x])
      lapply(1:length(unique(beesround$bee)),function(x){
        alldatabee <- alldataround%>%filter(bee==unique(alldataround$bee)[x])
        beesbee <- beesround%>%filter(bee==unique(beesround$bee)[x])
        sampledflowers <- sample(flowersround$depth, length(alldatabee$bee), replace = T, prob = flowersround$abundance)
        sampleddifferences <- sampledflowers - beesbee$tongue
        return(data.frame(sampleddifferences,tongue=rep(beesbee$tongue,length(sampleddifferences))))
        # length(alldatabee$bee)
        # length(sampledflowers)
      })
      # length(unique(alldataround$bee))
    })
  })


x <- c(32,35,28,25,36)

nrow(beefiltered)
beefiltered <- databees%>%filter(site=="Baldpate" & sampling_round=="2")%>%
  group_by(bee)%>%
  summarize(tongue=mean(tongue),abund=n())







c[[1]][[1]][[1]]
c[[1]][[1]][[2]]
c[[1]][[1]][[3]]
c[[1]][[1]][[4]]


sort(unique(alldata$bee))

 rbind.fill(lapply(c, function(x)as.data.frame(t(x))))

k <- map_dfr(c,rbind)

z <- as.data.frame(sapply(c, unlist))
names(z) <- unique(alldata$site)


alldatasite <- alldata%>%filter(site=="Cold Soil" & sampling_round=="1")
flowerssite <- dataflowers%>%filter(site=="Cold Soil"& sampling_round=="1")
beessite <- databees%>%filter(site=="Cold Soil"& sampling_round=="1")
# lapply(1:length(unique(alldata$sampling_round)),function(x) {
# lapply(1:length(unique(databees$bee)),function(x){
sampledflowers <- sample(flowerssite$depth, nrow(alldatasite), replace = T, prob = flowerssite$abundance)
sampleddifferences <- sampledflowers - alldatasite$tongue_length.tongue

hist(sampleddifferences)
hist(c[[2]][[1]][[1]][[1]])
length(sampleddifferences)
length(c[[2]][[1]][[1]])

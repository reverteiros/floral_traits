
library(bipartite)

dat<-subsetgeneraldata %>% mutate(sr=paste(site, sampling_round))

dat <- dat%>% mutate(webID=sr)

group <- group_by(dat, bee, plant_gs,webID)
datareal <- summarize(group, abundance=n())


testdata <- data.frame(higher = datareal$bee,
                       lower = datareal$plant_gs, 
                       webID = datareal$webID,
                       freq=datareal$abundance)

unique <- frame2webs(testdata, type.out =
             "list", emptylist = TRUE)


names(unique)

b1<-specieslevel(unique[[1]], index=c("degree","d"), level="both")
lowb1 <- b1$`lower level`
lowb1$bee <- rownames(lowb1)
lowb1$sr <- "Baldpate 1"
colnames(lowb1) <- c("bee_degree","bee_d","bee", "sr")
highb1 <- b1$`higher level`
highb1$plant_gs <- rownames(highb1)
highb1$sr <- "Baldpate 1"
colnames(highb1) <- c("plant_degree","plant_d","plant_gs", "sr")

b2<-specieslevel(unique[[2]], index=c("degree","d"), level="both")
lowb2 <- b2$`lower level`
lowb2$bee <- rownames(lowb2)
lowb2$sr <- "Baldpate 2"
colnames(lowb2) <- c("bee_degree","bee_d","bee", "sr")
highb2 <- b2$`higher level`
highb2$plant_gs <- rownames(highb2)
highb2$sr <- "Baldpate 2"
colnames(highb2) <- c("plant_degree","plant_d","plant_gs", "sr")

b3<-specieslevel(unique[[3]], index=c("degree","d"), level="both")
lowb3 <- b3$`lower level`
lowb3$bee <- rownames(lowb3)
lowb3$sr <- "Baldpate 3"
colnames(lowb3) <- c("bee_degree","bee_d","bee", "sr")
highb3 <- b3$`higher level`
highb3$plant_gs <- rownames(highb3)
highb3$sr <- "Baldpate 3"
colnames(highb3) <- c("plant_degree","plant_d","plant_gs", "sr")

b4<-specieslevel(unique[[4]], index=c("degree","d"), level="both")
lowb4 <- b4$`lower level`
lowb4$bee <- rownames(lowb4)
lowb4$sr <- "Baldpate 4"
colnames(lowb4) <- c("bee_degree","bee_d","bee", "sr")
highb4 <- b4$`higher level`
highb4$plant_gs <- rownames(highb4)
highb4$sr <- "Baldpate 4"
colnames(highb4) <- c("plant_degree","plant_d","plant_gs", "sr")

b5<-specieslevel(unique[[5]], index=c("degree","d"), level="both")
lowb5 <- b5$`lower level`
lowb5$bee <- rownames(lowb5)
lowb5$sr <- "Baldpate 5"
colnames(lowb5) <- c("bee_degree","bee_d","bee", "sr")
highb5 <- b5$`higher level`
highb5$plant_gs <- rownames(highb5)
highb5$sr <- "Baldpate 5"
colnames(highb5) <- c("plant_degree","plant_d","plant_gs", "sr")

c1<-specieslevel(unique[[6]], index=c("degree","d"), level="both")
lowc1 <- c1$`lower level`
lowc1$bee <- rownames(lowc1)
lowc1$sr <- "Cold Soil 1"
colnames(lowc1) <- c("bee_degree","bee_d","bee", "sr")
highc1 <- c1$`higher level`
highc1$plant_gs <- rownames(highc1)
highc1$sr <- "Cold Soil 1"
colnames(highc1) <- c("plant_degree","plant_d","plant_gs", "sr")

c2<-specieslevel(unique[[7]], index=c("degree","d"), level="both")
lowc2 <- c2$`lower level`
lowc2$bee <- rownames(lowc2)
lowc2$sr <- "Cold Soil 2"
colnames(lowc2) <- c("bee_degree","bee_d","bee", "sr")
highc2 <- c2$`higher level`
highc2$plant_gs <- rownames(highc2)
highc2$sr <- "Cold Soil 2"
colnames(highc2) <- c("plant_degree","plant_d","plant_gs", "sr")

c3<-specieslevel(unique[[8]], index=c("degree","d"), level="both")
lowc3 <- c3$`lower level`
lowc3$bee <- rownames(lowc3)
lowc3$sr <- "Cold Soil 3"
colnames(lowc3) <- c("bee_degree","bee_d","bee", "sr")
highc3 <- c3$`higher level`
highc3$plant_gs <- rownames(highc3)
highc3$sr <- "Cold Soil 3"
colnames(highc3) <- c("plant_degree","plant_d","plant_gs", "sr")

c4<-specieslevel(unique[[9]], index=c("degree","d"), level="both")
lowc4 <- c4$`lower level`
lowc4$bee <- rownames(lowc4)
lowc4$sr <- "Cold Soil 4"
colnames(lowc4) <- c("bee_degree","bee_d","bee", "sr")
highc4 <- c4$`higher level`
highc4$plant_gs <- rownames(highc4)
highc4$sr <- "Cold Soil 4"
colnames(highc4) <- c("plant_degree","plant_d","plant_gs", "sr")

c5<-specieslevel(unique[[10]], index=c("degree","d"), level="both")
lowc5 <- c5$`lower level`
lowc5$bee <- rownames(lowc5)
lowc5$sr <- "Cold Soil 5"
colnames(lowc5) <- c("bee_degree","bee_d","bee", "sr")
highc5 <- c5$`higher level`
highc5$plant_gs <- rownames(highc5)
highc5$sr <- "Cold Soil 5"
colnames(highc5) <- c("plant_degree","plant_d","plant_gs", "sr")

f1<-specieslevel(unique[[11]], index=c("degree","d"), level="both")
lowf1 <- f1$`lower level`
lowf1$bee <- rownames(lowf1)
lowf1$sr <- "Fox Hill 1"
colnames(lowf1) <- c("bee_degree","bee_d","bee", "sr")
highf1 <- f1$`higher level`
highf1$plant_gs <- rownames(highf1)
highf1$sr <- "Fox Hill 1"
colnames(highf1) <- c("plant_degree","plant_d","plant_gs", "sr")

f2<-specieslevel(unique[[12]], index=c("degree","d"), level="both")
lowf2 <- f2$`lower level`
lowf2$bee <- rownames(lowf2)
lowf2$sr <- "Fox Hill 2"
colnames(lowf2) <- c("bee_degree","bee_d","bee", "sr")
highf2 <- f2$`higher level`
highf2$plant_gs <- rownames(highf2)
highf2$sr <- "Fox Hill 2"
colnames(highf2) <- c("plant_degree","plant_d","plant_gs", "sr")

f3<-specieslevel(unique[[13]], index=c("degree","d"), level="both")
lowf3 <- f3$`lower level`
lowf3$bee <- rownames(lowf3)
lowf3$sr <- "Fox Hill 3"
colnames(lowf3) <- c("bee_degree","bee_d","bee", "sr")
highf3 <- f3$`higher level`
highf3$plant_gs <- rownames(highf3)
highf3$sr <- "Fox Hill 3"
colnames(highf3) <- c("plant_degree","plant_d","plant_gs", "sr")

f4<-specieslevel(unique[[14]], index=c("degree","d"), level="both")
lowf4 <- f4$`lower level`
lowf4$bee <- rownames(lowf4)
lowf4$sr <- "Fox Hill 4"
colnames(lowf4) <- c("bee_degree","bee_d","bee", "sr")
highf4 <- f4$`higher level`
highf4$plant_gs <- rownames(highf4)
highf4$sr <- "Fox Hill 4"
colnames(highf4) <- c("plant_degree","plant_d","plant_gs", "sr")

f5<-specieslevel(unique[[15]], index=c("degree","d"), level="both")
lowf5 <- f5$`lower level`
lowf5$bee <- rownames(lowf5)
lowf5$sr <- "Fox Hill 5"
colnames(lowf5) <- c("bee_degree","bee_d","bee", "sr")
highf5 <- f5$`higher level`
highf5$plant_gs <- rownames(highf5)
highf5$sr <- "Fox Hill 5"
colnames(highf5) <- c("plant_degree","plant_d","plant_gs", "sr")

i1<-specieslevel(unique[[16]], index=c("degree","d"), level="both")
lowi1 <- i1$`lower level`
lowi1$bee <- rownames(lowi1)
lowi1$sr <- "IAS 1"
colnames(lowi1) <- c("bee_degree","bee_d","bee", "sr")
highi1 <- i1$`higher level`
highi1$plant_gs <- rownames(highi1)
highi1$sr <- "IAS 1"
colnames(highi1) <- c("plant_degree","plant_d","plant_gs", "sr")

i2<-specieslevel(unique[[17]], index=c("degree","d"), level="both")
lowi2 <- i2$`lower level`
lowi2$bee <- rownames(lowi2)
lowi2$sr <- "IAS 2"
colnames(lowi2) <- c("bee_degree","bee_d","bee", "sr")
highi2 <- i2$`higher level`
highi2$plant_gs <- rownames(highi2)
highi2$sr <- "IAS 2"
colnames(highi2) <- c("plant_degree","plant_d","plant_gs", "sr")

i3<-specieslevel(unique[[18]], index=c("degree","d"), level="both")
lowi3 <- i3$`lower level`
lowi3$bee <- rownames(lowi3)
lowi3$sr <- "IAS 3"
colnames(lowi3) <- c("bee_degree","bee_d","bee", "sr")
highi3 <- i3$`higher level`
highi3$plant_gs <- rownames(highi3)
highi3$sr <- "IAS 3"
colnames(highi3) <- c("plant_degree","plant_d","plant_gs", "sr")

i4<-specieslevel(unique[[19]], index=c("degree","d"), level="both")
lowi4 <- i4$`lower level`
lowi4$bee <- rownames(lowi4)
lowi4$sr <- "IAS 4"
colnames(lowi4) <- c("bee_degree","bee_d","bee", "sr")
highi4 <- i4$`higher level`
highi4$plant_gs <- rownames(highi4)
highi4$sr <- "IAS 4"
colnames(highi4) <- c("plant_degree","plant_d","plant_gs", "sr")

i5<-specieslevel(unique[[20]], index=c("degree","d"), level="both")
lowi5 <- i5$`lower level`
lowi5$bee <- rownames(lowi5)
lowi5$sr <- "IAS 5"
colnames(lowi5) <- c("bee_degree","bee_d","bee", "sr")
highi5 <- i5$`higher level`
highi5$plant_gs <- rownames(highi5)
highi5$sr <- "IAS 5"
colnames(highi5) <- c("plant_degree","plant_d","plant_gs", "sr")

l1<-specieslevel(unique[[21]], index=c("degree","d"), level="both")
lowl1 <- l1$`lower level`
lowl1$bee <- rownames(lowl1)
lowl1$sr <- "Lord Stirling 1"
colnames(lowl1) <- c("bee_degree","bee_d","bee", "sr")
highl1 <- l1$`higher level`
highl1$plant_gs <- rownames(highl1)
highl1$sr <- "Lord Stirling 1"
colnames(highl1) <- c("plant_degree","plant_d","plant_gs", "sr")

l2<-specieslevel(unique[[22]], index=c("degree","d"), level="both")
lowl2 <- l2$`lower level`
lowl2$bee <- rownames(lowl2)
lowl2$sr <- "Lord Stirling 2"
colnames(lowl2) <- c("bee_degree","bee_d","bee", "sr")
highl2 <- l2$`higher level`
highl2$plant_gs <- rownames(highl2)
highl2$sr <- "Lord Stirling 2"
colnames(highl2) <- c("plant_degree","plant_d","plant_gs", "sr")

l3<-specieslevel(unique[[23]], index=c("degree","d"), level="both")
lowl3 <- l3$`lower level`
lowl3$bee <- rownames(lowl3)
lowl3$sr <- "Lord Stirling 3"
colnames(lowl3) <- c("bee_degree","bee_d","bee", "sr")
highl3 <- l3$`higher level`
highl3$plant_gs <- rownames(highl3)
highl3$sr <- "Lord Stirling 3"
colnames(highl3) <- c("plant_degree","plant_d","plant_gs", "sr")

l4<-specieslevel(unique[[24]], index=c("degree","d"), level="both")
lowl4 <- l4$`lower level`
lowl4$bee <- rownames(lowl4)
lowl4$sr <- "Lord Stirling 4"
colnames(lowl4) <- c("bee_degree","bee_d","bee", "sr")
highl4<- l4$`higher level`
highl4$plant_gs <- rownames(highl4)
highl4$sr <- "Lord Stirling 4"
colnames(highl4) <- c("plant_degree","plant_d","plant_gs", "sr")

l5<-specieslevel(unique[[25]], index=c("degree","d"), level="both")
lowl5 <- l5$`lower level`
lowl5$bee <- rownames(lowl5)
lowl5$sr <- "Lord Stirling 5"
colnames(lowl5) <- c("bee_degree","bee_d","bee", "sr")
highl5 <- l5$`higher level`
highl5$plant_gs <- rownames(highl5)
highl5$sr <- "Lord Stirling 5"
colnames(highl5) <- c("plant_degree","plant_d","plant_gs", "sr")


u1<-specieslevel(unique[[26]], index=c("degree","d"), level="both")
lowu1 <- u1$`lower level`
lowu1$bee <- rownames(lowu1)
lowu1$sr <- "URWA 1"
colnames(lowu1) <- c("bee_degree","bee_d","bee", "sr")
highu1 <- u1$`higher level`
highu1$plant_gs <- rownames(highu1)
highu1$sr <- "URWA 1"
colnames(highu1) <- c("plant_degree","plant_d","plant_gs", "sr")

u2<-specieslevel(unique[[27]], index=c("degree","d"), level="both")
lowu2 <- u2$`lower level`
lowu2$bee <- rownames(lowu2)
lowu2$sr <- "URWA 2"
colnames(lowu2) <- c("bee_degree","bee_d","bee", "sr")
highu2 <- u2$`higher level`
highu2$plant_gs <- rownames(highu2)
highu2$sr <- "URWA 2"
colnames(highu2) <- c("plant_degree","plant_d","plant_gs", "sr")

u3<-specieslevel(unique[[28]], index=c("degree","d"), level="both")
lowu3 <- u3$`lower level`
lowu3$bee <- rownames(lowu3)
lowu3$sr <- "URWA 3"
colnames(lowu3) <- c("bee_degree","bee_d","bee", "sr")
highu3 <- u3$`higher level`
highu3$plant_gs <- rownames(highu3)
highu3$sr <- "URWA 3"
colnames(highu3) <- c("plant_degree","plant_d","plant_gs", "sr")

u4<-specieslevel(unique[[29]], index=c("degree","d"), level="both")
lowu4 <- u4$`lower level`
lowu4$bee <- rownames(lowu4)
lowu4$sr <- "URWA 4"
colnames(lowu4) <- c("bee_degree","bee_d","bee", "sr")
highu4 <- u4$`higher level`
highu4$plant_gs <- rownames(highu4)
highu4$sr <- "URWA 4"
colnames(highu4) <- c("plant_degree","plant_d","plant_gs", "sr")

u5<-specieslevel(unique[[30]], index=c("degree","d"), level="both")
lowu5 <- u5$`lower level`
lowu5$bee <- rownames(lowu5)
lowu5$sr <- "URWA 5"
colnames(lowu5) <- c("bee_degree","bee_d","bee", "sr")
highu5 <- u5$`higher level`
highu5$plant_gs <- rownames(highu5)
highu5$sr <- "URWA 5"
colnames(highu5) <- c("plant_degree","plant_d","plant_gs", "sr")

lowlevels <- dplyr::bind_rows(lowb1,lowb2,lowb3,lowb4,lowb5,lowc1,lowc2,lowc3,lowc4,lowc5,lowf1,lowf2,lowf3,lowf4,lowf5,lowi1,lowi2,lowi3,lowi4,lowi5,lowl1,lowl2,lowl3,lowl4,lowl5,lowu1,lowu2,lowu3,lowu4,lowu5)

highlevels <- dplyr::bind_rows(highb1,highb2,highb3,highb4,highb5,highc1,highc2,highc3,highc4,highc5,highf1,highf2,highf3,highf4,highf5,highi1,highi2,highi3,highi4,highi5,highl1,highl2,highl3,highl4,highl5,highu1,highu2,highu3,highu4,highu5)

lowlevels

dat <- dat %>% left_join(highlevels, by=(c("bee","sr")))
dat <- dat %>% left_join(lowlevels, by=(c("plant_gs","sr")))

dat %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(degree=mean(bee_degree), ABUND=n(),vari=sd(degree)) %>% ggplot(aes(degree,ABUND))+
  geom_point(aes(alpha=0.3))


## bee degree
dat %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(degree=mean(bee_degree), ABUND=n(),vari=sd(bee_degree)) %>% ggplot(aes(tongue_length.tongue,degree))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  # geom_errorbar(aes(ymin=degree-1.96*vari, ymax=degree+1.96*vari))+
  # xlim(c(0,14))+
  # ylim(c(0,50))+
  geom_smooth()+
  labs(x="tongue length for bee species", y="bee degree")

## bee d'
dat %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(d=mean(bee_d), ABUND=n(),vari=sd(d)) %>% ggplot(aes(tongue_length.tongue,d))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  # geom_errorbar(aes(ymin=d-1.96*vari, ymax=degree+1.96*vari))+
  # xlim(c(0,14))+
  # ylim(c(0,1))+
  geom_smooth()+
  labs(x="tongue length for bee species", y="bee d'")


dat %>% group_by(plant_gs, sr, depth) %>% summarize(degree=mean(plant_degree), ABUND=n(),vari=sd(plant_degree))  %>% ggplot(aes(degree,ABUND))+
  geom_point(aes(alpha=0.3))

## plant degree
dat %>% group_by(plant_gs, sr, depth) %>% summarize(degree=mean(plant_degree), ABUND=n(),vari=sd(plant_degree)) %>% ggplot(aes(depth,degree))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  # geom_errorbar(aes(ymin=degree-1.96*vari, ymax=degree+1.96*vari))+
  # xlim(c(0,14))+
  # ylim(c(0,1))+
  geom_smooth()+
  labs(x="corolla depth", y="flower degree")

## plant d'
dat %>% group_by(plant_gs, sr, depth) %>% summarize(d=mean(plant_d), ABUND=n(),vari=sd(plant_d)) %>% ggplot(aes(depth,d))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  # geom_errorbar(aes(ymin=degree-1.96*vari, ymax=degree+1.96*vari))+
  # xlim(c(0,14))+
  # ylim(c(0,20))+
  geom_smooth()+
  labs(x="corolla depth", y="flower d'")


dat$difference <- dat$tongue_length.tongue-dat$depth

dat %>% group_by(plant_gs, sr) %>% summarize(diff=mean(difference), ABUND=n(),vari=sd(difference),d=mean(plant_d),meanplant=mean(depth),meantongue=mean(tongue_length.tongue)) %>% ggplot(aes(d,diff))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  # geom_errorbar(aes(ymin=difference-1.96*vari, ymax=difference+1.96*vari))+
  # xlim(c(0,14))+
  # ylim(c(0,20))+
  geom_smooth()+
  labs(x="flower d'", y="Difference (insect - flower)")

dat %>% group_by(bee, sr) %>% summarize(diff=mean(difference), ABUND=n(),vari=sd(difference),d=mean(bee_d)) %>% ggplot(aes(d,diff))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  # geom_errorbar(aes(ymin=difference-1.96*vari, ymax=difference+1.96*vari))+
  # xlim(c(0,14))+
  # ylim(c(0,20))+
  geom_smooth()+
  labs(x="bee d'", y="Difference (insect - flower)")




dat %>% group_by(bee,sr) %>% summarize(diff=mean(difference), srmean=mean(srmean),ABUND=n()) %>% ggplot(aes(srmean,diff))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  geom_smooth()



dat2 <- dat %>% group_by(sr) %>% summarize(mt=mean(tongue_length.tongue),md=mean(depth),srmean=(mt-md))
dat <- dat %>% left_join(dat2, by=(c("sr")))
  
### plot means and sd for bee species
dat %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(mfd=mean(depth), ABUND=n(), vari=sd(depth)) %>% ggplot(aes(tongue_length.tongue,mfd))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  geom_errorbar(aes(ymin=mfd-1.96*vari, ymax=mfd+1.96*vari))+
  xlim(c(0,14))+
  ylim(c(0,20))+
  geom_smooth()+
  labs(x="tongue length for bee species", y="average floral depth for flowers visited")

dat %>% group_by(plant_gs, sr, depth) %>% summarize(mtl=mean(tongue_length.tongue), ABUND=n(), vari=sd(tongue_length.tongue)) %>% ggplot(aes(depth,mtl))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  geom_errorbar(aes(ymin=mtl-1.96*vari, ymax=mtl+1.96*vari))+
  xlim(c(0,33))+
  ylim(c(0,16))+
  geom_smooth()+
  labs(x="flower depth", y="average tongue length of bees visiting")




#viusalize SR of flower visitors
#make binomial data
# library(parallel)
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores)
# clusterExport(cl=cl, varlist=ls())
# 
# pontlist<-parLapply(cl=cl,floor(c(seq(1,10,1) %o% 1.1^(1:90))),  fun=function(x){
#     females=rbinom(5000,x, 15373/(15373+3340))
#     males=rep(x,5000)-females
#     return(data.frame(females,males))})
# 
# stopCluster(cl)

library(plyr)
# gazillion<-ldply(pontlist)
# write.csv(gazillion, "data/gazillion.csv")
# gazillion<-read.csv("data/gazillion.csv")

#gazillion %>% ggplot(aes(females,males))+geom_point()+scale_x_log10()+scale_y_log10()


#load data using script
source("scripts/2016_data_readin.R")
#drop bad data of various sorts: things missing sex determination, sampling events that were interrupted by storms, site dropped after first round
XerDate<-droplevels(XerDate[XerDate$bee_sex%in%c("M", "F")&XerDate$keep=="Y"&XerDate$site!="Featherbed",])
# waspcheck<-mapply(XerDate, FUN=function(x){match(x,"wasp")})
# which(!is.na(waspcheck))

#rename columns for readability
# XerDate<-XerDate%>% mutate(bee=paste(XerDate$genus, XerDate$species, sep="_")) %>% 
#     dplyr::select(uniqueID,bee_sex=M.F, bee_genus=genus, bee_species=species, bee=bee, plant_code=GEN.SPE, plant_species, plant_genus, plant_family,sociality=Sociality, site=site_full, sampling_round=round)
# XerDate<-XerDate %>% filter(!is.na(uniqueID)) 
# XerDate<-droplevels(XerDate)

XerDate$sampling_round<-as.factor(XerDate$sampling_round)
plist<-data.frame(droplevels((XerDate %>% dplyr::group_by(plant_code) %>% dplyr::summarize(abund=n()) %>% arrange(abund) %>% filter(abund>19))))[,"plant_code"]
XD1<-XerDate %>% filter(plant_code %in% plist)

planteffs<-XD1 %>% group_by(plant_genus, plant_species, plant_code, bee_sex) %>% 
    dplyr::summarize(visits=n()) %>% 
    # left_join(plant_preds) %>% 
    spread(bee_sex, value=visits, fill=0) #%>% arrange( desc(preds))
#make plot
#gazillion<-rbind(gazillion+seq(0,.99,0.02))
planteffs<-planteffs %>% mutate(sr=M/F, tot=M+F)%>% mutate(quan=qbinom(M/tot, size=tot, prob= 3370/(3370+15497)))
    
    #this is not used now but was an idea to replace y with z-score
#mutate(z=((M-3340/(15373+3340)*tot)/(tot*3340/(15373+3340)*(1-(3340/(15373+3340))))^2)

# planteffs %>% ggplot(aes(tot, quan))+geom_point()+theme_classic()
# qbinom(.3,5,.5)
binomsimpleCap <- function(x) {
    if(x!="NECTARY"){
    paste(substring(x,1,1),tolower(substring(x, 2)), sep="") 
    }
    else x
    
}

planteffs$plant_code<-as.character(rbind(lapply(planteffs$plant_code, binomsimpleCap)))


nm<-ldply(lapply(1:2800, FUN=function(x){mr=rbinom(n=max(round(90000-10*x^1.1),5000), size=x, prob=3370/(15497+3370))
return(data.frame(tot=rep(x,max(round(90000-10*x^1.1),5000)),sr=mr/(x-mr)))}))
###########################################
##########################################
#### MS Fig 2 ############################
##########################################

png(file="figures/odds_deviant_flowers.png", width=2200, height=1433)
planteffs %>% ggplot(aes(tot, sr+0.001))+
    theme_classic()+
    geom_bin2d(data=nm, aes(tot, sr+0.001), bins=200, inherit.aes = F) +
    geom_hline(yintercept=3370/15497)+
    scale_fill_gradient(low="lightblue", high="black")+
    geom_point(color="darkred", size=8)+
    geom_text(label=paste(planteffs$plant_code," "), angle=20, vjust=0.5,hjust=1,size=10, fontface="italic")+
   
    theme(legend.position = "none",axis.text = element_text(size=40,),text= element_text(size=40))+
    scale_x_log10(limits=c(15,2500), breaks=c(20,50,100,200,500,1000,2000))+
    scale_y_log10(breaks=c(0.01,.05, 0.1, 0.2, 0.5, 1, 2,4))+
 
    labs(x="# bees recorded", y="#males/#females")
   
dev.off()

# png(file="figures/SR_deviant_flowers.png", width=2200, height=1433)
# #pdf(file="figures/SR_deviant_flowers.pdf")
# planteffs %>% ggplot(aes(F+1, M+1))+theme_classic()+
#     geom_bin2d(data=gazillion, aes(females+1, males+1), bins=100, inherit.aes = F)+scale_fill_gradient(low="lightblue",high="black")+ #low="ivory", high="purple4"
#    geom_smooth(data=gazillion, aes(females, males), method="glm", color="purple", alpha=0.5, se=F, fullrange=F, inherit.aes = F)+
#     geom_point(alpha=0.3, color="firebrick2", size=10) +
#     geom_text(aes(F+1, M+1, label=paste(" ",plant_code)), angle=33, vjust=0.5, hjust=0, size=10, fontface="bold")+scale_x_log10(limits=c(10,3000), breaks=c(1,2,5,10,20,50,100,200,500,1000,2000))+scale_y_log10(limits=c(-1,500),breaks=c(1,2,5,10,20,50,100,200,500))+
#     labs(x="# female visitors to flower+1 (log scale)", y="male visitors to flower+1 (log scale)")+
#     theme(legend.position = "none", axis.text = element_text(size=40),text= element_text(size=40))
# dev.off()
# 

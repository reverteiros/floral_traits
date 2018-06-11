##### Start playing with SRS-MER collaboration
### when do we see bees?

source("scripts/2016_data_readin.R")
#make a middle of the sampling bout time vector
#try using mean
XerDate<-XerDate %>% group_by_all() %>% summarize(midbout=mean(c(boutstart, boutend), na.rm=T))

#plot activity by time of day, separate plot for each round and sex
XerDate %>%filter(bee_sex%in% c("M","F")&sampling_round %in% c(1:5)) %>% droplevels() %>%  
    ggplot(aes(x=midbout, fill=bee_sex))+
    facet_wrap(~sampling_round+bee_sex)+
    geom_histogram(bins=15)+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))


##### Bee size (IT distance) by time of day and sex

source("scripts/sara_data_cleaning.R")

#make a middle of the sampling bout time vector
#try using mean
maleIT<-maleIT %>% group_by_all() %>% summarize(midbout=mean(c(boutstart, boutend), na.rm=T))

levels(maleIT$bee_genus)
#plot IT distance by time of day, separate for sex. Points plot
maleIT %>%filter(sex%in% c("male","female")) %>% droplevels() %>%
    ggplot(aes(x=midbout))+
    facet_wrap(~sex)+
    geom_point(aes(y=ITlength_mm))+
    theme_classic()

#plot IT distance by time of day, separate for sex. Smooth plot
#added jitter to see overlapping points (in y direction) and colored points by genus a bit
maleIT %>%filter(sex%in% c("male","female")) %>% droplevels() %>%
    ggplot(aes(x=midbout, ITlength_mm))+
    facet_wrap(~sex, ncol=1)+
    geom_smooth()+
    geom_jitter(aes(color=bee_genus),alpha=0.1, height=0.1)+
    theme_classic()

#look by sampling round, and not sex
maleIT %>%filter(sex%in% c("male","female")&sampling_round<6) %>% droplevels() %>%
    ggplot(aes(x=midbout, ITlength_mm))+
    facet_wrap(~sampling_round)+
    geom_smooth()+
    geom_jitter(aes(color=bee_genus),alpha=0.1, height=0.1)+
    theme_classic()

########################### ######
### log tranform body size, look without bombus too
#########
###################################
maleIT %>%filter(sex%in% c("male","female")&sampling_round<6&bee_genus!="Bombus"&bee_genus!="Xylocopa") %>% droplevels() %>%
    ggplot(aes(x=midbout, log(ITlength_mm)))+
    facet_wrap(~sex, ncol=1)+
    geom_smooth()+
    geom_jitter(aes(color=bee_genus),alpha=0.1, height=0.1)+
    theme_classic()

#look by sampling round, and not sex
maleIT %>%
    filter(sex%in% c("male","female") & sampling_round<6&bee_genus!="Bombus"&bee_genus!="Xylocopa") %>% droplevels() %>%
    ggplot(aes(x=midbout, log(ITlength_mm)))+
    facet_wrap(~sampling_round)+
    geom_smooth()+
    geom_jitter(aes(color=bee_genus),alpha=0.1, height=0.1)+
    theme_classic()



##### Plant as unit, plant-centered plots


flowers<-maleIT %>% 
    dplyr::group_by(plant_code) %>% 
    dplyr::summarize(visits=n()) %>%
    dplyr::filter(visits > 400) 

mostvisitedplants<-maleIT %>% inner_join(flowers,by = "plant_code")

#added jitter and alpha to see density more clearly,
#looked at overall in addition to by sex
#log transformed
#included more plant taxa (floor of 400 visits)
mostvisitedplants %>%filter(sex%in% c("male","female")) %>%  droplevels() %>%  
    ggplot(aes(x=midbout))+
    facet_wrap(~plant_code)+
    #facet_wrap(~plant_code+sex)+
    geom_jitter(aes(y=log(ITlength_mm)), height=0.1, alpha=0.1)+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))


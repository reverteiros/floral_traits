
library(tidyverse)
library(lqmm)
library(aqmm)
# library(nlqmm)
library(parallel)

source("scripts/script_traits.R")

subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))
subsetgeneraldata$sampling_round <- as.character(subsetgeneraldata$sampling_round)
subsetgeneraldata <- as.data.frame(subsetgeneraldata)
subsetgeneraldata<-subsetgeneraldata[complete.cases(subsetgeneraldata),]
subsetgeneraldata$sampling_round <- as.character(subsetgeneraldata$sampling_round)
subsetgeneraldata$randomfactor <- paste(subsetgeneraldata$site, subsetgeneraldata$sampling_round, sep = "_")

####
#look at distribution of tongues and flowers across site-round

subsetgeneraldata %>%mutate(sr=paste(site, sampling_round)) %>% 
  ggplot(aes(sampling_round, tongue_length.tongue))+
  geom_boxplot()+geom_jitter(size=0.05)+
  theme_classic()+facet_wrap(~site)

subsetgeneraldata %>%mutate(sr=paste(site, sampling_round)) %>% 
  ggplot(aes(sampling_round, depth))+
  geom_boxplot()+
  geom_jitter(size=0.05)+
  theme_classic()+facet_wrap(~site)


subsetgeneraldata<-subsetgeneraldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72))
subsetgeneraldata<-subsetgeneraldata %>% mutate(beewider=if_else(IT_improved>width, "true", "false"))


dat<-subsetgeneraldata %>% mutate(sr=paste(site, sampling_round))
# two<-aqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=0.2, data=subsetgeneraldata)
start_time<-Sys.time()
no_cores<-detectCores()-1
cl<-makeCluster(no_cores)
clusterExport(cl=cl, varlist=ls())
clusterEvalQ(cl, library(lqmm))
QR1<-parLapply(cl, c(1:9), fun=function(x){
  lqmm(fixed = tongue_length.tongue~depth+depth^2+depth^3, random = ~1, group = sr, tau=c(x/10), data=dat,  control= list(method="df", LP_max_iter=100000))
})
stopCluster(cl)
endtime<-Sys.time()-start_time
endtime  

plot(jitter(dat$depth, amount=0.35), jitter(dat$tongue_length.tongue, amount=0.1), pch=".", xlab="corolla depth", ylab="tongue length", xlim=c(0,30))

start_time<-Sys.time()
# no_cores<-detectCores()-1
# cl<-makeCluster(no_cores)
# clusterExport(cl=cl, varlist=ls())
lapply(c(1,3,5,7,9), function(x){
  points(dat$depth, predict(QR1[[x]], level=0), col=x+1, pch="*")
})

# stopCluster(cl)
endtime<-Sys.time()-start_time
endtime  


# dat$sr<-as.factor(dat$sr)
#Try additive model version, since most recent attempt to do lqmm led to lines crossing in range of data e.g. between quantiles 0.5 and 0.6. 
start_time<-Sys.time()
no_cores<-detectCores()-1
cl<-makeCluster(no_cores)
clusterExport(cl=cl, varlist=ls())
clusterEvalQ(cl, library(aqmm))
aQR1<-parLapply(cl, c(1:9), fun=function(x){
  aqmm(fixed = tongue_length.tongue~s(depth, k=9), random=~1, group=sr, data=dat, tau=x/10)
})
stopCluster(cl)
endtime<-Sys.time()-start_time
endtime  

start_time<-Sys.time()
no_cores<-detectCores()-1
cl<-makeCluster(no_cores)
clusterExport(cl=cl, varlist=ls())
clusterEvalQ(cl, library(aqmm))
aQR2<-parLapply(cl, c(1:9), fun=function(x){
  aqmm(fixed = tongue_length.tongue~s(depth, k=5), random=~depth, group=sr, data=dat, tau=x/10)
})
stopCluster(cl)
endtime<-Sys.time()-start_time
endtime  


# library(nlqmm)
dat<-dat %>% mutate(sq=depth^2, cu=depth^3)

start_time<-Sys.time()
no_cores<-detectCores()-1
cl<-makeCluster(no_cores)
clusterExport(cl=cl, varlist=ls())
clusterEvalQ(cl, library(lqmm))
cubetest<-parLapply(cl, 1:9, function(x){
#   lqmm(fixed=tongue_length.tongue~poly(depth, 3),random=~1, group=sampling_round, data=dat, tau=x/10,control= list(method="df", LP_max_iter=100000))
# })
  lqmm(fixed=tongue_length.tongue~poly(depth, 3, raw=TRUE),random=~1, group=sampling_round, data=dat, tau=x/10,control= list(method="df", LP_max_iter=100000))
})
ct2<-parLapply(cl, 1:9, function(x){
  lqmm(fixed=tongue_length.tongue~depth+sq+cu,random=~1, group=sampling_round, data=dat, tau=x/10,control= list(method="df", LP_max_iter=100000))
})
stopCluster(cl)
endtime<-Sys.time()-start_time
endtime  


ct3<-lqmm(fixed=tongue_length.tongue~depth+sq+cu,random=~depth+sq+cu, group=sr, data=dat, tau=0.5,control= list(method="df", LP_max_iter=100000))

ct4<-lqmm(fixed=tongue_length.tongue~poly(depth,3),random=~poly(depth,3), group=sr, data=dat, tau=0.5,control= list(method="df", LP_max_iter=100000))

plot(jitter(dat$depth, amount=0.35), jitter(dat$tongue_length.tongue, amount=0.1), pch=".", xlab="corolla depth", ylab="tongue length", xlim=c(0,33))
colrs<-c("red", "darkred", "blue", "darkblue", "darkgreen", "orange", "purple", "lightblue", "grey")
lapply(1:9, function(x){
  points(dat$depth, predict(cubetest[[x]], level=0), col=colrs[x], pch="*")
})


lapply(c(1,5,9), function(x){
  points(dat$depth, predict(aQR2[[x]], level=0), col=colrs[x], pch="*")
})

lapply(c(2,4,6,8), function(x){
  points(dat$depth, predict(aQR1[[x]], level=0), col=colrs[x], pch="*")
})



aqmm
cl<-makeCluster(no_cores)
clusterExport(cl=cl, varlist=ls())
clusterEvalQ(cl, library(lqmm))
QRsum<-parLapply(cl, QR1, fun=function(x){
 summary(x)
})
stopCluster(cl)
endtime<-Sys.time()-start_time
endtime  


four<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = site, tau=c(0.4), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
six<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.6), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
eight<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.8), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
nine<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.9), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))

# summary.lqmm(two)

# coef.lqmm(two)
# ranef.lqmm(two)

summary(two)
summary(four)
summary(six)
summary(eight)
summary(nine)

# tdiff<-start_time-Sys.time()
# print(tdiff)


### The same as before but dropping the bees that are smaller than the flowers and they crawl in

subsetgeneraldatawiderbees <- droplevels(dplyr::filter(subsetgeneraldata, beewider=="true"))

four<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.4), data=subsetgeneraldatawiderbees,  control= list(method="df", LP_max_iter=10000))
nine<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.9), data=subsetgeneraldatawiderbees,  control= list(method="df", LP_max_iter=10000 ))

summary(four)

ranef(nine)
summary(nine)
VarCorr(four)


####### lqmm with all the data included
# 
# > two<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.2), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
# > coef.lqmm(two)
# (Intercept)           depth sampling_round2 sampling_round3 sampling_round4 sampling_round5 
# 3.07470019      0.04813696      0.49959251     -0.13306594     -0.06414822     -0.06402975 
# > ranef.lqmm(two)
# (Intercept)      depth
# Baldpate       -1.9246477 0.26928748
# Cold Soil      -1.0417799 0.19900154
# Fox Hill       -3.1669060 0.11123971
# IAS            -2.9631951 0.42670969
# Lord Stirling  -0.7569019 0.05396013
# URWA           -3.0783832 0.30915231
# > tdiff<-start_time-Sys.time()
# > print(tdiff)
# Time difference of -4.926123 mins
# > start_time<-Sys.time()
# > summary(two)
# Call: lqmm(fixed = tongue_length.tongue ~ depth + sampling_round, random = ~depth, 
#            group = site, tau = c(0.2), data = subsetgeneraldata, control = list(method = "df", 
#                                                                                 LP_max_iter = 10000))
# 
# Quantile 0.2 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)      3.074700   0.727821    1.612090      4.5373 0.0001037 ***
#   depth            0.048137   0.095263   -0.143301      0.2396 0.6156093    
# sampling_round2  0.499593   0.319665   -0.142799      1.1420 0.1245213    
# sampling_round3 -0.133066   0.505071   -1.148043      0.8819 0.7933008    
# sampling_round4 -0.064148   0.345783   -0.759025      0.6307 0.8535902    
# sampling_round5 -0.064030   0.374149   -0.815910      0.6879 0.8648228    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# AIC:
#   [1] 61105 (df = 9)
# > four<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.4), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
# > start_time<-Sys.time()
# > summary(four)
# Call: lqmm(fixed = tongue_length.tongue ~ depth + sampling_round, random = ~depth, 
#            group = site, tau = c(0.4), data = subsetgeneraldata, control = list(method = "df", 
#                                                                                 LP_max_iter = 10000))
# 
# Quantile 0.4 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)      2.0557463  0.5982206   0.8535771      3.2579 0.0012103 ** 
#   depth            0.5132289  0.1241010   0.2638386      0.7626 0.0001383 ***
#   sampling_round2  0.0779143  0.3682285  -0.6620685      0.8179 0.8333028    
# sampling_round3  0.0070431  0.6392877  -1.2776536      1.2917 0.9912545    
# sampling_round4 -0.1397770  0.4837248  -1.1118584      0.8323 0.7738311    
# sampling_round5 -0.1022349  0.5703226  -1.2483410      1.0439 0.8584743    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# AIC:
#   [1] 63420 (df = 9)
# > tdiff<-start_time-Sys.time()
# > print(tdiff)
# Time difference of -21.68793 mins
# > six<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.6), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
# > summary(six)
# Call: lqmm(fixed = tongue_length.tongue ~ depth + sampling_round, random = ~depth, 
#            group = site, tau = c(0.6), data = subsetgeneraldata, control = list(method = "df", 
#                                                                                 LP_max_iter = 10000))
# 
# Quantile 0.6 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)      1.876022   0.853806    0.160234      3.5918   0.03276 *  
#   depth            0.494918   0.059928    0.374488      0.6153 7.709e-11 ***
#   sampling_round2  0.092909   0.862359   -1.640065      1.8259   0.91464    
# sampling_round3  0.092915   1.067218   -2.051740      2.2376   0.93098    
# sampling_round4 -0.209870   0.928657   -2.076077      1.6563   0.82215    
# sampling_round5 -0.192429   1.089140   -2.381137      1.9963   0.86049    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# AIC:
#   [1] 68306 (df = 9)
# Warning message:
#   In summary.lqmm(six) : Negative LR test (set to zero).
# 
# 
# 
# > eight<-lqmm(fixed = tongue_length.tongue~depth+sampling_round, random = ~depth, group = site, tau=c(0.8), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
# > summary(eight)
# Call: lqmm(fixed = tongue_length.tongue ~ depth + sampling_round, random = ~depth, 
#            group = site, tau = c(0.8), data = subsetgeneraldata, control = list(method = "df", 
#                                                                                 LP_max_iter = 10000))
# 
# Quantile 0.8 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)  
# (Intercept)      3.13431    1.19403     0.73482      5.5338  0.01153 *
#   depth            0.48690    0.18561     0.11390      0.8599  0.01158 *
#   sampling_round2  0.49444    0.74580    -1.00430      1.9932  0.51046  
# sampling_round3  0.61163    0.74970    -0.89494      2.1182  0.41854  
# sampling_round4  0.41893    0.83303    -1.25510      2.0930  0.61728  
# sampling_round5  0.43873    1.97971    -3.53964      4.4171  0.82553  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# AIC:
#   [1] 72473 (df = 9)
# 
# 
# Quantile 0.9 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)   
# (Intercept)      3.6373533  1.1099836   1.4067577      5.8679 0.001932 **
#   depth            0.0049032  0.2096522  -0.4164086      0.4262 0.981436   
# sampling_round2  0.1414956  1.2919420  -2.4547591      2.7378 0.913236   
# sampling_round3  0.0773398  0.5705534  -1.0692302      1.2239 0.892731   
# sampling_round4  0.0286330  0.5681584  -1.1131240      1.1704 0.960012   
# sampling_round5  0.0179729  1.5762176  -3.1495549      3.1855 0.990949   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# AIC:
#   [1] 74027 (df = 9)


###### dropping small bees

# Quantile 0.2 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)   
# (Intercept)      1.94408    0.84424     0.24751      3.6406 0.025579 *
# depth            0.33667    0.10908     0.11746      0.5559 0.003329 **
# sampling_round2  0.24925    0.38225    -0.51890      1.0174 0.517409   
# sampling_round3  0.13877    0.42001    -0.70528      0.9828 0.742516   
# sampling_round4  0.12094    0.33881    -0.55993      0.8018 0.722663   
# sampling_round5  0.13877    0.34992    -0.56442      0.8420 0.693408   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

pdf("trait_matching.pdf")
dat %>% group_by(bee, sr, tongue_length.tongue) %>% summarize(mfd=mean(depth), ABUND=n(), vari=sd(depth)) %>% ggplot(aes(tongue_length.tongue,mfd))+
  geom_point(aes(size=ABUND, alpha=0.3))+
  theme_classic()+
  geom_errorbar(aes(ymin=mfd-1.96*vari, ymax=mfd+1.96*vari))+
 
  scale_x_log10()+
  scale_y_log10()+
  xlim(c(0,25))+
  ylim(c(0,25))+
  geom_smooth()+
  labs(x="tongue length for bee species", y="average floral depth for flowers visited")

subsetgeneraldata %>% group_by(plant_gs, depth) %>% summarize(mtl=mean(tongue_length.tongue)) %>% ggplot(aes(depth, mtl))+
  geom_point()+
  theme_classic()+
  xlim(c(0,33))+
  ylim(c(0,33))+
  # scale_x_log10()+
  # scale_y_log10()+
  geom_smooth()+
  labs(x="corolla depth for plant species", y="mean visitor tongue length")
dev.off()

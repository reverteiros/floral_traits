
library(ggplot2)
library(lqmm)

source("scripts/script_traits.R")

subsetgeneraldata <- droplevels(dplyr::filter(generaldata, !is.na(depth) & !Bombus=="N"))
subsetgeneraldata$sampling_round <- as.character(subsetgeneraldata$sampling_round)
subsetgeneraldata <- as.data.frame(subsetgeneraldata)
subsetgeneraldata<-subsetgeneraldata[complete.cases(subsetgeneraldata),]
subsetgeneraldata$sampling_round <- as.character(subsetgeneraldata$sampling_round)
subsetgeneraldata$randomfactor <- paste(subsetgeneraldata$site, subsetgeneraldata$sampling_round, sep = "_")

subsetgeneraldata<-subsetgeneraldata %>% mutate(IT_improved=if_else((bee_genus == "Bombus"| bee_genus == "Xylocopa"), IT_mm, IT_mm/0.72))
subsetgeneraldata<-subsetgeneraldata %>% mutate(beewider=if_else(IT_improved>width, "true", "false"))

start_time<-Sys.time()

## Entire dataset
two<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.2), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
four<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.4), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
six<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.6), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
eight<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.8), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))
nine<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.9), data=subsetgeneraldata,  control= list(method="df", LP_max_iter=10000))

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

two<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.2), data=subsetgeneraldatawiderbees,  control= list(method="df", LP_max_iter=10000))
ranef(two)
summary(two)
VarCorr(two)

four<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.4), data=subsetgeneraldatawiderbees,  control= list(method="df", LP_max_iter=10000))
ranef(four)
summary(four)
VarCorr(four)

six<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.6), data=subsetgeneraldatawiderbees,  control= list(method="df", LP_max_iter=10000))
ranef(six)
summary(six)
VarCorr(six)

eight<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.8), data=subsetgeneraldatawiderbees,  control= list(method="df", LP_max_iter=10000))
ranef(eight)
summary(eight)
VarCorr(eight)

nine<-lqmm(fixed = tongue_length.tongue~depth, random = ~depth, group = randomfactor, tau=c(0.9), data=subsetgeneraldatawiderbees,  control= list(method="df", LP_max_iter=10000 ))
nineranef <- ranef(nine)
summary(nine)
varnine <- VarCorr(nine)


###### Results for the second part, removing the small bees that can crawl in

# Quantile 0.2
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)
# (Intercept) 3.450886   0.568904    2.307631      4.5941 1.853e-07 ***
#   depth       0.328012   0.153197    0.020152      0.6359   0.03726 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# AIC:
#   [1] 53825 (df = 5)
# Warning message:
#   In summary.lqmm(two) : Negative LR test (set to zero).
# 
# (Intercept)        depth
# Baldpate_1       -2.7303685  0.062839092
# Baldpate_2       -0.5227363 -0.052467887
# Baldpate_3        0.5403630 -0.091286901
# Baldpate_4       -2.2852911 -0.014836189
# Baldpate_5       -3.3416902 -0.170910731
# Cold Soil_1      -3.5822659  0.110178241
# Cold Soil_2      -3.2283768  0.138230972
# Cold Soil_3      -1.3211473  0.111152408
# Cold Soil_4      -1.1878845  0.419564639
# Cold Soil_5      -0.1452855  0.371485827
# Fox Hill_1       -4.3047312  0.190031508
# Fox Hill_2       -1.6815134 -0.197096870
# Fox Hill_3       -4.3202052  0.196942833
# Fox Hill_4       -3.7546761 -0.183569612
# Fox Hill_5       -3.9319802 -0.282816005
# IAS_1            -3.1441033  0.343560741
# IAS_2            -3.4570801  0.044282145
# IAS_3            -2.9691389  0.183605768
# IAS_4            -3.8701412  0.320889155
# IAS_5            -3.3816252  1.088211819
# Lord Stirling_1   2.0042999 -0.225297588
# Lord Stirling_2  -0.5448886 -0.163222043
# Lord Stirling_3  -1.6983144  0.482280623
# Lord Stirling_4  -0.2085057 -0.007526702
# Lord Stirling_5  -2.3349440  0.162436895
# URWA_1           -4.7707214  0.555607828
# URWA_2           -3.4584609  0.446588538
# URWA_3           -4.4283461  1.155843360
# URWA_4           -4.5216192  1.424326075
# URWA_5           -3.9399831  1.063025914
# 
# (Intercept)       depth 
# 1.039373    1.131600 
# 
# 
# 
# Quantile 0.4 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 4.02701    0.61312     2.79489      5.2591 3.099e-08 ***
#   depth       0.55319    0.13699     0.27790      0.8285  0.000189 ***
#   
#   (Intercept)         depth
# Baldpate_1      -2.25584958 -7.417117e-31
# Baldpate_2      -2.62134832 -7.457500e-29
# Baldpate_3      -1.91456559 -1.074720e-28
# Baldpate_4      -1.72356776 -1.256768e-29
# Baldpate_5      -3.63388354 -1.948287e-28
# Cold Soil_1     -3.06593253 -2.666877e-30
# Cold Soil_2     -2.84551477 -1.551051e-29
# Cold Soil_3     -0.78205531 -1.419224e-29
# Cold Soil_4      0.13313923  3.223528e-30
# Cold Soil_5      1.23238496  2.790252e-30
# Fox Hill_1      -3.50022081  1.361471e-32
# Fox Hill_2      -2.55925505 -3.373109e-29
# Fox Hill_3      -3.51708414 -5.736531e-30
# Fox Hill_4      -4.04123438 -7.836483e-29
# Fox Hill_5      -4.31839094 -1.458290e-28
# IAS_1           -1.58743924  5.461881e-30
# IAS_2           -3.32468701 -1.046742e-29
# IAS_3           -2.20923859 -3.343021e-30
# IAS_4           -2.78294420  4.617027e-30
# IAS_5           -0.59468960  1.464335e-29
# Lord Stirling_1  0.01116697 -5.432543e-30
# Lord Stirling_2 -1.88378326 -2.451086e-29
# Lord Stirling_3 -0.09442009  3.040136e-30
# Lord Stirling_4 -1.16761132 -1.443702e-29
# Lord Stirling_5 -1.87441360 -5.169622e-31
# URWA_1          -2.83545664  2.111895e-30
# URWA_2          -2.08115761  9.285510e-31
# URWA_3          -0.84805161  4.936328e-30
# URWA_4          -1.67915650  4.192008e-30
# URWA_5          -2.68111313  9.844706e-31
# 
# (Intercept)        depth 
# 6.351750e-01 4.930381e-32
# 
# 
# Quantile 0.6 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 2.94737    0.32178     2.30073      3.5940 3.414e-12 ***
#   depth       0.44095    0.10547     0.22900      0.6529 0.0001195 ***
# 
# 
# (Intercept)        depth
# Baldpate_1       0.28151756  0.069177853
# Baldpate_2       2.75166349 -0.157499107
# Baldpate_3       3.85590979 -0.198956229
# Baldpate_4       1.06826853 -0.120968190
# Baldpate_5       0.02038644 -0.282185634
# Cold Soil_1     -0.41189817  0.033800508
# Cold Soil_2      0.05872723  0.034169233
# Cold Soil_3      2.02191581  0.003912309
# Cold Soil_4      2.06382589  0.337792086
# Cold Soil_5      3.07811887  0.294865486
# Fox Hill_1      -1.37880845  0.213252567
# Fox Hill_2       1.62240962 -0.298146342
# Fox Hill_3      -0.99689951  0.089372852
# Fox Hill_4      -0.44305080 -0.292264887
# Fox Hill_5      -0.59312629 -0.393246861
# IAS_1           -0.01090618  0.265920464
# IAS_2           -0.22787182 -0.048966958
# IAS_3            0.33194707  0.081531533
# IAS_4           -0.55663324  0.219640791
# IAS_5           -0.13954261  1.013638264
# Lord Stirling_1  4.88617134 -0.269032991
# Lord Stirling_2  2.70320891 -0.258262454
# Lord Stirling_3  1.41222515  0.430587237
# Lord Stirling_4  2.85591733 -0.096664610
# Lord Stirling_5 -0.02061893  0.131259062
# URWA_1          -2.31106428  0.641517412
# URWA_2          -0.67612946  0.534455012
# URWA_3          -1.73526300  1.234000325
# URWA_4          -1.64785968  1.483422665
# URWA_5          -1.04800949  1.138853011
# 
# (Intercept)       depth 
# 1.360019    2.189325 
# 
# 
# 
# Quantile 0.8 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  3.73627    0.31013     3.11304      4.3595 2.923e-16 ***
#   depth        0.26522    0.23813    -0.21332      0.7438    0.2708   
# 
# (Intercept)       depth
# Baldpate_1        1.3220382  0.18789576
# Baldpate_2        3.6478116  0.01815925
# Baldpate_3        4.7557451 -0.02345576
# Baldpate_4        1.9633654  0.05314507
# Baldpate_5        0.9102621 -0.10577694
# Cold Soil_1       0.5069946  0.19795377
# Cold Soil_2       0.9550085  0.20861600
# Cold Soil_3       2.9250293  0.17688866
# Cold Soil_4       3.0909585  0.45190168
# Cold Soil_5       4.1492574  0.41702756
# Fox Hill_1       -0.2699025  0.29637545
# Fox Hill_2        2.4981964 -0.11843075
# Fox Hill_3       -0.1027812  0.26332512
# Fox Hill_4        0.4363237 -0.11463350
# Fox Hill_5        0.2897509 -0.21552855
# IAS_1             0.9713674  0.42435640
# IAS_2             0.6635761  0.12499721
# IAS_3             1.2387520  0.25231890
# IAS_4             0.3597025  0.37943152
# IAS_5             1.0379221  1.05682894
# Lord Stirling_1   5.7769962 -0.09066628
# Lord Stirling_2   3.5814271 -0.07963584
# Lord Stirling_3   2.5765713  0.50809358
# Lord Stirling_4   3.7719913  0.07646212
# Lord Stirling_5   0.9113381  0.27325677
# URWA_1           -0.7578426  0.60490124
# URWA_2            0.8151728  0.43831562
# URWA_3            0.3653492  0.99092325
# URWA_4            0.2984696  1.01342232
# URWA_5            0.2601743  0.43993739
# 
# (Intercept)       depth 
# 2.66005403  0.04985749
# 
# 
# Quantile 0.9 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)    
# (Intercept)  3.481649   0.322621    2.833319       4.130 1.51e-14 ***
#   depth        0.448939   0.228417   -0.010081       0.908  0.05504 .
# 
# (Intercept)        depth
# Baldpate_1        2.4519959  0.065814173
# Baldpate_2        4.8653801 -0.158307993
# Baldpate_3        5.9868455 -0.200816035
# Baldpate_4        3.2465092 -0.116194903
# Baldpate_5        2.2105699 -0.286930975
# Cold Soil_1       1.7252672  0.035212141
# Cold Soil_2       2.2279409  0.029357293
# Cold Soil_3       4.2064490 -0.001043507
# Cold Soil_4       4.3649023  0.270531131
# Cold Soil_5       5.3897833  0.246173402
# Fox Hill_1        0.9355783  0.145401032
# Fox Hill_2        3.7224523 -0.284663143
# Fox Hill_3        1.1931829  0.081849402
# Fox Hill_4        1.7149109 -0.291895646
# Fox Hill_5        1.5806597 -0.394256012
# IAS_1             2.2286609  0.247637261
# IAS_2             1.8990112 -0.045224809
# IAS_3             2.5155725  0.074658819
# IAS_4             1.6565223  0.197905708
# IAS_5             2.4723035  0.809860197
# Lord Stirling_1   6.2758177 -0.158860191
# Lord Stirling_2   4.7341151 -0.239407143
# Lord Stirling_3   3.8485121  0.322615513
# Lord Stirling_4   4.8195208 -0.084322997
# Lord Stirling_5   1.8482083  0.121572878
# URWA_1            0.6148755  0.388213273
# URWA_2            2.0785284  0.257920727
# URWA_3            2.0134849  0.681826258
# URWA_4            1.8428952  0.660451969
# URWA_5            1.4864063  0.234283039
# 
# (Intercept)       depth  
# 2.8861573   0.0347965 
# 
# 
# hist(nineranef$`(Intercept)`)
# abline(v= 3.481649)
# hist(nineranef$depth)
# abline(v= 0.448939)
# 
# (Intercept)  3.481649   0.322621    2.833319       4.130 1.51e-14 ***
#   depth        0.448939   0.228417   -0.010081       0.908  0.05504 .

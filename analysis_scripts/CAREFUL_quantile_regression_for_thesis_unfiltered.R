#########################################################
### run quantile regressions for Joe's thesis     #######
#########################################################

# packages
require(lqmm)

#data

joes_data<-read.csv("data/unflt.csv")
#drop the unused columns to remove NA
joes_data<-joes_data[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
joes_data$year<-factor(joes_data$year)
joes_data$round<-factor(joes_data$round)
joes_data<-joes_data[complete.cases(joes_data),]


# model quantiles with random slope and intercept for site
# look at tau= 0.05, 0.1, 0.25, 0.5, 0.75

fifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.05, data=joes_data,  control= list(method="df", LP_max_iter=1000000))

tenth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.1, data=joes_data,  control= list(method="df", LP_max_iter=1000000))

twentyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.25, data=joes_data,  control= list(method="df", LP_max_iter=1000000))

med<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.5, data=joes_data,  control= list(method="df", LP_max_iter=1000000))

sevtyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.75, data=joes_data,  control= list(method="df", LP_max_iter=1000000))

lowest<-summary(fifth)
scnd<-summary(tenth)
thrd<-summary(twentyfifth)
mdn<-summary(med)
uppr<-summary(sevtyfifth)

# lowest
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.05, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.05 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)   
# (Intercept)  4.319623   1.327111    1.652694      6.9866  0.00206 **
#   depth        0.055863   0.048913   -0.042432      0.1542  0.25897   
# round2       0.062345   0.263756   -0.467691      0.5924  0.81413   
# round3       0.140917   0.312129   -0.486330      0.7682  0.65364   
# round4       0.166401   0.354157   -0.545304      0.8781  0.64055   
# year2013    -0.099932   0.342182   -0.787572      0.5877  0.77149   
# year2014    -0.386779   0.238962   -0.866992      0.0934  0.11196   
# year2015    -0.465333   0.331463   -1.131432      0.2008  0.16666   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 1692 (p = 0)
# AIC:
#   [1] 47637 (df = 11)
# scnd
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.1, data = joes_data, control = list(method = "df", 
#                                                                      LP_max_iter = 1e+06))
# 
# Quantile 0.1 
# 
# Fixed effects:
#   Value  Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.1102e+00  1.0305e+00  2.0394e+00      6.1809 0.0002213 ***
#   depth        1.7573e-01  9.9067e-02 -2.3349e-02      0.3748 0.0822963 .  
# round2       5.1504e-05  3.4365e-01 -6.9053e-01      0.6906 0.9998810    
# round3       5.4008e-02  3.8270e-01 -7.1505e-01      0.8231 0.8883490    
# round4       2.1129e-01  5.8834e-01 -9.7102e-01      1.3936 0.7210390    
# year2013     5.6283e-04  2.3360e-01 -4.6887e-01      0.4700 0.9980874    
# year2014    -4.9466e-01  3.0010e-01 -1.0977e+00      0.1084 0.1056825    
# year2015    -5.2981e-01  4.9611e-01 -1.5268e+00      0.4672 0.2907917    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 3001 (p = 0)
# AIC:
#   [1] 46372 (df = 11)
# 
# 
#                                                          
# thrd
# Warning message:
#   In summary.lqmm(twentyfifth) : Negative LR test (set to zero).
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
# group = site, tau = 0.25, data = joes_data, control = list(method = "df", 
#                                                            LP_max_iter = 1e+06))
# 
# Quantile 0.25 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.454732   0.964574    2.516348      6.3931 2.824e-05 ***
#   depth        0.413711   0.085577    0.241738      0.5857 1.362e-05 ***
#   round2       0.547291   0.283113   -0.021646      1.1162   0.05901 .  
# round3       0.508541   0.295134   -0.084552      1.1016   0.09118 .  
# round4       0.545055   0.953018   -1.370106      2.4602   0.56999    
# year2013    -0.551265   0.422088   -1.399484      0.2970   0.19764    
# year2014    -0.891259   0.421905   -1.739109     -0.0434   0.03977 *  
#   year2015    -1.598393   0.497153   -2.597460     -0.5993   0.00231 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 0 (p = 1)
# AIC:
#   [1] 47402 (df = 11)

# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.5, data = joes_data, control = list(method = "df", 
#                                                                      LP_max_iter = 1e+06))
# 
# Quantile 0.5 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.448024   1.212448    2.011519      6.8845 0.0006008 ***
#   depth        0.434595   0.042414    0.349360      0.5198 8.918e-14 ***
#   round2       0.592143   0.828521   -1.072833      2.2571 0.4781866    
# round3       0.596656   0.819932   -1.051060      2.2444 0.4702669    
# round4       4.945317   2.148014    0.628722      9.2619 0.0256085 *  
#   year2013    -1.055083   0.538340   -2.136918      0.0268 0.0557097 .  
# year2014    -0.944577   0.583340   -2.116841      0.2277 0.1118114    
# year2015    -1.132592   0.685081   -2.509314      0.2441 0.1046792    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 538.2 (p = 0)
# AIC:
#   [1] 46999 (df = 11)

# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.75, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.75 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)    
# (Intercept)  5.38429    1.00239     3.36991      7.3987 2.14e-06 ***
#   depth        0.14571    0.12389    -0.10326      0.3947   0.2452    
# round2       0.35134    0.81270    -1.28183      1.9845   0.6674    
# round3       0.35694    0.99632    -1.64524      2.3591   0.7217    
# round4       2.24322    1.54698    -0.86555      5.3520   0.1534    
# year2013    -0.78119    0.72434    -2.23680      0.6744   0.2861    
# year2014    -0.77610    0.67680    -2.13619      0.5840   0.2571    
# year2015    -0.77712    0.67817    -2.13996      0.5857   0.2574    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 44.64 (p = 1.603e-07)
# AIC:
#   [1] 47257 (df = 11)

#############################
## rerun with 0 corolla omitted
#########################

nozero<-droplevels(joes_data[joes_data$depth!=0,])
fifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.05, data=nozero,  control= list(method="df", LP_max_iter=1000000))

tenth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.1, data=nozero,  control= list(method="df", LP_max_iter=1000000))

twentyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.25, data=nozero,  control= list(method="df", LP_max_iter=1000000))

med<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.5, data=nozero,  control= list(method="df", LP_max_iter=1000000))

sevtyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.75, data=nozero,  control= list(method="df", LP_max_iter=1000000))


lowest<-summary(fifth)
scnd<-summary(tenth)
thrd<-summary(twentyfifth)
# Warning message:
#   In summary.lqmm(twentyfifth) : Negative LR test (set to zero).
mdn<-summary(med)
uppr<-summary(sevtyfifth)Warning message:
#  In summary.lqmm(sevtyfifth) : Negative LR test (set to zero).

> hist(errs)
> plot(jitter(joes_dat$depth), resid(lm(meantongue~depth+year+round, data=joes_dat)))
> plot(joes_dat$round, resid(lm(meantongue~depth+year+round, data=joes_dat)))
> plot(joes_dat$year, resid(lm(meantongue~depth+year+round, data=joes_dat)))
> plot(fit, errs)
> require(lqmm)
> #data
  > joes_data<-read.csv("data/unflt.csv")
> #drop the unused columns to remove NA
  > joes_data<-joes_data[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
> joes_data$year<-factor(joes_data$year)
> joes_data$round<-factor(joes_data$round)
> joes_data<-joes_data[complete.cases(joes_data),]
> # model quantiles with random slope and intercept for site
  > # look at tau= 0.05, 0.1, 0.25, 0.5, 0.75
  > fifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.05, data=joes_data,  control= list(method="df", LP_max_iter=1000000))
> tenth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.1, data=joes_data,  control= list(method="df", LP_max_iter=1000000))
> twentyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.25, data=joes_data,  control= list(method="df", LP_max_iter=1000000))
> med<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.5, data=joes_data,  control= list(method="df", LP_max_iter=1000000))
> sevtyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.75, data=joes_data,  control= list(method="df", LP_max_iter=1000000))
# > lowest<-summary(fifth)
# > scnd<-summary(tenth)
# > thrd<-summary(twentyfifth)
# Warning message:
#   In summary.lqmm(twentyfifth) : Negative LR test (set to zero).
# > mdn<-summary(med)
# > uppr<-summary(sevtyfifth)
# > lowest
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.05, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.05 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)   
# (Intercept)  4.319623   1.327111    1.652694      6.9866  0.00206 **
#   depth        0.055863   0.048913   -0.042432      0.1542  0.25897   
# round2       0.062345   0.263756   -0.467691      0.5924  0.81413   
# round3       0.140917   0.312129   -0.486330      0.7682  0.65364   
# round4       0.166401   0.354157   -0.545304      0.8781  0.64055   
# year2013    -0.099932   0.342182   -0.787572      0.5877  0.77149   
# year2014    -0.386779   0.238962   -0.866992      0.0934  0.11196   
# year2015    -0.465333   0.331463   -1.131432      0.2008  0.16666   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 1692 (p = 0)
# AIC:
#   [1] 47637 (df = 11)
# > scnd
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.1, data = joes_data, control = list(method = "df", 
#                                                                      LP_max_iter = 1e+06))
# 
# Quantile 0.1 
# 
# Fixed effects:
#   Value  Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.1102e+00  1.0305e+00  2.0394e+00      6.1809 0.0002213 ***
#   depth        1.7573e-01  9.9067e-02 -2.3349e-02      0.3748 0.0822963 .  
# round2       5.1504e-05  3.4365e-01 -6.9053e-01      0.6906 0.9998810    
# round3       5.4008e-02  3.8270e-01 -7.1505e-01      0.8231 0.8883490    
# round4       2.1129e-01  5.8834e-01 -9.7102e-01      1.3936 0.7210390    
# year2013     5.6283e-04  2.3360e-01 -4.6887e-01      0.4700 0.9980874    
# year2014    -4.9466e-01  3.0010e-01 -1.0977e+00      0.1084 0.1056825    
# year2015    -5.2981e-01  4.9611e-01 -1.5268e+00      0.4672 0.2907917    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 3001 (p = 0)
# AIC:
#   [1] 46372 (df = 11)
# > third
# Error: object 'third' not found
# > thrd
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.25, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.25 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.454732   0.964574    2.516348      6.3931 2.824e-05 ***
#   depth        0.413711   0.085577    0.241738      0.5857 1.362e-05 ***
#   round2       0.547291   0.283113   -0.021646      1.1162   0.05901 .  
# round3       0.508541   0.295134   -0.084552      1.1016   0.09118 .  
# round4       0.545055   0.953018   -1.370106      2.4602   0.56999    
# year2013    -0.551265   0.422088   -1.399484      0.2970   0.19764    
# year2014    -0.891259   0.421905   -1.739109     -0.0434   0.03977 *  
#   year2015    -1.598393   0.497153   -2.597460     -0.5993   0.00231 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 0 (p = 1)
# AIC:
#   [1] 47402 (df = 11)
# > mdn
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.5, data = joes_data, control = list(method = "df", 
#                                                                      LP_max_iter = 1e+06))
# 
# Quantile 0.5 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.448024   1.212448    2.011519      6.8845 0.0006008 ***
#   depth        0.434595   0.042414    0.349360      0.5198 8.918e-14 ***
#   round2       0.592143   0.828521   -1.072833      2.2571 0.4781866    
# round3       0.596656   0.819932   -1.051060      2.2444 0.4702669    
# round4       4.945317   2.148014    0.628722      9.2619 0.0256085 *  
#   year2013    -1.055083   0.538340   -2.136918      0.0268 0.0557097 .  
# year2014    -0.944577   0.583340   -2.116841      0.2277 0.1118114    
# year2015    -1.132592   0.685081   -2.509314      0.2441 0.1046792    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 538.2 (p = 0)
# AIC:
#   [1] 46999 (df = 11)
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.5, data = joes_data, control = list(method = "df", 
#                                                                                     +                                                                      LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.5 
# Error: unexpected numeric constant in "Quantile 0.5"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value Std. Error lower bound upper bound  Pr(>|t|)    
# Error: unexpected symbol in "  Value Std."
# > (Intercept)  4.448024   1.212448    2.011519      6.8845 0.0006008 ***
#   Error: unexpected numeric constant in "(Intercept)  4.448024"
# >   depth        0.434595   0.042414    0.349360      0.5198 8.918e-14 ***
#   Error: unexpected numeric constant in "  depth        0.434595"
# >   round2       0.592143   0.828521   -1.072833      2.2571 0.4781866    
# Error: unexpected numeric constant in "  round2       0.592143"
# > round3       0.596656   0.819932   -1.051060      2.2444 0.4702669    
# Error: unexpected numeric constant in "round3       0.596656"
# > round4       4.945317   2.148014    0.628722      9.2619 0.0256085 *  
#   Error: unexpected numeric constant in "round4       4.945317"
# >   year2013    -1.055083   0.538340   -2.136918      0.0268 0.0557097 .  
# Error: unexpected numeric constant in "  year2013    -1.055083   0.538340"
# > year2014    -0.944577   0.583340   -2.116841      0.2277 0.1118114    
# Error: unexpected numeric constant in "year2014    -0.944577   0.583340"
# > year2015    -1.132592   0.685081   -2.509314      0.2441 0.1046792    
# Error: unexpected numeric constant in "year2015    -1.132592   0.685081"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 538.2 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 46999 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > uppr
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.75, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.75 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)    
# (Intercept)  5.38429    1.00239     3.36991      7.3987 2.14e-06 ***
#   depth        0.14571    0.12389    -0.10326      0.3947   0.2452    
# round2       0.35134    0.81270    -1.28183      1.9845   0.6674    
# round3       0.35694    0.99632    -1.64524      2.3591   0.7217    
# round4       2.24322    1.54698    -0.86555      5.3520   0.1534    
# year2013    -0.78119    0.72434    -2.23680      0.6744   0.2861    
# year2014    -0.77610    0.67680    -2.13619      0.5840   0.2571    
# year2015    -0.77712    0.67817    -2.13996      0.5857   0.2574    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 44.64 (p = 1.603e-07)
# AIC:
#   [1] 47257 (df = 11)
# > thrd
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.25, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.25 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.454732   0.964574    2.516348      6.3931 2.824e-05 ***
#   depth        0.413711   0.085577    0.241738      0.5857 1.362e-05 ***
#   round2       0.547291   0.283113   -0.021646      1.1162   0.05901 .  
# round3       0.508541   0.295134   -0.084552      1.1016   0.09118 .  
# round4       0.545055   0.953018   -1.370106      2.4602   0.56999    
# year2013    -0.551265   0.422088   -1.399484      0.2970   0.19764    
# year2014    -0.891259   0.421905   -1.739109     -0.0434   0.03977 *  
#   year2015    -1.598393   0.497153   -2.597460     -0.5993   0.00231 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 0 (p = 1)
# AIC:
#   [1] 47402 (df = 11)
# > set.seed(1)
# > x1 <- rnorm(100, 10, 2)
# > x2 <- rnorm(100, 100, 10)
# > x3 <- gl(n = 2, k = 50)
# > modmat <- model.matrix(~x1 + x2 + x3, data = data.frame(x1, x2, x3))
# > # vector of fixed effect
#   > betas <- c(10, 2, 0.2, 3)
# > # generate data
#   > y <- rnorm(n = 100, mean = modmat %*% betas, sd = 1)
# > # first model
#   > m <- lm(y ~ x1 + x2 + x3)
# > summary(m)
# 
# Call:
#   lm(formula = y ~ x1 + x2 + x3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.88054 -0.49476  0.03585  0.71034  2.66685 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.47570    1.25220   8.366 4.75e-13 ***
#   x1           2.01019    0.05856  34.329  < 2e-16 ***
#   x2           0.19380    0.01106  17.520  < 2e-16 ***
#   x32          3.13586    0.21086  14.872  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.047 on 96 degrees of freedom
# Multiple R-squared:  0.949,	Adjusted R-squared:  0.9474 
# F-statistic: 595.6 on 3 and 96 DF,  p-value: < 2.2e-16
# 
# > require(lqmm)
# > joes_data<-read.csv("data/measvisits.csv")
# > #drop the unused columns to remove NA
#   > joes_data<-joes_data[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
# > joes_data$year<-factor(joes_data$year)
# > joes_data$round<-factor(joes_data$round)
# > joes_data<-joes_data[complete.cases(joes_data),]
# > nozero<-joes_data[joes_data$depth!=0,]
# > nozero<-droplevels(joes_data[joes_data$depth!=0,])
# > fifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.05, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > tenth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.1, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > twentyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.25, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > med<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.5, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > sevtyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.75, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > lowest<-summary(fifth)
# > scnd<-summary(tenth)
# > thrd<-summary(twentyfifth)
# > mdn<-summary(med)
# > uppr<-summary(sevtyfifth)
# Warning message:
#   In summary.lqmm(sevtyfifth) : Negative LR test (set to zero).
# > joes_data<-read.csv("data/unflt.csv")
# > #drop the unused columns to remove NA
#   > joes_data<-joes_data[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
# > joes_data$year<-factor(joes_data$year)
# > joes_data$round<-factor(joes_data$round)
# > joes_data<-joes_data[complete.cases(joes_data),]
# > lowest
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.05, data = nozero, control = list(method = "df", 
#                                                                    LP_max_iter = 1e+06))
# 
# Quantile 0.05 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.994712   1.379130    2.223246      7.7662 0.0006935 ***
#   depth        0.490123   0.085609    0.318085      0.6622 6.188e-07 ***
#   round2       0.480715   0.762395   -1.051374      2.0128 0.5312755    
# round3       0.465231   0.794775   -1.131929      2.0624 0.5609905    
# round4       0.325154   0.743838   -1.169645      1.8200 0.6639374    
# year2013    -0.032132   0.514215   -1.065486      1.0012 0.9504282    
# year2014    -0.101096   0.459771   -1.025040      0.8228 0.8268749    
# year2015    -0.182779   0.511507   -1.210691      0.8451 0.7223753    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 5449 (p = 0)
# AIC:
#   [1] 37023 (df = 11)
# > scnd
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.1, data = nozero, control = list(method = "df", 
#                                                                   LP_max_iter = 1e+06))
# 
# Quantile 0.1 
# 
# Fixed effects:
#   Value  Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.3538e+00  1.0920e+00  2.1592e+00      6.5483 0.0002226 ***
#   depth        4.9101e-01  4.4414e-02  4.0176e-01      0.5803 6.488e-15 ***
#   round2       4.1289e-01  6.0628e-01 -8.0547e-01      1.6313 0.4990637    
# round3       4.1289e-01  6.5274e-01 -8.9885e-01      1.7246 0.5299730    
# round4       5.5612e-01  6.2747e-01 -7.0483e-01      1.8171 0.3797930    
# year2013    -6.6931e-07  3.4288e-01 -6.8905e-01      0.6891 0.9999985    
# year2014    -3.4592e-06  3.0388e-01 -6.1067e-01      0.6107 0.9999910    
# year2015    -2.7901e-04  4.4723e-01 -8.9903e-01      0.8985 0.9995048    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 1130 (p = 0)
# AIC:
#   [1] 36189 (df = 11)
# > thrd
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.25, data = nozero, control = list(method = "df", 
#                                                                    LP_max_iter = 1e+06))
# 
# Quantile 0.25 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound Pr(>|t|)    
# (Intercept)  3.242188   1.018039    1.196363      5.2880  0.00252 ** 
#   depth        0.452397   0.032276    0.387536      0.5173  < 2e-16 ***
#   round2       0.655151   0.508908   -0.367538      1.6778  0.20401    
# round3       0.599167   0.579102   -0.564583      1.7629  0.30591    
# round4       0.778482   1.344611   -1.923615      3.4806  0.56527    
# year2013    -0.047630   0.641801   -1.337377      1.2421  0.94114    
# year2014    -0.103792   0.598057   -1.305632      1.0980  0.86294    
# year2015    -0.156956   0.731045   -1.626046      1.3121  0.83089    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 267.9 (p = 0)
# AIC:
#   [1] 36158 (df = 11)
# > mdn
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.5, data = nozero, control = list(method = "df", 
#                                                                   LP_max_iter = 1e+06))
# 
# Quantile 0.5 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.87177    1.41758     2.02304      7.7205 0.0012094 ** 
#   depth        0.31470    0.07927     0.15540      0.4740 0.0002348 ***
#   round2       0.63080    1.23073    -1.84244      3.1040 0.6105725    
# round3       0.62779    1.31168    -2.00813      3.2637 0.6343406    
# round4       3.85653    2.00473    -0.17213      7.8852 0.0602098 .  
# year2013    -0.65683    0.53380    -1.72955      0.4159 0.2243938    
# year2014    -0.45883    0.51267    -1.48907      0.5714 0.3751678    
# year2015    -0.54428    0.52654    -1.60240      0.5138 0.3063557    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 736.3 (p = 0)
# AIC:
#   [1] 36078 (df = 11)
# > > lowest
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.05, data = nozero, control = list(method = "df", 
#                                                                                   +                                                                    LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.05 
# Error: unexpected numeric constant in "Quantile 0.05"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value Std. Error lower bound upper bound  Pr(>|t|)    
# Error: unexpected symbol in "  Value Std."
# > (Intercept)  4.994712   1.379130    2.223246      7.7662 0.0006935 ***
#   Error: unexpected numeric constant in "(Intercept)  4.994712"
# >   depth        0.490123   0.085609    0.318085      0.6622 6.188e-07 ***
#   Error: unexpected numeric constant in "  depth        0.490123"
# >   round2       0.480715   0.762395   -1.051374      2.0128 0.5312755    
# Error: unexpected numeric constant in "  round2       0.480715"
# > round3       0.465231   0.794775   -1.131929      2.0624 0.5609905    
# Error: unexpected numeric constant in "round3       0.465231"
# > round4       0.325154   0.743838   -1.169645      1.8200 0.6639374    
# Error: unexpected numeric constant in "round4       0.325154"
# > year2013    -0.032132   0.514215   -1.065486      1.0012 0.9504282    
# Error: unexpected numeric constant in "year2013    -0.032132   0.514215"
# > year2014    -0.101096   0.459771   -1.025040      0.8228 0.8268749    
# Error: unexpected numeric constant in "year2014    -0.101096   0.459771"
# > year2015    -0.182779   0.511507   -1.210691      0.8451 0.7223753    
# Error: unexpected numeric constant in "year2015    -0.182779   0.511507"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 5449 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 37023 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > > scnd
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.1, data = nozero, control = list(method = "df", 
#                                                                                  +                                                                   LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.1 
# Error: unexpected numeric constant in "Quantile 0.1"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value  Std. Error lower bound upper bound  Pr(>|t|)    
# Error: unexpected symbol in "  Value  Std."
# > (Intercept)  4.3538e+00  1.0920e+00  2.1592e+00      6.5483 0.0002226 ***
#   Error: unexpected numeric constant in "(Intercept)  4.3538e+00"
# >   depth        4.9101e-01  4.4414e-02  4.0176e-01      0.5803 6.488e-15 ***
#   Error: unexpected numeric constant in "  depth        4.9101e-01"
# >   round2       4.1289e-01  6.0628e-01 -8.0547e-01      1.6313 0.4990637    
# Error: unexpected numeric constant in "  round2       4.1289e-01"
# > round3       4.1289e-01  6.5274e-01 -8.9885e-01      1.7246 0.5299730    
# Error: unexpected numeric constant in "round3       4.1289e-01"
# > round4       5.5612e-01  6.2747e-01 -7.0483e-01      1.8171 0.3797930    
# Error: unexpected numeric constant in "round4       5.5612e-01"
# > year2013    -6.6931e-07  3.4288e-01 -6.8905e-01      0.6891 0.9999985    
# Error: unexpected numeric constant in "year2013    -6.6931e-07  3.4288e-01"
# > year2014    -3.4592e-06  3.0388e-01 -6.1067e-01      0.6107 0.9999910    
# Error: unexpected numeric constant in "year2014    -3.4592e-06  3.0388e-01"
# > year2015    -2.7901e-04  4.4723e-01 -8.9903e-01      0.8985 0.9995048    
# Error: unexpected numeric constant in "year2015    -2.7901e-04  4.4723e-01"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 1130 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 36189 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > > thrd
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.25, data = nozero, control = list(method = "df", 
#                                                                                   +                                                                    LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.25 
# Error: unexpected numeric constant in "Quantile 0.25"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value Std. Error lower bound upper bound Pr(>|t|)    
# Error: unexpected symbol in "  Value Std."
# > (Intercept)  3.242188   1.018039    1.196363      5.2880  0.00252 ** 
#   Error: unexpected numeric constant in "(Intercept)  3.242188"
# >   depth        0.452397   0.032276    0.387536      0.5173  < 2e-16 ***
#   Error: unexpected numeric constant in "  depth        0.452397"
# >   round2       0.655151   0.508908   -0.367538      1.6778  0.20401    
# Error: unexpected numeric constant in "  round2       0.655151"
# > round3       0.599167   0.579102   -0.564583      1.7629  0.30591    
# Error: unexpected numeric constant in "round3       0.599167"
# > round4       0.778482   1.344611   -1.923615      3.4806  0.56527    
# Error: unexpected numeric constant in "round4       0.778482"
# > year2013    -0.047630   0.641801   -1.337377      1.2421  0.94114    
# Error: unexpected numeric constant in "year2013    -0.047630   0.641801"
# > year2014    -0.103792   0.598057   -1.305632      1.0980  0.86294    
# Error: unexpected numeric constant in "year2014    -0.103792   0.598057"
# > year2015    -0.156956   0.731045   -1.626046      1.3121  0.83089    
# Error: unexpected numeric constant in "year2015    -0.156956   0.731045"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 267.9 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 36158 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > > mdn
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.5, data = nozero, control = list(method = "df", 
#                                                                                  +                                                                   LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.5 
# Error: unexpected numeric constant in "Quantile 0.5"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value Std. Error lower bound upper bound  Pr(>|t|)    
# Error: unexpected symbol in "  Value Std."
# > (Intercept)  4.87177    1.41758     2.02304      7.7205 0.0012094 ** 
#   Error: unexpected numeric constant in "(Intercept)  4.87177"
# >   depth        0.31470    0.07927     0.15540      0.4740 0.0002348 ***
#   Error: unexpected numeric constant in "  depth        0.31470"
# >   round2       0.63080    1.23073    -1.84244      3.1040 0.6105725    
# Error: unexpected numeric constant in "  round2       0.63080"
# > round3       0.62779    1.31168    -2.00813      3.2637 0.6343406    
# Error: unexpected numeric constant in "round3       0.62779"
# > round4       3.85653    2.00473    -0.17213      7.8852 0.0602098 .  
# Error: unexpected numeric constant in "round4       3.85653"
# > year2013    -0.65683    0.53380    -1.72955      0.4159 0.2243938    
# Error: unexpected numeric constant in "year2013    -0.65683    0.53380"
# > year2014    -0.45883    0.51267    -1.48907      0.5714 0.3751678    
# Error: unexpected numeric constant in "year2014    -0.45883    0.51267"
# > year2015    -0.54428    0.52654    -1.60240      0.5138 0.3063557    
# Error: unexpected numeric constant in "year2015    -0.54428    0.52654"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 736.3 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 36078 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > > lowest
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.05, data = nozero, control = list(method = "df", 
#                                                                                   +                                                                    LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.05 
# Error: unexpected numeric constant in "Quantile 0.05"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value Std. Error lower bound upper bound  Pr(>|t|)    
# Error: unexpected symbol in "  Value Std."
# > (Intercept)  4.994712   1.379130    2.223246      7.7662 0.0006935 ***
#   Error: unexpected numeric constant in "(Intercept)  4.994712"
# >   depth        0.490123   0.085609    0.318085      0.6622 6.188e-07 ***
#   Error: unexpected numeric constant in "  depth        0.490123"
# >   round2       0.480715   0.762395   -1.051374      2.0128 0.5312755    
# Error: unexpected numeric constant in "  round2       0.480715"
# > round3       0.465231   0.794775   -1.131929      2.0624 0.5609905    
# Error: unexpected numeric constant in "round3       0.465231"
# > round4       0.325154   0.743838   -1.169645      1.8200 0.6639374    
# Error: unexpected numeric constant in "round4       0.325154"
# > year2013    -0.032132   0.514215   -1.065486      1.0012 0.9504282    
# Error: unexpected numeric constant in "year2013    -0.032132   0.514215"
# > year2014    -0.101096   0.459771   -1.025040      0.8228 0.8268749    
# Error: unexpected numeric constant in "year2014    -0.101096   0.459771"
# > year2015    -0.182779   0.511507   -1.210691      0.8451 0.7223753    
# Error: unexpected numeric constant in "year2015    -0.182779   0.511507"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 5449 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 37023 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > > scnd
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.1, data = nozero, control = list(method = "df", 
#                                                                                  +                                                                   LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.1 
# Error: unexpected numeric constant in "Quantile 0.1"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value  Std. Error lower bound upper bound  Pr(>|t|)    
# Error: unexpected symbol in "  Value  Std."
# > (Intercept)  4.3538e+00  1.0920e+00  2.1592e+00      6.5483 0.0002226 ***
#   Error: unexpected numeric constant in "(Intercept)  4.3538e+00"
# >   depth        4.9101e-01  4.4414e-02  4.0176e-01      0.5803 6.488e-15 ***
#   Error: unexpected numeric constant in "  depth        4.9101e-01"
# >   round2       4.1289e-01  6.0628e-01 -8.0547e-01      1.6313 0.4990637    
# Error: unexpected numeric constant in "  round2       4.1289e-01"
# > round3       4.1289e-01  6.5274e-01 -8.9885e-01      1.7246 0.5299730    
# Error: unexpected numeric constant in "round3       4.1289e-01"
# > round4       5.5612e-01  6.2747e-01 -7.0483e-01      1.8171 0.3797930    
# Error: unexpected numeric constant in "round4       5.5612e-01"
# > year2013    -6.6931e-07  3.4288e-01 -6.8905e-01      0.6891 0.9999985    
# Error: unexpected numeric constant in "year2013    -6.6931e-07  3.4288e-01"
# > year2014    -3.4592e-06  3.0388e-01 -6.1067e-01      0.6107 0.9999910    
# Error: unexpected numeric constant in "year2014    -3.4592e-06  3.0388e-01"
# > year2015    -2.7901e-04  4.4723e-01 -8.9903e-01      0.8985 0.9995048    
# Error: unexpected numeric constant in "year2015    -2.7901e-04  4.4723e-01"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 1130 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 36189 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > > thrd
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.25, data = nozero, control = list(method = "df", 
#                                                                                   +                                                                    LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.25 
# Error: unexpected numeric constant in "Quantile 0.25"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value Std. Error lower bound upper bound Pr(>|t|)    
# Error: unexpected symbol in "  Value Std."
# > (Intercept)  3.242188   1.018039    1.196363      5.2880  0.00252 ** 
#   Error: unexpected numeric constant in "(Intercept)  3.242188"
# >   depth        0.452397   0.032276    0.387536      0.5173  < 2e-16 ***
#   Error: unexpected numeric constant in "  depth        0.452397"
# >   round2       0.655151   0.508908   -0.367538      1.6778  0.20401    
# Error: unexpected numeric constant in "  round2       0.655151"
# > round3       0.599167   0.579102   -0.564583      1.7629  0.30591    
# Error: unexpected numeric constant in "round3       0.599167"
# > round4       0.778482   1.344611   -1.923615      3.4806  0.56527    
# Error: unexpected numeric constant in "round4       0.778482"
# > year2013    -0.047630   0.641801   -1.337377      1.2421  0.94114    
# Error: unexpected numeric constant in "year2013    -0.047630   0.641801"
# > year2014    -0.103792   0.598057   -1.305632      1.0980  0.86294    
# Error: unexpected numeric constant in "year2014    -0.103792   0.598057"
# > year2015    -0.156956   0.731045   -1.626046      1.3121  0.83089    
# Error: unexpected numeric constant in "year2015    -0.156956   0.731045"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 267.9 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 36158 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > > mdn
# Error: unexpected '>' in ">"
# > Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#              +            group = site, tau = 0.5, data = nozero, control = list(method = "df", 
#                                                                                  +                                                                   LP_max_iter = 1e+06))
# Error: object 'Call' not found
# > 
#   > Quantile 0.5 
# Error: unexpected numeric constant in "Quantile 0.5"
# > 
#   > Fixed effects:
#   Error: unexpected symbol in "Fixed effects"
# >   Value Std. Error lower bound upper bound  Pr(>|t|)    
# Error: unexpected symbol in "  Value Std."
# > (Intercept)  4.87177    1.41758     2.02304      7.7205 0.0012094 ** 
#   Error: unexpected numeric constant in "(Intercept)  4.87177"
# >   depth        0.31470    0.07927     0.15540      0.4740 0.0002348 ***
#   Error: unexpected numeric constant in "  depth        0.31470"
# >   round2       0.63080    1.23073    -1.84244      3.1040 0.6105725    
# Error: unexpected numeric constant in "  round2       0.63080"
# > round3       0.62779    1.31168    -2.00813      3.2637 0.6343406    
# Error: unexpected numeric constant in "round3       0.62779"
# > round4       3.85653    2.00473    -0.17213      7.8852 0.0602098 .  
# Error: unexpected numeric constant in "round4       3.85653"
# > year2013    -0.65683    0.53380    -1.72955      0.4159 0.2243938    
# Error: unexpected numeric constant in "year2013    -0.65683    0.53380"
# > year2014    -0.45883    0.51267    -1.48907      0.5714 0.3751678    
# Error: unexpected numeric constant in "year2014    -0.45883    0.51267"
# > year2015    -0.54428    0.52654    -1.60240      0.5138 0.3063557    
# Error: unexpected numeric constant in "year2015    -0.54428    0.52654"
# > ---
#   +   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Error: unexpected symbol in:
#   "---
# Signif. codes"
# > Null model (likelihood ratio):
#   Error: unexpected symbol in "Null model"
# >   [1] 736.3 (p = 0)
# Error: unexpected '[' in "  ["
# > AIC:
#   +   [1] 36078 (df = 11)
# Error: unexpected '[' in:
#   "AIC:
# ["
# > uppr
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.75, data = nozero, control = list(method = "df", 
#                                                                    LP_max_iter = 1e+06))
# 
# Quantile 0.75 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  5.5506501  1.3564810   2.8246995      8.2766 0.0001591 ***
#   depth        0.0026175  0.1351205  -0.2689173      0.2742 0.9846234    
# round2      -0.0246977  0.8921707  -1.8175817      1.7682 0.9780277    
# round3      -0.1114281  1.0014330  -2.1238831      1.9010 0.9118578    
# round4      -0.1075178  1.3771260  -2.8749561      2.6599 0.9380871    
# year2013    -0.0046339  0.5432341  -1.0963037      1.0870 0.9932286    
# year2014    -0.0054588  0.5091464  -1.0286267      1.0177 0.9914893    
# year2015    -0.0063043  0.4808202  -0.9725486      0.9599 0.9895920    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 0 (p = 1)
# AIC:
#   [1] 35747 (df = 11)
# > nozero<-droplevels(joes_data[joes_data$depth!=0,])
# > fifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.05, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > tenth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.1, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > twentyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.25, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > med<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.5, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > sevtyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.75, data=nozero,  control= list(method="df", LP_max_iter=1000000))
# > 
#   > 
#   > 
#   > 
#   > 
#   > 
#   > 
#   > 
#   > lowest<-summary(fifth)
# > scnd<-summary(tenth)
# > thrd<-summary(twentyfifth)
# Warning message:
#   In summary.lqmm(twentyfifth) : Negative LR test (set to zero).
# > mdn<-summary(med)
# > uppr<-summary(sevtyfifth)
# Warning message:
#   In summary.lqmm(sevtyfifth) : Negative LR test (set to zero).
# # > lowest
# # Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
# #            group = site, tau = 0.05, data = nozero, control = list(method = "df", 
# #                                                                    LP_max_iter = 1e+06))
# # 
# # Quantile 0.05 
# # 
# # Fixed effects:
# #   Value Std. Error lower bound upper bound  Pr(>|t|)    
# # (Intercept)  4.380981   1.228498    1.912222      6.8497 0.0008207 ***
# #   depth        0.031963   0.090041   -0.148980      0.2129 0.7241208    
# # round2       0.051784   0.201559   -0.353265      0.4568 0.7983201    
# # round3       0.255702   0.218868   -0.184130      0.6955 0.2483413    
# # round4       0.286577   0.273617   -0.263276      0.8364 0.3000689    
# # year2013     0.084829   0.197574   -0.312212      0.4819 0.6695502    
# # year2014    -0.293345   0.202835   -0.700956      0.1143 0.1544804    
# # year2015    -0.340475   0.277062   -0.897251      0.2163 0.2249871    
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # Null model (likelihood ratio):
# #   [1] 1588 (p = 0)
# # AIC:
# #   [1] 44970 (df = 11)
# # > scnd
# # Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
# #            group = site, tau = 0.1, data = nozero, control = list(method = "df", 
# #                                                                   LP_max_iter = 1e+06))
# # 
# # Quantile 0.1 
# # 
# # Fixed effects:
# #   Value Std. Error lower bound upper bound  Pr(>|t|)    
# # (Intercept)  3.936396   0.935846    2.055742      5.8170 0.0001101 ***
# #   depth        0.157351   0.127834   -0.099542      0.4142 0.2242364    
# # round2      -0.032800   0.286689   -0.608923      0.5433 0.9093802    
# # round3       0.139946   0.359642   -0.582781      0.8627 0.6988687    
# # round4       0.136306   0.425118   -0.718000      0.9906 0.7498540    
# # year2013     0.135314   0.278670   -0.424694      0.6953 0.6294361    
# # year2014    -0.823270   0.281543   -1.389052     -0.2575 0.0052177 ** 
# #   year2015    -1.207596   0.391593   -1.994531     -0.4207 0.0033535 ** 
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # Null model (likelihood ratio):
# #   [1] 651 (p = 0)
# # AIC:
# #   [1] 45781 (df = 11)
# # > thrd
# # Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
# #            group = site, tau = 0.25, data = nozero, control = list(method = "df", 
# #                                                                    LP_max_iter = 1e+06))
# # 
# # Quantile 0.25 
# # 
# # Fixed effects:
# #   Value Std. Error lower bound upper bound  Pr(>|t|)    
# # (Intercept)  4.611745   0.882438    2.838420      6.3851 3.546e-06 ***
# #   depth        0.413451   0.089949    0.232692      0.5942 3.039e-05 ***
# #   round2       0.225870   0.414268   -0.606632      1.0584   0.58807    
# # round3       0.104119   0.452013   -0.804235      1.0125   0.81878    
# # round4       0.368218   1.017504   -1.676532      2.4130   0.71899    
# # year2013    -0.174526   0.400579   -0.979519      0.6305   0.66498    
# # year2014    -0.585217   0.417084   -1.423379      0.2529   0.16689    
# # year2015    -1.290493   0.610792   -2.517925     -0.0631   0.03973 *  
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # Null model (likelihood ratio):
# #   [1] 0 (p = 1)
# # AIC:
# #   [1] 44853 (df = 11)
# # > mdn
# # Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
# #            group = site, tau = 0.5, data = nozero, control = list(method = "df", 
# #                                                                   LP_max_iter = 1e+06))
# 
# Quantile 0.5 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  3.075767   1.139544    0.785768      5.3658  0.009513 ** 
#   depth        0.384796   0.055922    0.272415      0.4972 1.015e-08 ***
#   round2       0.796744   1.096336   -1.406426      2.9999  0.470847    
# round3       0.678121   1.216536   -1.766600      3.1228  0.579778    
# round4       4.629484   1.826431    0.959134      8.2998  0.014498 *  
#   year2013    -0.030668   0.559863   -1.155754      1.0944  0.956539    
# year2014    -0.219210   0.507780   -1.239632      0.8012  0.667850    
# year2015    -0.908737   0.586951   -2.088259      0.2708  0.128001    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 321.9 (p = 0)
# AIC:
#   [1] 44399 (df = 11)
# > uppr
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.75, data = nozero, control = list(method = "df", 
#                                                                    LP_max_iter = 1e+06))
# 
# Quantile 0.75 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  5.2612917  1.0961624   3.0584708      7.4641 1.532e-05 ***
#   depth        0.1325553  0.0856211  -0.0395069      0.3046    0.1280    
# round2       0.0022118  0.9184891  -1.8435611      1.8480    0.9981    
# round3       0.0019829  0.9472197  -1.9015264      1.9055    0.9983    
# round4       1.7125466  1.3991811  -1.0992130      4.5243    0.2268    
# year2013    -0.1900663  0.6746844  -1.5458953      1.1658    0.7794    
# year2014    -0.1901132  0.5943522  -1.3845087      1.0043    0.7504    
# year2015    -0.2106707  0.6480403  -1.5129564      1.0916    0.7465    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 0 (p = 1)
# AIC:
#   [1] 43814 (df = 11)

?predict.lqmm

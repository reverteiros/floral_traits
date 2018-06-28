#########################################################
### run quantile regressions for Joe's thesis     #######
#########################################################

# packages
require(lqmm)

#data

joes_data<-read.csv("data/measvisits.csv")
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

sevtyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.95, data=joes_data,  control= list(method="df", LP_max_iter=1000000))

lowest<-summary(fifth)
# returns error, maybe because fifth percentile is small?

# Error in optim(par = theta_0, fn = loglik.t, sigma = sigma_0, x = x, y = y,  : 
#                  non-finite value supplied by optim
#                Error in fit$theta : $ operator is invalid for atomic vectors
scnd<-summary(tenth)
thrd<-summary(twentyfifth)
mdn<-summary(med)
#negative LR
uppr<-summary(sevtyfifth)
#negative LR

# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.1, data = joes_data, control = list(method = "df", 
#                                                                      LP_max_iter = 1e+06))
# 
# Quantile 0.1 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.313942   1.071769    2.160141      6.4677 0.0001971 ***
#   depth        0.490185   0.043653    0.402461      0.5779 3.735e-15 ***
#   round2       0.052491   0.442068   -0.835879      0.9409 0.9059672    
# round3       0.159626   0.496406   -0.837940      1.1572 0.7491514    
# round4       0.123825   0.400117   -0.680241      0.9279 0.7582748    
# year2013    -0.042209   0.330610   -0.706595      0.6222 0.8989315    
# year2014    -0.042204   0.299440   -0.643951      0.5595 0.8884927    
# year2015    -0.043674   0.376514   -0.800307      0.7130 0.9081301    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 6710 (p = 0)
# AIC:
#   [1] 38309 (df = 11)


# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.25, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.25 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  3.5372150  0.9886719   1.5504044      5.5240 0.0007924 ***
#   depth        0.4495985  0.0325599   0.3841669      0.5150 < 2.2e-16 ***
#   round2       0.4030443  0.3887738  -0.3782260      1.1843 0.3049619    
# round3       0.3074221  0.4387611  -0.5743014      1.1891 0.4868296    
# round4       0.4455032  0.9796865  -1.5232504      2.4143 0.6513042    
# year2013     0.0036315  0.4617959  -0.9243822      0.9316 0.9937576    
# year2014     0.0036280  0.3778736  -0.7557374      0.7630 0.9923786    
# year2015     0.0138372  0.4376391  -0.8656316      0.8933 0.9749053    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 21.43 (p = 0.003186)
# AIC:
#   [1] 38361 (df = 11)
# 
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.5, data = joes_data, control = list(method = "df", 
#                                                                      LP_max_iter = 1e+06))
# 
# Quantile 0.5 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  2.907355   1.302006    0.290876      5.5238   0.03015 *  
#   depth        0.458117   0.062401    0.332717      0.5835 1.966e-09 ***
#   round2      -0.218832   1.158098   -2.546117      2.1085   0.85091    
# round3      -0.077049   1.113973   -2.315662      2.1616   0.94514    
# round4       4.189743   1.796532    0.579477      7.8000   0.02385 *  
#   year2013    -0.275110   0.471118   -1.221858      0.6716   0.56193    
# year2014    -0.134739   0.411998   -0.962681      0.6932   0.74503    
# year2015    -0.014673   0.714610   -1.450736      1.4214   0.98370    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 0 (p = 1)
# AIC:
#   [1] 39636 (df = 11)

# 
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
#            group = site, tau = 0.75, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 1e+06))
# 
# Quantile 0.75 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.96868    1.32278     2.31046      7.6269 0.0004588 ***
#   depth        0.16095    0.12259    -0.08541      0.4073 0.1953422    
# round2      -0.12494    0.83812    -1.80920      1.5593 0.8821114    
# round3      -0.12461    0.85112    -1.83499      1.5858 0.8842035    
# round4       1.95073    1.62504    -1.31491      5.2164 0.2357482    
# year2013    -0.36769    0.62221    -1.61807      0.8827 0.5572814    
# year2014    -0.36736    0.60948    -1.59216      0.8574 0.5494657    
# year2015    -0.36735    0.57041    -1.51363      0.7789 0.5225748    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 0 (p = 1)
# AIC:
#   [1] 40075 (df = 11)

# Quantile 0.9 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  5.0908912  0.9875677   3.1062996      7.0755 4.537e-06 ***
#   depth        0.3482223  0.1166129   0.1138798      0.5826  0.004401 ** 
#   round2      -0.0016152  0.3875663  -0.7804589      0.7772  0.996692    
# round3      -0.0551965  0.5543256  -1.1691556      1.0588  0.921089    
# round4       2.0223108  1.0338452  -0.0552789      4.0999  0.056166 .  
# year2013    -0.0140260  0.3904213  -0.7986070      0.7706  0.971488    
# year2014    -0.0676054  0.6052863  -1.2839738      1.1488  0.911524    
# year2015    -0.0646486  0.3593948  -0.7868794      0.6576  0.857987    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 71.6 (p = 7.002e-13)
# AIC:
#   [1] 39395 (df = 11)

#############################
## rerun with 0 corolla omitted
#########################

nozero<-droplevels(joes_data[joes_data$depth!=0,])
fifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.05, data=nozero,  control= list(method="df", LP_max_iter=1000000))

tenth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.1, data=nozero,  control= list(method="df", LP_max_iter=1000000))

twentyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.25, data=nozero,  control= list(method="df", LP_max_iter=1000000))

med<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.5, data=nozero,  control= list(method="df", LP_max_iter=1000000))

sevtyfifth<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.75, data=nozero,  control= list(method="df", LP_max_iter=1000000))

##### run summaries
lowest<-summary(fifth)
scnd<-summary(tenth)
thrd<-summary(twentyfifth)
mdn<-summary(med)

uppr<-summary(sevtyfifth)
#Warning message:
#In summary.lqmm(sevtyfifth) : Negative LR test (set to zero).

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
#Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~depth, 
# group = site, tau = 0.75, data = nozero, control = list(method = "df", 
#                                                         LP_max_iter = 1e+06))
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

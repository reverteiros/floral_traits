### this is a file to test out lqmm, a package that uses linear quantile regression with random effects. It was recently published in Science

## Muir et al 2015
# Science  05 Jun 2015:
# Vol. 348, Issue 6239, pp. 1135-1138
# DOI: 10.1126/science.1259911 

##############
#packages

require(lqmm)
require(quantreg)
#vignette("lqmm")

#############################
# #First, see that it works, with example from lqmm documentation
# I think we should skip this today, so if you're in RStudio, click the curly bracket to skip to line 60
{
  # ?lqmm
  # set.seed(123)
  # 
  # M <- 50
  # n <- 10
  # test <- data.frame(x = runif(n*M,0,1), group = rep(1:M,each=n))
  # test$y <- 10*test$x + rep(rnorm(M, 0, 2), each = n) + rchisq(n*M, 3)
  # 
  # plot(test$x, test$y)
  # 
  # fit.lqmm <- lqmm(fixed = y ~ x, random = ~ 1, group = group,	
  #                  data = test, tau = 0.5, nK = 11, type = "normal")
  # fit.lqmm
  
  ######################
  # #Now Orthodont is a data set that is supposed to actually meet the expectations of quantile regression. Let's also skip this
  # data(Orthodont)
  # fitOi.lqmm <- lqmm(distance ~ age, random = ~ 1, group = Subject,
  #                    tau = c(0.1,0.5,0.9), data = Orthodont)
  # coef(fitOi.lqmm)
  # 
  # fitOs.lqmm <- lqmm(distance ~ age, random = ~ age, group = Subject,
  #                    tau = c(0.1,0.5,0.9), cov = "pdDiag", data = Orthodont)
  # VarCorr(fitOs.lqmm)
  # coef(fitOs.lqmm)
  # ranef(fitOs.lqmm)
  # AIC(fitOi.lqmm)
  # AIC(fitOs.lqmm)
  
  ###################### 
  # #Here, we can compare the quantile regession with the glmm 
  # library("nlme")
  # data("Orthodont", package = "nlme")
  # Orthodont$Subject <- as.character(Orthodont$Subject)
  # Orthodont <- update(Orthodont, units = list(x = "(years)", y = "(mm)"), order.groups = FALSE)
  # summary(Orthodont)
  # head(Orthodont)
  # plot(Orthodont$age, Orthodont$distance)
  # simplereg<-lm(Orthodont$distance~Orthodont$age)
  # summary(simplereg)
  #### distance~age is highly significang with R2 of .257
  # abline(simplereg)
  # mixed<-nlme(distance~age+sex)
}

######################################
# Appendix S3. Quantile regression analysis.
# Supporting Online Material for:
# Title: Understanding productivity in richness relationships: causal networks as an alternative to bivariate plots.
# Authors: Grace, James B; Adler, Peter; Harpole, W; Borer, Elizabeth; Seabloom, Eric

##### QUANTILE REGRESSION OF ADLER DATA

# So they are comparing richness to productivity, measured as biomass. They are testing for a humped relationship between productivity (x-axis) and richness (y axis)

#read in data and drop rows with NA
a.dat <- read.csv("data/fec12269-sup-0005-AppendixS4.csv")
b.dat <- na.omit(a.dat)
# after dropping NAs, look at the data. They have plots nested in blocks in sites, with richenss and productivity measurements for each.
head(b.dat)

##### Create plot-level Objects

# Note from MR: This is not how I like to code. Rather than rename objects a bunch of times, I like to refer to them within the d.f. they come from. 
y <- b.dat$richness
x <- b.dat$live.biomass
site.code <- b.dat$site
# and so for these, I would just append the log transformed values to the table with descriptive headers
log.y <- log(y +1)
log.x <- log(x +1)
plot.dat <- data.frame(y, log.y, x, log.x, site.code)

##### Create site-level Objects by taking site-level means. I'd use dplyr for this, but here you go:
site.y <- as.vector(tapply(b.dat$richness, b.dat$site, mean))
site.x <- as.vector(tapply(b.dat$live.biomass, b.dat$site, mean))
log.site.y <- log(site.y + 1)
log.site.x <- log(site.x + 1)
site.dat <- data.frame(site.y, site.x, log.site.y, log.site.x)

#############################
# ANALYSES OF SITE-LEVEL DATA
#############################

##### Ricker equation fit of untransformed data superimposed on linear-scale plot
### Mean Regression on log-log plot using Ricker equation

# The Ricker equation is a general discrete time population model that projects population at time t+1 based on population at time t.
#It is exponential growth, but with carrying capacity. That's not that 
#relevant here. Instead, they choose it because it matches the hypothesis of 
#the hump shape. In particular, it is grounded at 0,0, and increases at a 
#decreasing rate toward the top of the hump. Then, it decreases at a decreasing rate to an asymptote, which makes sense because as productivity increases, 
#you don't expect richness to decline at an accelerating rate nor to cross #below 0. 

#here are their data
plot(site.y ~ site.x, pch=16, xlab="Production, g/m2", ylab="Species Richness")
#This approximates the ricker equation because of the log-log transformation, which will then be plotted on arithmetic axes

lm1 <- lm(log.site.y ~ log.site.x + site.x)
summary(lm1)
#again, they make new names for these things, which is something I avoid.
INT = as.numeric(lm1$coef[1])
B1 = as.numeric(lm1$coef[2])
B2 = as.numeric(lm1$coef[3])

# exponentiate to recover nonlinear form of eqn "y=beta0 * x^beta1 * e^(beta2*x)
beta0 = exp(INT)
beta1 = B1
beta2 = B2

xv <- seq(min(site.x),max(site.x),1)
yv.part1 <- beta0*xv^beta1 
yv.part2 <- exp(beta2*xv)
yv <- yv.part1*yv.part2
#print the predictions from the lm ont the graph
lines(xv,yv, lwd=2, lty=2, col="black")

### Compare model above to null model
lm2 <- lm(log.site.y ~ 1)
summary(lm2)
anova(lm1,lm2)
#their data show no difference from a null model that only estimates intercept

### Quantile Regressions
#This just replots the same data again
plot(site.y ~ site.x, pch=16, xlab="Production, g/m2", ylab="Species Richness", cex.lab=1.4, cex.axis=1.2, cex=1.1)

### Quantile Regression using quantreg
## 99th percentile
#this is with the package quantreg. It's been published with a lot. I think it uses different algorithms for estiamting quantiles and isn't restricted to linear functions. It does not deal with random effects. They do it for the within-site data
?rq
out1 <- rq(log.site.y ~ log.site.x + site.x, tau=0.99)
summary(out1, se="boot")
INT = as.numeric(out1$coef[1])
B1 = as.numeric(out1$coef[2])
B2 = as.numeric(out1$coef[3])

# exponentiate to recover nonlinear form of eqn "y=beta0 * x^beta1 * e^(beta2*x)
beta0 = exp(INT)
beta1 = B1
beta2 = B2

#xv <- seq(0,1400,1)
xv <- seq(min(site.x),max(site.x),1)
yv.part1 <- beta0*xv^beta1  
yv.part2 <- exp(beta2*xv)
yv <- yv.part1*yv.part2
lines(xv,yv, lwd=2, lty=2, col="red4")


## 95th percentile
out1 <- rq(log.site.y ~ log.site.x + site.x, tau=0.95)
summary(out1, se="boot")
INT = as.numeric(out1$coef[1])
B1 = as.numeric(out1$coef[2])
B2 = as.numeric(out1$coef[3])

# exponentiate to recover nonlinear form of eqn "y=beta0 * x^beta1 * e^(beta2*x)
beta0 = exp(INT)
beta1 = B1
beta2 = B2

#xv <- seq(0,1400,1)
xv <- seq(min(site.x),max(site.x),1)
yv.part1 <- beta0*xv^beta1  
yv.part2 <- exp(beta2*xv)
yv <- yv.part1*yv.part2
lines(xv,yv, lwd=2, lty=2, col="red1")


## 90th percentile
out1 <- rq(log.site.y ~ log.site.x + site.x, tau=0.90)
summary(out1, se="boot")
INT = as.numeric(out1$coef[1])
B1 = as.numeric(out1$coef[2])
B2 = as.numeric(out1$coef[3])

#One thing that struck me was that the lines cross. This is expected somewhere on the curve, if you think about it. 

# exponentiate to recover nonlinear form of eqn "y=beta0 * x^beta1 * e^(beta2*x)
beta0 = exp(INT)
beta1 = B1
beta2 = B2

#xv <- seq(0,1400,1)
xv <- seq(min(site.x),max(site.x),1)
yv.part1 <- beta0*xv^beta1  
yv.part2 <- exp(beta2*xv)
yv <- yv.part1*yv.part2
lines(xv,yv, lwd=2, lty=2, col="red")

#### etc 
### if you're working in R studio, click the curly bracket to skip many lines below... to 290
{
  ## 75th percentile
  out1 <- rq(log.site.y ~ log.site.x + site.x, tau=0.75)
  summary(out1, se="boot")
  INT = as.numeric(out1$coef[1])
  B1 = as.numeric(out1$coef[2])
  B2 = as.numeric(out1$coef[3])
  
  # exponentiate to recover nonlinear form of eqn "y=beta0 * x^beta1 * e^(beta2*x)
  beta0 = exp(INT)
  beta1 = B1
  beta2 = B2
  
  #xv <- seq(0,1400,1)
  xv <- seq(min(site.x),max(site.x),1)
  yv.part1 <- beta0*xv^beta1  
  yv.part2 <- exp(beta2*xv)
  yv <- yv.part1*yv.part2
  lines(xv,yv, lwd=2, lty=2, col="magenta1")
  
  ## 50th percentile
  out1 <- rq(log.site.y ~ log.site.x + site.x, tau=0.5)
  summary(out1, se="boot")
  INT = as.numeric(out1$coef[1])
  B1 = as.numeric(out1$coef[2])
  B2 = as.numeric(out1$coef[3])
  
  # exponentiate to recover nonlinear form of eqn "y=beta0 * x^beta1 * e^(beta2*x)
  beta0 = exp(INT)
  beta1 = B1
  beta2 = B2
  
  #xv <- seq(0,1400,1)
  xv <- seq(min(site.x),max(site.x),1)
  yv.part1 <- beta0*xv^beta1  
  yv.part2 <- exp(beta2*xv)
  yv <- yv.part1*yv.part2
  lines(xv,yv, lwd=2, lty=2, col="dark green")
  
  ## 25th percentile
  out1 <- rq(log.site.y ~ log.site.x + site.x, tau=0.25)
  summary(out1, se="boot")
  INT = as.numeric(out1$coef[1])
  B1 = as.numeric(out1$coef[2])
  B2 = as.numeric(out1$coef[3])
  
  # exponentiate to recover nonlinear form of eqn "y=beta0 * x^beta1 * e^(beta2*x)
  beta0 = exp(INT)
  beta1 = B1
  beta2 = B2
  
  #xv <- seq(0,1400,1)
  xv <- seq(min(site.x),max(site.x),1)
  yv.part1 <- beta0*xv^beta1  
  yv.part2 <- exp(beta2*xv)
  yv <- yv.part1*yv.part2
  lines(xv,yv, lwd=2, lty=2, col="skyblue")
  
  
  ##### Quantile Regression using Polynomials
  # let's definitely skip this. quantreg makes it easy to use polynomials of arbitrary degree like an lm
  ## raw variables - 90 percentile  -- tried various degrees, 2, 3, 4
  out1 <- rq(site.y ~ poly(site.x, degree = 2), tau=0.90)
  summary(out1, se="boot")
  
  ## log variables - 95 percentile
  out1 <- rq(log.site.y ~ poly(log.site.x, degree = 3), tau=0.95)
  summary(out1, se="boot")
  
  ## log variables - 90 percentile
  out1 <- rq(log.site.y ~ poly(log.site.x, degree = 3), tau=0.90)
  summary(out1, se="boot")
  
  ## log variables - 75 percentile
  out1 <- rq(log.site.y ~ poly(log.site.x, degree = 3), tau=0.75)
  summary(out1, se="boot")
  
  ## log variables - 50 percentile
  out1 <- rq(log.site.y ~ poly(log.site.x, degree = 3), tau=0.50)
  summary(out1, se="boot")
}

#############################
# ANALYSES OF PLOT-LEVEL DATA
#############################
# This is a more detailed dataset, but includes plots nested in site
# there's 1500 points or so on this plot.
plot(x,y)

##### ILLUSTRATION OF MEAN REGRESSION IGNORING CLUSTERING IN DATA
fit.lm.1 <- lm(log.y ~ log.x +x, data=b.dat)
summary(fit.lm.1)

##### QUANTILE REGRESSION WITH RANDOM EFFECTS

### NOTE, YOU MAY HAVE TO RUN THE SUMMARY STATEMENT MORE THAN ONCE TO GET A CONVERGED SOLUTION... OK, so here MR is confused.

### there is a warning of negative LR (likelihood ratio). I assume this tells you you have an unconverged solution... Did anybody get this? 

### There's a differnt warning that lets you know that you didn't get convergence, too. That's straight forward. Can run again, or can run with larger iterations. This is quick, so we can try that here. 

?lqmm

# Ricker eqn with random intercept
out.1 <- lqmm(fixed = log.y ~ x + log.x, random = ~1, group= site.code, tau=0.5, data=plot.dat, 
              control= list(LP_tol_11= 1e-04, LP_tol_theta = 1e-03, UP_tol = 1e-02, LP_max_iter = 100000))
summary(out.1)
#### this is just fitting the same model to different quantiles. I think there's a way to do this all at once, maybe...

### nb that each one returns an AIC value. I think they don't compare here for the sake of showing us that the coefficients on the fixed effects are not significant anyways. 
out.2 <- lqmm(fixed = log.y ~ x + log.x, random = ~1, group= site.code, tau=0.75, data=plot.dat, 
              control= list(LP_tol_11= 1e-02, LP_tol_theta = 1e-03, UP_tol = 1e-02, LP_max_iter = 10000))
summary(out.2)     

out.3 <- lqmm(fixed = log.y ~ x + log.x, random = ~1, group= site.code, tau=0.90, data=plot.dat, 
              control= list(LP_tol_11= 1e-02, LP_tol_theta = 1e-03, UP_tol = 1e-02, LP_max_iter = 10000))
summary(out.3)     

out.4 <- lqmm(fixed = log.y ~ x + log.x, random = ~1, group= site.code, tau=0.95, data=plot.dat, 
              control= list(LP_tol_11= 1e-02, LP_tol_theta = 1e-03, UP_tol = 1e-02, LP_max_iter = 10000))
summary(out.4)     

#### skip this, I think this is rhetorical on their part to say that a 3rd degree polynomial isn't better. 
{
  # Polynomial eqn with random intercept
  out.1alt <- lqmm(fixed = y ~ poly(x, degree = 3), random = ~1, group= site.code, tau=0.5, data=plot.dat, 
                   control= list(LP_tol_11= 1e-04, LP_tol_theta = 1e-04, UP_tol = 1e-03, LP_max_iter = 1000))
  print(out.1alt)
  summary(out.1alt)  
}

########################################################
######################
# Now let's look at Joe's data
#####################################################

joes_data<-read.csv("data/measvisits.csv")
#drop the unused columns to remove NA
joes_data<-joes_data[, c("genus_species", "site", "year", "gs", "round", "meantongue", "depth")]
joes_data$year<-factor(joes_data$year)
joes_data$round<-factor(joes_data$round)
joes_data<-joes_data[complete.cases(joes_data),]
levels(joes_data$site)
############# look at data
# str(joes_data)

########## quick bivariate plot to see all of the data
plot(joes_data$depth, joes_data$meantongue)
# what is that long corolla?
#joes_data[joes_data$depth>30,]
#Cirsium vulgare, bull thistle. I have caught lots of little bees of this- Ceratina in particular comes to mind. I think they're going for pollen. all 13 bees collected off it in CIG were female. Anthers are available outside corolla. 
# table(joes_data[joes_data$depth>30, "sex"])

########################### Make the model!

# Ok, we're predicting tongue (meantongue), with corolla depth (depth). 
# We want to recognize the effect of sampling round, and year (additional fixed effects)
# Then, we can include as random effects, site and plant species.

# so using lqmm:
?lqmm

#no random effects with this few iterations is instant
fixedonly<-lqm(meantongue~depth+round+year, tau=0.5, data=joes_data, control= list(LP_max_iter = 100))

# to get this to run I had to change method to df.
runlqmm<-lqmm(fixed = meantongue~depth, random = ~depth, group = site, tau=0.5, data=joes_data, covariance="pdIdent", control= list(method="df",LP_max_iter = 500, UP_max_iter=500))

#The above ran. Now going to go more complicated again.

runlqmm<-lqmm(fixed = meantongue~depth, random = ~1, group = site, tau=0.5, data=joes_data, covariance="pdIdent", control= list(method="df"))
summary(runlqmm)

# still fast without maximum iterations
# summary command took 20 or 30 secs
# now try without specifying pdIdent
runlqmm<-lqmm(fixed = meantongue~depth, random = ~1, group = site, tau=0.5, data=joes_data,  control= list(method="df"))
# quick enough

#now add back fixed effects
runlqmm<-lqmm(fixed = meantongue~depth+round+year, random = ~1, group = site, tau=0.5, data=joes_data,  control= list(method="df"))
#takes under a minute, converges

# now instead of random intercept, use site as random var... not sure quite what the diff is...
# this fails
runlqmm<-lqmm(fixed = meantongue~depth+round+year, random = ~site, group = site, tau=0.5, data=joes_data,  control= list(method="df"))

# have been unable to include flower species as an effect, fixed or random. 

med<-lqmm(fixed = meantongue~depth+round+year, random = ~depth, group = site, tau=0.5, data=joes_data,  control= list(method="df", LP_max_iter=10000))
summary(med)
## plot actual quantiles (in a different color)

#meaning of intercept is control bee community- the ones landing on the 0 flowers

# look at 10%ile (takes a minute or two to run summary)
tenth<-lqmm(fixed = meantongue~depth+round+year, random = ~1, group = site, tau=0.1, data=joes_data,  control= list(method="df", LP_max_iter=10000))
summary(tenth)
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~1, 
#            group = site, tau = 0.1, data = joes_data, control = list(method = "df", 
#                                                                      LP_max_iter = 10000))
# 
# Quantile 0.1 
# 
# Fixed effects:
#   Value  Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  3.29438336  0.83079525  1.62483780      4.9639 0.0002383 ***
#   depth        0.47861142  0.04402428  0.39014133      0.5671 1.168e-14 ***
#   round2       0.20706984  0.35465437 -0.50563480      0.9198 0.5619902    
# round3       0.14836586  0.46303888 -0.78214561      1.0789 0.7500142    
# round4      -0.03105635  0.44299247 -0.92128304      0.8592 0.9443948    
# year2013    -0.08119580  0.24250156 -0.56852093      0.4061 0.7391856    
# year2014     0.00055043  0.23828767 -0.47830657      0.4794 0.9981663    
# year2015    -0.28159602  0.31506461 -0.91474206      0.3516 0.3758127    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 5596 (p = 0)
# AIC:
#   [1] 39437 (df = 10)

ofive<-med<-lqmm(fixed = meantongue~depth+round+year, random = ~1, group = site, tau=0.05, data=joes_data,  control= list(method="df",LP_max_iter=10000))
summary(ofive)
# Call: lqmm(fixed = meantongue ~ depth + round + year, random = ~1, 
#            group = site, tau = 0.05, data = joes_data, control = list(method = "df", 
#                                                                       LP_max_iter = 10000))
# 
# Quantile 0.05 
# 
# Fixed effects:
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  4.037479   1.194631    1.636779      6.4382  0.001431 ** 
#   depth        0.397836   0.073018    0.251102      0.5446 1.635e-06 ***
#   round2       0.288185   0.700511   -1.119545      1.6959  0.682579    
# round3       0.176202   0.854686   -1.541355      1.8938  0.837521    
# round4      -0.201592   0.792492   -1.794164      1.3910  0.800268    
# year2013    -0.055719   0.418565   -0.896858      0.7854  0.894644    
# year2014    -0.104365   0.384839   -0.877728      0.6690  0.787383    
# year2015     0.091551   0.433305   -0.779207      0.9623  0.833541    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Null model (likelihood ratio):
#   [1] 3856 (p = 0)
# AIC:
#   [1] 41374 (df = 10)
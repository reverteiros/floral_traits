## In this script, we are making fake data sets to test our modelss
require(plyr)
require(tidyr)
require(reshape)
require(lqmm)
## define what the vectors are going to look like
a<-c(1:400)
slps<-(0:199/200)

meanf<- function (x){
  out=rnorm(400, mean=slps[x]*a, sd = 1)
  return(out)
}

newdata<-ldply(1:200, meanf)
newdata<-melt(newdata)
#assign random sites to data
newdata$site<-sample(1:10, 4000, replace=T)

plot(as.numeric(newdata$variable), newdata$value)
newdata$variable<-as.numeric(newdata$variable)
newdata$site<-as.factor(newdata$site)

newdata<-newdata[newdata$variable<21,]
######################################################################
test5<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.05, data=newdata,  control= list(method="df", LP_max_iter=100000))
summary(test5)
## negative LR
# Value  Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) -6.1440e-07  1.4937e-01 -3.0017e-01      0.3002 0.9999967    
# variable     2.6705e+00  6.4720e-01  1.3699e+00      3.9711 0.0001425 ***

test10<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.0505, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test10)
## no negative LR
# Value Std. Error lower bound upper bound Pr(>|t|)
# (Intercept) -0.062504   0.149051   -0.362033      0.2370   0.6768
# variable     0.062501   0.052405   -0.042810      0.1678   0.2387

test25<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.25, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test25)
## no negative LR
# Value  Std. Error lower bound upper bound Pr(>|t|)    
# (Intercept)  6.7171e-07  1.3076e-01 -2.6277e-01      0.2628        1    
# variable     2.0000e-01  8.9529e-03  1.8201e-01      0.2180   <2e-16 ***

test50<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.5, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test50)
## no negative LR
# Value  Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept)  8.7646e-05  1.1585e-01 -2.3272e-01      0.2329    0.9994    
# variable     4.4996e-01  5.2087e-02  3.4529e-01      0.5546 2.051e-11 ***

test75<-lqmm(fixed = value~variable, random = ~variable, group = site, tau=0.75, data=newdata,  control= list(method="df", LP_max_iter=1000000))
summary(test75)
## no negative LR
# Value  Std. Error lower bound upper bound Pr(>|t|)    
# (Intercept) -8.1936e-07  8.8066e-02 -1.7698e-01      0.1770        1    
# variable     7.0000e-01  2.0316e-02  6.5917e-01      0.7408   <2e-16 ***

################################################################################# ## now create a random variable of site
newdata$site<-sample(1:10, 20, replace = T)
## add a column for row numbers (I'm sure there is a way to do this, but I don't know how)
newdata$row<-1:20
plot(newdata$row, newdata$V1)
points(newdata$row, newdata$V2)
points(newdata$row, newdata$V3)
points(newdata$row, newdata$V4)
points(newdata$row, newdata$V5)
points(newdata$row, newdata$V6)
points(newdata$row, newdata$V7)
points(newdata$row, newdata$V8)
points(newdata$row, newdata$V9)
points(newdata$row, newdata$V10)
points(newdata$row, newdata$V11)
points(newdata$row, newdata$V12)
points(newdata$row, newdata$V13)
points(newdata$row, newdata$V14)
points(newdata$row, newdata$V15)
points(newdata$row, newdata$V16)
points(newdata$row, newdata$V17)
points(newdata$row, newdata$V18)
points(newdata$row, newdata$V19)
points(newdata$row, newdata$V20)
###################################################################################
# gave up on smart way....this is the dumb way of making a fake data set
a<-c(1:20)
b<-rnorm(20, mean = a + 2)
c<-rnorm(20, mean = 0.9*a + 2)
d<-rnorm(20, mean = 0.8*a + 2)
e<-rnorm(20, mean = 0.7*a + 2)
f<-rnorm(20, mean = 0.6*a + 2)
g<-rnorm(20, mean = 0.5*a + 2)
h<-rnorm(20, mean = 0.4*a + 2)
i<-rnorm(20, mean = 0.3*a + 2)
j<-rnorm(20, mean = 0.2*a + 2)
k<-rnorm(20, mean = 0.1*a + 2)
l<-rnorm(20, mean = 0*a + 2)
dumbdata<-data.frame(a, b, c, d, e, f, g, h, i, j, k, l)
#what does dumbdata look like?
plot(dumbdata$a, dumbdata$b)
points(dumbdata$a, dumbdata$c)
points(dumbdata$a, dumbdata$d)
points(dumbdata$a, dumbdata$e)
points(dumbdata$a, dumbdata$f)
points(dumbdata$a, dumbdata$g)
points(dumbdata$a, dumbdata$h)
points(dumbdata$a, dumbdata$i)
points(dumbdata$a, dumbdata$j)
points(dumbdata$a, dumbdata$k)
points(dumbdata$a, dumbdata$l)
# data looks fine
# add a random site for each of the rows
dumbdata$site<-sample(1:10, 20, replace = T)
#make data easier to analyze
write.csv(dumbdata, 'data/dumbdata2.csv')
## to save time ... I made manual edits in excel to dumbdata2.csv. I copy and pasted all the columns from c-l into column b. Now there are two columns of data (a and b). Each entry in the original columns c-l is paired with its original number from column a
dumbtest<-read.csv('data/dumbdata_test.csv')
#add a random site to the dumbtest data
dumbtest$site<-sample(1:10, 220, replace = T)
#lets see what the dumbtest looks like now
plot(dumbtest$a, dumbtest$b)
####################################################################################
## now I want to run some quantile regression using original dumbdata
require(lqmm)
test5<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.05, data=dumbdata,  control= list(method="df", LP_max_iter=1000000))
summary(test5)
# Value Std. Error lower bound upper bound Pr(>|t|)    
# (Intercept)  0.289081   0.273972   -0.261486      0.8396   0.2965    
# a            1.014318   0.016583    0.980994      1.0476   <2e-16 ***

test25<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.25, data=dumbdata,  control= list(method="df", LP_max_iter=1000000))
summary(test25)
# Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 1.217088   0.390200    0.432952      2.0012  0.003036 ** 
#   a           0.970111   0.045593    0.878487      1.0617 < 2.2e-16 ***

test50<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.5, data=dumbdata,  control= list(method="df", LP_max_iter=1000000))
summary(test50)
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 1.442942   0.515642    0.406720      2.4792  0.007324 ** 
#   a           1.034992   0.066346    0.901664      1.1683 < 2.2e-16 ***

test90<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.90, data=dumbdata,  control= list(method="df", LP_max_iter=1000000))
summary(test90)
#   Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 2.078143   0.408197    1.257840      2.8984 5.657e-06 ***
#   a           1.070980   0.054218    0.962025      1.1799 < 2.2e-16 ***


##########################################################################################
## now I want to run some quantile regression using the new and improved dumbdata
require(lqmm)
test5<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.05, data=dumbtest,  control= list(method="df", LP_max_iter=1000000))
summary(test5)
# Value Std. Error lower bound upper bound Pr(>|t|)   
# (Intercept)  1.36605    0.49220     0.37694      2.3552 0.007784 **
#   a            1.39272    1.35218    -1.32458      4.1100 0.308076 

test10<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.10, data=dumbtest,  control= list(method="df", LP_max_iter=1000000))
summary(test10)

test25<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.25, data=dumbtest,  control= list(method="df", LP_max_iter=1000000))
summary(test25)
# Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 2.162544   0.307705    1.544188      2.7809 6.004e-09 ***
#   a           0.197728   0.063909    0.069298      0.3262   0.00326 ** 

test50<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.5, data=dumbtest,  control= list(method="df", LP_max_iter=1000000))
summary(test50)
# Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 2.151248   0.323084    1.501987      2.8005 2.244e-08 ***
#   a           0.476018   0.071127    0.333082      0.6190 1.987e-08 ***

test90<-lqmm(fixed = b~a, random = ~a, group = site, tau=0.90, data=dumbtest,  control= list(method="df", LP_max_iter=1000000))
summary(test90)
# Value Std. Error lower bound upper bound  Pr(>|t|)    
# (Intercept) 2.530222   0.219293    2.089535      2.9709 1.413e-15 ***
#   a           0.895037   0.037042    0.820598      0.9695 < 2.2e-16 ***

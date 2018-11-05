# look at observed CV with sample size given sigma, mu
library(plyr)
library(tidyverse)

sigma<-seq(.1,.3,.05)
mu<-seq(1,4,1)

out<-ldply(lapply(rep(seq(1,20,1),100), function(n){ldply(lapply(sigma, function(x){ldply(lapply(mu, function(y){
  raw<-rnorm(n,y,y*x)
  av<-mean(raw)
  std<-sd(raw)
  CV<-std/av
  return(cbind("n"=n, "mu"=y, "sigma"=x*y,"CV_obs"=CV, "mu_obs"=av))
}))}))}))


out %>% ggplot(aes(n,CV_obs))+
  geom_jitter(alpha=0.2)+
  facet_wrap(~mu+sigma)+theme_classic()+ylim(c(0,1))

out %>% ggplot(aes(n,mu_obs))+
  geom_jitter(alpha=0.2)+
  facet_wrap(~mu+sigma)+theme_classic()+ylim(c(0,20))



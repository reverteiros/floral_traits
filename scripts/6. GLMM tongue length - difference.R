
source("scripts/1. Data cleaning.R")

library(ggbeeswarm)
library(ggplot2)
library(purrr)
library(ggExtra)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)
library(lme4)
library(multcomp)
library(MuMIn)
library(caret)
library(lme4)
library(lmerTest)
library(AER)

observed<-generaldata %>%
  group_by(site,sampling_round,bee) %>%
  summarize(mean_obs=mean(difference),abundance=n(),tongue=mean(tongue_length.tongue))


############ Lineal model to see relationship between observed difference and togue length per site-round

hist(observed$mean_obs)

fit <- lmer(mean_obs ~ tongue  + (tongue)^2 + (1|site) + (1|sampling_round), data=observed)
car::vif(fit)
qqPlot(resid(fit),envelope = 0.95)
plot(fit, which = 1)
hist(resid(fit))
summary(fit)

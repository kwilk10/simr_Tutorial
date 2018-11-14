## This script walks through the simr package for R
## simr works with linear mixed models and generalized linear mixed models
## and uses Monte Carlo simulations to estimate power for various sample sizes
## 
## In this walk through we focus on a linear mixed model with normal responses
## and nested effects. 
## 
## Author: Katherine Wilkinson
## Last updated: 11/14/2018

#install.packages('simr')
#install.packages('data.table')
#install.packages('dplyr')
library(simr); library(data.table); library(dplyr)


#Data adapted from:
#https://humburg.github.io/Power-Analysis/simr_power_analysis.html

## 4 patients
subj <- factor(1:4)
## 3 Centers
center_id <- letters[1:3]
## 2 Measurements (one at Time 0, one at Time 1)
time <- 0:1
## Control and Treatment groups
group <- c("C", "T")

## A total of 12 subjects, each with 2 measurements
  # To get a total of 24 rows
subj_full <- rep(subj, 6)
center_full <- rep(rep(center_id, each=4), 2)
time_full <- rep(time, each=12)
group_full <- rep(rep(group, each=2), 6)

## Combine all the variables together into one data frame
covars <- data.frame(Subject=as.character(subj_full), Center= as.factor(center_full), 
                     Treatment =as.character(group_full), Time=factor(time_full))

## Specify the parameters to work with makeLmer ##

## Intercept and slopes for fixed effecs:
fixed <- c(2.5, 0.01, 0.5, 1.5)
## Variance for participants clustered by Center
rand <- list(0.2, 0.1)
## residual variance
res <- 1.5

## use data and specified parameters to create lmer object to work with simr package
model <- makeLmer(y ~ Treatment*Time + (1|Center/Subject), fixef=fixed, VarCorr=rand, data=covars,
                  sigma = res)
## This give our model output
model

## Simulate and estimate power with simr
sim_treat <- powerSim(model, nsim=100, test = fcompare(y~Time), alpha = 0.05,
                      seed = 123)
sim_treat

## Change effect size and simulate to get power estimate
model_large <- model
fixef(model_large)['TreatmentT:Time1'] <- 2

sim_treat_large <- powerSim(fit = model_large, nsim=100, test = fcompare(y~Time),
                            seed = 123)

## Adjust sample size by increasing number of subjects in each center
model2 <- extend(object = model, along = "Subject", n = 6)

## Simulate to estimate power
powerSim(fit = model2, nsim=100, test = fcompare(y~Time),
         seed = 123)

## Adjust sample size by increasing number of subjects within center, treatment and time 
model3 <- extend(object = model, within = "Center+Treatment+Time", n=6)
model3

## Simulate to estimate power
powerSim(fit = model3, nsim=100, test = fcompare(y~Time),
         seed = 123)

## Use powerCurve function to estimate power for a range of sample sizes 
model5 <- extend(object = model, along = "Subject", n = 20)
subj_curv <- powerCurve(fit = model5, test=fcompare(y~Time), along = "Subject", nsim = 100,
                        seed = 123)
#get output
subj_curv
#create plot 
plot(subj_curv)

library(tidyverse)
library(rstan)
library(shinystan)

load("demofit.RData")

hm_trials <- 10 #exp trials: when reading from sim data, each trial is a randomly selected row/sample: contains 1 instance of each choice.

datalist <- list(hm_trials=nrow(trials.df),
                 hm_options=3,
                 hm_attributes=2,
                 choices=trials.df$choice


## fit <- stan(file="obsinput.stan",
##             data=datalist,
##             iter=1000,
##             init=function(){
##                 zeros <- rep(0.5,nrow(rawstim.df)*3*2) #trials x options x attributes. Need to consider what counts as a good init value!
##                 dim(zeros)=c(nrow(rawstim.df),3,2)
##                 list(est_trial_option_attribute=zeros)
##             },
##             chains=4,
##             control=list(max_treedepth=15)
##             )

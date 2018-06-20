library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)
rm(list=ls())
##set.seed(4);
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load("calc_ord_obsdfs.RData") #supplies calcobs.df, ordobs.df, stim.df

datalist = list(
    hm_trials = length(unique(stim.df$trialid)),
    hm_options = 3,
    hm_attributes = length(unique(ordobs.df$attribute)), #ie. 2
    hm_calcobs = nrow(calcobs.df),
    hm_ordobs = nrow(ordobs.df),
    ord_trialid = ordobs.df$trialid,
    ord_option1=ordobs.df$option1,
    ord_option2=ordobs.df$option2,
    ord_attribute=as.integer(ordobs.df$attribute),
    ord_value=ordobs.df$ord_status,
    ord_noisesd = ordobs.df$noisesd,
    ord_tolerance = ordobs.df$tolerance,
    calc_trialid = calcobs.df$trialid,
    calc_optionid = calcobs.df$optionid,
    calc_noisesd=calcobs.df$noisesd,
    calc_value=calcobs.df$value)
    

fit <- stan(file="obsinput.stan",
            data=datalist,
            iter=1000,
            init=function(){
                zeros <- rep(2000,nrow(stim.df)*3*2) #trials x options x attributes. Need to consider what counts as a good init value! Mean of attributes?
                dim(zeros)=c(nrow(stim.df),3,2)
                list(est_trial_option_attribute=zeros)
            },
            chains=4)


save.image("fit.RData")
mysamples <- as.data.frame(extract(fit, permuted = TRUE))

#launch_shinystan(fit)
#View("done")

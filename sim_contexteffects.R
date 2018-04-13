library(tidyverse)
library(rstan)
library(patchwork)
library(MCMCpack)#supplies rdirichlet
rm(list=ls())
set.seed(4)

##set up
hm_ppnts=20 #ppnts are identical, but provide repetition to check choice-proportions for each stim.
hm_options=3 #will change: 3 then 2 specify locally.
hm_attributes=2; #hand-coded in, up to you to stay consistent.

sim.k <- matrix(1/hm_attributes,ncol=hm_attributes,nrow=hm_ppnts)#,# demo1: even weight on all attributes: all ppnts are identical
#sim.k <- rdirichlet(hm_ppnts,rep(1,hm_attributes)) #demo2, random weights to recover, all ppnts are different.

simexp.df <- read.table(text="trialid,option1attribute1,option1attribute2,option2attribute1,option2attribute2,option3attribute1,option3attribute2
1,0.25,0.75,0.75,0.25,.15,.75
2,0.25,0.75,0.75,0.25,.25,.60
3,0.25,0.75,0.75,0.25,.15,.65
4,0.25,0.75,0.75,0.25,0.5,0.5
5,0.25,0.75,0.75,0.25,0.2,0.8
6,0.25,0.75,0.75,0.25,0.3,0.7",
header=TRUE, sep=",") #'base' stim are {.25,.75} & its reflection, both have value .5 under weights {.5,.5}. '3rd option' decoys: match A worse on B, match B worse on A, worse on both, compromise candidate on equivalue line.

accumulator <- data.frame()
for(i in 1:hm_ppnts){
    simexp.df$ppntid = i;
    accumulator <- rbind(accumulator,simexp.df)
}
simexp.df <- accumulator; rm(accumulator);
    ## data.frame(ppntid=rep(1:hm_ppnts,each=hm_stim),
              ##           trialid=1:hm_stim
              ##           )

#convert to stan-friendly format
ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
dim(ttoa) <- c(nrow(simexp.df),hm_options,hm_attributes)#convert to matrix
for(atrial in 1:nrow(simexp.df)){ #populate with values
    for(i in 1:hm_attributes){
        for(j in 1:hm_options){
            ttoa[atrial,j,i] <- simexp.df[atrial,paste0("option",j,"attribute",i)]
        }
    }
}

for(calcsd_level in seq(from=.01,to=1,length=8)){

    ordsd_level=.25
    
datalist = list(hm_trials=nrow(simexp.df),
                hm_ppnts=hm_ppnts,
                ppntid=simexp.df$ppntid,

                hm_options=3, #run with decoys
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                k=sim.k,
                calcsd_level=calcsd_level,
                ordsd_level=ordsd_level
                )

sim.fit <- stan(file="getchoices.stan",
           data=datalist,
           iter=1000,
           init=function(){
               zeros <- rep(0,nrow(simexp.df)*hm_options*hm_attributes)
               dim(zeros)=c(nrow(simexp.df),hm_options,hm_attributes)
               list(est_trial_option_attribute=zeros)
           },
           chains=4,
           control = list(max_treedepth = 15));

#save(sim.fit,file="context_withdecoys_fit.RData"); #use save image to get everything at once instead

withdecoy.samples <- as.data.frame(extract(sim.fit, permuted = TRUE))# extract returns a list of arrays


##Go around again but with 2 options.
##first, trim the data passed. Probably not be necessary, can't hurt, might expose an error?

## hm_options=2 #new setting
## ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
## dim(ttoa) <- c(nrow(simexp.df),hm_options,hm_attributes)#convert to matrix
## for(atrial in 1:nrow(simexp.df)){ #populate with values
##     for(i in 1:hm_attributes){
##         for(j in 1:hm_options){
##             ttoa[atrial,j,i] <- simexp.df[atrial,paste0("option",j,"attribute",i)]
##         }
##     }
## }

## datalist = list(hm_trials=nrow(simexp.df),
##                 hm_ppnts=hm_ppnts,
##                 ppntid=simexp.df$ppntid,

##                 hm_options=hm_options,
##                 hm_attributes=hm_attributes,

##                 truth_trial_option_attribute = ttoa,
##                 k=sim.k
##                 )

## twooption.fit <- stan(file="getchoices.stan",
##            data=datalist,
##            iter=1000,
##            chains=4,
##            init=function(){
##                zeros <- rep(0,nrow(simexp.df)*hm_options*hm_attributes)
##                dim(zeros)=c(nrow(simexp.df),hm_options,hm_attributes)
##                list(est_trial_option_attribute=zeros)
##            },
##            control = list(max_treedepth = 15));

## #save(twooption.fit,file="context_twooption_fit.RData")

## twooption.samples <- as.data.frame(extract(twooption.fit, permuted = TRUE)) # extract returns a list of arrays


save.image(file=paste0("noisesurvey/calc",calcsd_level,"ord",ordsd_level,"fit.RData"))
}


for(ordsd_level in seq(from=.01,to=1,length=8)){

    calcsd_level=.25
    
datalist = list(hm_trials=nrow(simexp.df),
                hm_ppnts=hm_ppnts,
                ppntid=simexp.df$ppntid,

                hm_options=3, #run with decoys
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                k=sim.k,
                calcsd_level=calcsd_level,
                ordsd_level=ordsd_level
                )

sim.fit <- stan(file="getchoices.stan",
           data=datalist,
           iter=1000,
           init=function(){
               zeros <- rep(0,nrow(simexp.df)*hm_options*hm_attributes)
               dim(zeros)=c(nrow(simexp.df),hm_options,hm_attributes)
               list(est_trial_option_attribute=zeros)
           },
           chains=4,
           control = list(max_treedepth = 15));

#save(sim.fit,file="context_withdecoys_fit.RData"); #use save image to get everything at once instead

withdecoy.samples <- as.data.frame(extract(sim.fit, permuted = TRUE))# extract returns a list of arrays


##Go around again but with 2 options.
##first, trim the data passed. Probably not be necessary, can't hurt, might expose an error?

## hm_options=2 #new setting
## ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
## dim(ttoa) <- c(nrow(simexp.df),hm_options,hm_attributes)#convert to matrix
## for(atrial in 1:nrow(simexp.df)){ #populate with values
##     for(i in 1:hm_attributes){
##         for(j in 1:hm_options){
##             ttoa[atrial,j,i] <- simexp.df[atrial,paste0("option",j,"attribute",i)]
##         }
##     }
## }

## datalist = list(hm_trials=nrow(simexp.df),
##                 hm_ppnts=hm_ppnts,
##                 ppntid=simexp.df$ppntid,

##                 hm_options=hm_options,
##                 hm_attributes=hm_attributes,

##                 truth_trial_option_attribute = ttoa,
##                 k=sim.k
##                 )

## twooption.fit <- stan(file="getchoices.stan",
##            data=datalist,
##            iter=1000,
##            chains=4,
##            init=function(){
##                zeros <- rep(0,nrow(simexp.df)*hm_options*hm_attributes)
##                dim(zeros)=c(nrow(simexp.df),hm_options,hm_attributes)
##                list(est_trial_option_attribute=zeros)
##            },
##            control = list(max_treedepth = 15));

## #save(twooption.fit,file="context_twooption_fit.RData")

## twooption.samples <- as.data.frame(extract(twooption.fit, permuted = TRUE)) # extract returns a list of arrays


save.image(file=paste0("noisesurvey/calc",calcsd_level,"ord",ordsd_level,"fit.RData"))
}
#source("vis_contexteffects.R")
View("done")

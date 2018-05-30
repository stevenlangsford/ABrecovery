library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


load(paste0("training_choices/",list.files("training_choices",pattern="*.RData")[1]))
hm_ppnts = 5
hm_options = 3
hm_attributes = 2 #magic numbers la, better set these in do_survey!


ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
dim(ttoa) <- c(nrow(simexp.df),hm_options,hm_attributes)#convert to matrix
for(atrial in 1:nrow(simexp.df)){ #populate with values
    for(i in 1:hm_attributes){
        for(j in 1:hm_options){
            ttoa[atrial,j,i] <- simexp.df[atrial,paste0("option",j,"attribute",i)]
        }
    }
}

#normal recovery setup:
datalist = list(hm_trials=nrow(simexp.df),
                hm_ppnts=hm_ppnts,
                ppntid=simexp.df$ppntid,

                hm_options=hm_options,
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                calcsd=rep(.1,hm_ppnts),
                ordsd=rep(.1,hm_ppnts),
                tolerance=rep(.1,hm_ppnts),

                choice=simexp.df$choice
                )

initter <- function(){ #inits refer to both new and old data (sorry for the crazy structure...)
               zeros <- rep(0,nrow(og.simexp.df)*hm_options*hm_attributes)
               dim(zeros)=c(nrow(og.simexp.df),hm_options,hm_attributes)

               more.zeros <- rep(0,nrow(simexp.df)*hm_options*hm_attributes)
               dim(more.zeros)=c(nrow(simexp.df),hm_options,hm_attributes)
               
               return(
                   list(est_trial_option_attribute=zeros,
                        test_est_trial_option_attribute=more.zeros
                        )
               )
           }

##prediction test data setup
og.simexp.df <- simexp.df #refd by initter, which now refs this og version and then the new test version too. UGH! Sorry.
simexp.df <- read.csv("teststim.csv")
##still need to take the stim list and turn it into something like an experiment eg attach ppnt ids.
accumulator.df <- data.frame()
for(i in 1:hm_ppnts){
        simexp.df$ppntid=i
        accumulator.df <- rbind(accumulator.df,simexp.df)
}
simexp.df <- accumulator.df;

test_ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
dim(test_ttoa) <- c(nrow(simexp.df),hm_options,hm_attributes)#convert to matrix
for(atrial in 1:nrow(simexp.df)){ #populate with values
    for(i in 1:hm_attributes){
        for(j in 1:hm_options){
            test_ttoa[atrial,j,i] <- simexp.df[atrial,paste0("option",j,"attribute",i)]
        }
    }
}

datalist$test_hm_trials=nrow(simexp.df)
datalist$test_hm_ppnts=hm_ppnts
datalist$test_ppntid=simexp.df$ppntid
datalist$test_truth_trial_option_attribute = test_ttoa
datalist$test_calcsd=rep(.1,hm_ppnts)
datalist$test_ordsd=rep(.1,hm_ppnts)
datalist$test_tolerance=rep(.1,hm_ppnts)


recovery.fit <- stan(file="howes16_solo.stan",#file="baseline_plus.stan",
           data=datalist,
           iter=1000,
           chains=4,
           init=initter
           #control = list(max_treedepth = 15,adapt_delta = 0.9)
           );

recovery.samples <- as.data.frame(extract(recovery.fit, permuted = TRUE)) # extract returns a list of arrays
save.image(file="howes16solo.RData")#"blplus_recovery.RData")
View("done")

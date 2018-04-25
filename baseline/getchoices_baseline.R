library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#Assumes you already *have* choices from howes16, & you want to match them ie run the same stimuli.
load("calc0.2ord0.2tolerance0.1modelgetchoices.stanfit.RData")
#simexp.df$simchoice <-   as.numeric(mysamples%>%dplyr::select(contains("choice"))%>%sample_n(1)%>%t)#delete

ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
dim(ttoa) <- c(nrow(simexp.df),hm_options,hm_attributes)#convert to matrix
for(atrial in 1:nrow(simexp.df)){ #populate with values
    for(i in 1:hm_attributes){
        for(j in 1:hm_options){
            ttoa[atrial,j,i] <- simexp.df[atrial,paste0("option",j,"attribute",i)]
        }
    }
}

datalist = list(hm_trials=nrow(simexp.df),
                hm_ppnts=hm_ppnts,
                ppntid=simexp.df$ppntid,

                hm_options=hm_options,
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                calcsd=.2,
                choice=simexp.df$choice                
                )

recovery.fit <- stan(file="baseline_regression_choicegen.stan",
           data=datalist,
           iter=1000,
           chains=4,
           init=function(){
               zeros <- rep(0,nrow(simexp.df)*hm_options*hm_attributes)
               dim(zeros)=c(nrow(simexp.df),hm_options,hm_attributes)
               return(
                   list(est_trial_option_attribute=zeros#,
                        ## calcsd=rep(.1,hm_ppnts), #might need to revisit these inits?
                        ## ordsd=rep(.1,hm_ppnts),
                        ## tolerance=rep(.1,hm_ppnts)
                        )
               )
           },
           #control = list(max_treedepth = 15,adapt_delta = 0.9)
           );

choicegen.samples <- as.data.frame(extract(recovery.fit, permuted = TRUE)) # extract returns a list of arrays
save.image(file="baseline_choicegen.RData")
View("done making choices")

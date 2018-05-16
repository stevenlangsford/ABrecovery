library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

targfolder="range_of_k_stim/" ; hm_options=3
afile = list.files(targfolder,pattern="calc.*RData")[1]
load(paste0(targfolder,afile))

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
                hm_ppnts=length(unique(simexp.df$ppntid)),#must be consecutive starting with 1
                ppntid=simexp.df$ppntid,

                hm_options=hm_options,
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                calcsd=rep(.1,length(unique(simexp.df$ppntid))), #placeholders, happen to match sim values...
                ordsd=rep(.1,length(unique(simexp.df$ppntid))),
                tolerance=rep(.1,length(unique(simexp.df$ppntid))),
                
                choice=simexp.df$choice
                )

recovery.fit <- stan(file="baseline_plus.stan",
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

recovery.samples <- as.data.frame(extract(recovery.fit, permuted = TRUE)) # extract returns a list of arrays
save.image(file="range_of_k_stim/baseline_plus_recovery.RData")


####################################################################################################
rm(list=ls())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

targfolder="range_of_k_stim/" ; hm_options=3
afile = list.files(targfolder,pattern="calc.*RData")[1]
load(paste0(targfolder,afile))

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
                hm_ppnts=length(unique(simexp.df$ppntid)),#must be consecutive starting with 1
                ppntid=simexp.df$ppntid,

                hm_options=hm_options,
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                ## calcsd=rep(.1,length(unique(simexp.df$ppntid))), #baseline doesn't care about these
                ## ordsd=rep(.1,length(unique(simexp.df$ppntid))),
                ## tolerance=rep(.1,length(unique(simexp.df$ppntid))),
                
                choice=simexp.df$choice
                )

recovery.fit <- stan(file="baseline_regression.stan",
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

recovery.samples <- as.data.frame(extract(recovery.fit, permuted = TRUE)) # extract returns a list of arrays
save.image(file="range_of_k_stim/baseline_regression_recovery.RData")
View("done baseline_recovery")

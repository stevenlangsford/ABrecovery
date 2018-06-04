library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


recover_k <- function(targdata,modelstring){
load(targdata) #or whatever it is
##simexp.df$simchoice <-   as.numeric(mysamples%>%dplyr::select(contains("choice"))%>%sample_n(1)%>%t)#delete

hm_options = 3; #Should not have to set this here. Don't forget it's there!
hm_ppnts = length(unique(simexp.df$ppntid));

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

                choice=simexp.df$choice,

                calcsd = rep(.15,hm_ppnts),
                ordsd = rep(.15,hm_ppnts),
                tolerance= rep(.1,hm_ppnts)

                )

recovery.fit <- stan(file=paste0(modelstring,".stan"),#"baseline_regression.stan",#file="agentRecovery_konly.stan",
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
           control = list(max_treedepth = 13,adapt_delta = 0.8) # feasible? 10 and .8 defaults are too low, fifteen and .9 too high
           );

#recovery.samples <- as.data.frame(extract(recovery.fit, permuted = TRUE)) # extract returns a list of arrays
save(recovery.fit,file=paste0(modelstring,".RData"))#file="baseline_recovery.RData")#file="recoveryfit.RData")
#View("done")
}#ends recover k


targdata="calc0.15ord0.15tolerance0.1modelgetchoices.stanfit.RData"
##recover_k(targdata,"baseline")
##recover_k(targdata,"hybrid")
##recover_k(targdata,"agentRecovery_konly")
##recover_k(targdata,"agentRecovery_sans_extremify") #Nope, it's not about the extremification.
##recover_k(targdata,"agentRecovery_halford") # makes life really hard for the sampler... why? It's a constant factor. (probably) drops k but (definitely) v. flat.
#recover_k(targdata,"agentRecovery_sansord")

#todo: hybrid_sansord, hybrid_sanscalc?  hybrid_halford
#recover_k(targdata,"hybrid_sansord")
recover_k(targdata,"hybrid_sanscalc")

library(tidyverse)
library(rstan)
library(patchwork)
rm(list=ls())
set.seed(4)

##set up tiny data
hm_stim=2
hm_ppnts=2
hm_options=3
hm_attributes=2;

sim.k <- matrix(1/hm_attributes,ncol=hm_attributes,nrow=hm_ppnts)#,# demo1: even weight on all attributes

simexp.df <- data.frame(ppntid=rep(1:hm_ppnts,each=hm_stim),
                        trialid=1:hm_stim,
                        choice=base::sample(1:hm_options,hm_stim*hm_ppnts,replace=TRUE)
                        )

for(i in 1:hm_attributes){
    for(j in 1:hm_options){
        simexp.df[,paste0("option",j,"attribute",i)] <- rnorm(nrow(simexp.df))
    }
}

ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
dim(ttoa) <- c(nrow(simexp.df),3,2)#convert to matrix
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

                k=sim.k
                
#                choice=simexp.df$choice               
                )

sim.fit <- stan(file="getchoices.stan",
           data=datalist,
           iter=1000,
           chains=4,
           control = list(max_treedepth = 15));

save(sim.fit,file="choicegen_fit.RData");

simexp.samples <- as.data.frame(extract(sim.fit, permuted = TRUE)) # extract returns a list of arrays

#add generated choices
for(i in 1:nrow(simexp.df)){
    simexp.df[i,"choice"] <- sample_n(simexp.samples,1)%>%select(paste0("generated_choice.",i))%>%as.numeric
}

##recovery from simulation!
datalist = list(hm_trials=nrow(simexp.df),
                hm_ppnts=hm_ppnts,
                ppntid=simexp.df$ppntid,

                hm_options=hm_options,
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                choice=simexp.df$choice
                )

recovery.fit <- stan(file="agentRecovery.stan",
           data=datalist,
           iter=1000,
           chains=4,
           control = list(max_treedepth = 15));

save(recovery.fit,file="recovery_fit.RData");

recovery.samples <- as.data.frame(extract(recovery.fit, permuted = TRUE)) # extract returns a list of arrays

View("done")

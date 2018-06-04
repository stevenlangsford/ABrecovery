library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

rm(list=ls())
load("calc0.15ord0.15tolerance0.1modelgetchoices.stanfit.RData")
rm(list=setdiff(ls(),c("simexp.df","sim.k")))
                                        #load("regression_recovery.RData")
hm_attributes=2 #ARGH should not be setting this here.


check_recovery=function(targdata){
load(targdata)
hm_ppnts=length(unique(simexp.df$ppntid)) #WHY IS THIS GETTING RESET SO BAD NOOOO

recovery.samples <- as.data.frame(extract(recovery.fit, permuted = TRUE))
k.samples <- recovery.samples%>%select(contains("k."))

mean.k <- apply(k.samples,2,mean)

recoverycheck.df <- as.data.frame(sim.k)
for(i in 1:hm_ppnts){
    for(j in 1:hm_attributes){
        recoverycheck.df[i,j+hm_attributes] <- mean.k[paste0("k.",i,".",j)]
    }
}
names(recoverycheck.df) <- c(paste0("sim.",1:hm_attributes),paste0("recovered.",1:hm_attributes))

return (recoverycheck.df)
#return (ggplot(recoverycheck.df,aes(x=sim.1,y=recovered.1))+geom_point()+theme_bw())
}

##print(check_recovery("range_of_k_stim/baseline_plus_recovery.RData")+check_recovery("range_of_k_stim/baseline_regression_recovery.RData"))
compare.df <- rbind(
#    check_recovery("agentRecovery_konly.RData")%>%mutate(model="howes16"),
    check_recovery("baseline.RData")%>%mutate(model="baseline"),
    check_recovery("hybrid.RData")%>%mutate(model="hybrid"),
    check_recovery("agentRecovery_sans_extremify.RData")%>%mutate(model="sansextreme"), #nope doesn't make much difference.
    check_recovery("agentRecovery_halford.RData")%>%mutate(model="full_halford"),   #k ests seem to drop a bit, but still flat, sampler screams in agony, why?
    check_recovery("agentRecovery_sansord.RData")%>%mutate(model="full_sansord"),
    check_recovery("hybrid_sansord.RData")%>%mutate(model="h_sansord"),   
    check_recovery("hybrid_sanscalc.RData")%>%mutate(model="h_sanscalc")
)

ggplot(compare.df,aes(x=sim.1,y=recovered.1,color=model))+
    geom_line(data=data.frame(x=seq(from=0,to=1,length=100)),aes(x=x,y=x),color="black")+ #identity line
    geom_point(aes(shape=model))+geom_line()+theme_bw()

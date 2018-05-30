library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)
                                        #load("regression_recovery.RData")
check_recovery=function(targdata){
load(targdata)
hm_ppnts=length(unique(simexp.df$ppntid)) #WHY IS THIS GETTING RESET SO BAD NOOOO

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
compare.df <- rbind(check_recovery("range_of_k_stim/baseline_plus_recovery.RData")%>%mutate(model="with_calcord"),
      check_recovery("range_of_k_stim/baseline_regression_recovery.RData")%>%mutate(model="baseline"))

ggplot(compare.df,aes(x=sim.1,y=recovered.1,color=model))+
    geom_line(data=data.frame(x=seq(from=0,to=1,length=100)),aes(x=x,y=x),color="black")+ #identity line
    geom_point()+geom_line()+theme_bw()

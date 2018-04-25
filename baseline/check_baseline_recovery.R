library(tidyverse)
library(rstan)
library(shinystan)

##load("regression_recovery.RData")

k.samples <- recovery.samples%>%select(contains("k."))

mean.k <- apply(k.samples,2,mean)

recoverycheck.df <- as.data.frame(sim.k)
for(i in 1:hm_ppnts){
    for(j in 1:hm_attributes){
        recoverycheck.df[i,j+hm_attributes] <- mean.k[paste0("k.",i,".",j)]
    }
}
names(recoverycheck.df) <- c(paste0("sim.",1:hm_attributes),paste0("recovered.",1:hm_attributes))

ggplot(recoverycheck.df,aes(x=sim.1,y=recovered.1))+geom_point()+theme_bw()

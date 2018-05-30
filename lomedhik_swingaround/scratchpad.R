library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())

#load("agentRecovery_konly.RData")
#howes16samples <- as.data.frame(extract(recovery.fit, permuted = TRUE))
load("baseline.RData")
baselinesamples <- as.data.frame(extract(recovery.fit, permuted = TRUE))
load("hybrid.RData")
hybridsamples <- as.data.frame(extract(recovery.fit, permuted = TRUE))

load("calc0.15ord0.15tolerance0.1modelgetchoices.stanfit.RData")#targdata, contains simexp.df
original.sim.k <- sim.k
rm(list=setdiff(ls(),c("howes16samples","baselinesamples","hybridsamples","simexp.df","original.sim.k")))

## howes.k <- howes16samples%>%select(starts_with("k"))%>%select(1:3)%>%gather(whichppnt,kest,k.1.1:k.3.1)%>%mutate(model="howes")
## baseline.k <- baselinesamples%>%select(starts_with("k"))%>%select(1:3)%>%gather(whichppnt,kest,k.1.1:k.3.1)%>%mutate(model="baseline")
## hybrid.k <- hybridsamples%>%select(starts_with("k"))%>%select(1:3)%>%gather(whichppnt,kest,k.1.1:k.3.1)%>%mutate(model="hybrid")

## combo.k <- rbind(howes.k,baseline.k,hybrid.k)
## ggplot(combo.k,aes(x=whichppnt,y=kest,color=model))+geom_jitter()+theme_bw() #oh dear. This doesn't look very good. Sad.

#howes.k <- howes16samples%>%select(starts_with("k"))%>%select(1:3)%>%apply(2,mean)
baseline.k <- baselinesamples%>%select(starts_with("k"))%>%select(1:3)%>%apply(2,mean)
hybrid.k <- hybridsamples%>%select(starts_with("k"))%>%select(1:3)%>%apply(2,mean)

#howes16.recovered = matrix(c(howes.k, 1-howes.k),ncol=2,nrow=length(unique(simexp.df$ppntid)))
baseline.recovered = matrix(c(baseline.k, 1-baseline.k),ncol=2,nrow=length(unique(simexp.df$ppntid)))
hybrid.recovered =  matrix(c(hybrid.k, 1-hybrid.k),ncol=2,nrow=length(unique(simexp.df$ppntid)))

source("sim_dosurvey.R") #loads do_a_survey function, expects to find simexp.df in the environment (with no ppnt details attached)

simexp.df <- simexp.df%>%filter(ppntid==1)%>%select(-ppntid,-choice) #pre do-a-survey format. Man, why does this function look in the global env at all? ugh.

##do the thing with the recovered mean sim k
## sim.k <- baseline.recovered
## do_a_survey(
##     simexp.df, sim.k,
##     calcsd_levels=c(.15),
##     ordsd_levels=c(.15),
##     tolerance_levels=c(.1),
##     model_names=c("baseline_choicegen.stan"),
##     targfolder="baseline_predict_from_meank",
##     hm_options=3,
##     hm_ppnts=nrow(sim.k)
## )

## sim.k <- howes16.recovered
## do_a_survey(
##     simexp.df, sim.k,
##     calcsd_levels=c(.15),
##     ordsd_levels=c(.15),
##     tolerance_levels=c(.1),
##     model_names=c("getchoices.stan"),
##     targfolder="howes16_predict_from_meank",
##     hm_options=3,
##     hm_ppnts=nrow(sim.k)
## )

## sim.k <- hybrid.recovered
## do_a_survey(
##     simexp.df, sim.k,
##     calcsd_levels=c(.15),
##     ordsd_levels=c(.15),
##     tolerance_levels=c(.1),
##     model_names=c("hybrid_getchoices.stan"),
##     targfolder="hybrid_predict_from_meank",
##     hm_options=3,
##     hm_ppnts=nrow(sim.k)
## )


##do the thing with the original sim.k
sim.k <- original.sim.k

## do_a_survey(
##     simexp.df, sim.k,
##     calcsd_levels=c(.15),
##     ordsd_levels=c(.15),
##     tolerance_levels=c(.1),
##     model_names=c("baseline_choicegen.stan"),
##     targfolder="baseline_predict_from_originalk",
##     hm_options=3,
##     hm_ppnts=nrow(sim.k)
## )



## do_a_survey(
##     simexp.df, sim.k,
##     calcsd_levels=c(.15),
##     ordsd_levels=c(.15),
##     tolerance_levels=c(.1),
##     model_names=c("getchoices.stan"),
##     targfolder="howes16_predict_from_originalk",
##     hm_options=3,
##     hm_ppnts=nrow(sim.k)
## )


do_a_survey(
    simexp.df, sim.k,
    calcsd_levels=c(.15),
    ordsd_levels=c(.15),
    tolerance_levels=c(.1),
    model_names=c("hybrid_getchoices.stan"),
    targfolder="hybrid_predict_from_originalk",
    hm_options=3,
    hm_ppnts=nrow(sim.k)
)

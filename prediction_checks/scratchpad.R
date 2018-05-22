library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())


load("blplus_recovery.RData")
blplus.samples <- recovery.samples;
rm(list=setdiff(ls(),"blplus.samples"))

load("regression_recovery.RData")
regression.samples <- recovery.samples; rm(recovery.samples)
rm(list=setdiff(ls(),c("blplus.samples","regression.samples", "simexp.df")))

load("test_choices/calc0.15ord0.15tolerance0.1modelgetchoices.stanfit.RData")
sim.samples <- mysamples
rm(list=setdiff(ls(),c("blplus.samples","regression.samples", "sim.samples","simexp.df"))) #note simexp.df is from sim run, has a "sim choice" col.

plus.preds <- blplus.samples%>%select(starts_with("predicted"))
regression.preds <- regression.samples%>%select(starts_with("predicted"))
sim.outcomes <- sim.samples%>%select(starts_with("generated_choice"))
modellist = list("plus"=plus.preds,"baseline"=regression.preds,"sim"=sim.outcomes)

##****************************************************************************************************
mode.so <- function(x) { #credit stack overflow
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

compare.df=data.frame()
for(astim in unique(simexp.df$trialid)){
    for(appnt in unique(simexp.df$ppntid)){
        targ.trial = which(simexp.df$trialid==astim&simexp.df$ppntid==appnt)
        for(achoice in 1:3){
            for(amodel in 1:length(modellist)){
                success.status = c(mode.so(modellist[["plus"]][,targ.trial])==mode.so(modellist[["sim"]][,targ.trial]),
                                   mode.so(modellist[["baseline"]][,targ.trial])==mode.so(modellist[["sim"]][,targ.trial]))
                if(sum(success.status)==0)success.status.flag="neither"
                if(sum(success.status)==2)success.status.flag="both"
                if(sum(success.status)==1)success.status.flag=ifelse(success.status[1]==1,"plus","baseline")#there has got to be a better way than this :-(
                
                
                compare.df <- rbind(compare.df,data.frame(
                                                   choice=achoice,
                                                   count=sum(modellist[[amodel]][,targ.trial]==achoice),
                                                   modeltype=names(modellist)[amodel],
                                                   ppntid=appnt,
                                                   trialid=astim,
                                                   sim.gap=sum(modellist[[amodel]][,targ.trial]==achoice)-sum(modellist[["sim"]][,targ.trial]==achoice),
                                                   trialindex=targ.trial,
                                                   successstatus=success.status.flag
                                                       
                                               )
                                    )
            }
        }
    }
}

##Ok here are the raw results...
ggplot(compare.df,aes(x=choice,y=count,group=modeltype,fill=modeltype))+
    geom_rect(data = compare.df,aes(fill = successstatus),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1)+
    geom_bar(stat="identity",position="dodge")+facet_grid(ppntid~trialid)+theme_bw()+scale_fill_brewer(palette="Spectral")
x11();
##Here's the "difference from sim", ideally close to zero. A little weird because it's possible for large gaps to appear when the max choice is in the correct spot, the sim is generally quite extreme in its preferences, the recovery conservative/confused.
ggplot(filter(compare.df,modeltype!="sim"),aes(x=choice,y=sim.gap,group=modeltype,fill=modeltype))+
    geom_rect(data = compare.df,aes(fill = successstatus),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1)+
    geom_bar(stat="identity",position="dodge")+facet_grid(ppntid~trialid)+theme_bw()+scale_fill_brewer(palette="Spectral")



##possibly also color backgrounds by stim-ppnt matchiness?


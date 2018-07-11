library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())

#saveplots=FALSE;

load("simk.RData")
choicecheck.df <- data.frame()

for(arecovery in list.files("recovery/",pattern=".*RData")){
    load(paste0("recovery/",arecovery))
    mysamples <- as.data.frame(extract(recovery.fit[[1]], permuted=TRUE))
    mydata.df <- recovery.fit[[2]]
    
for(arow in 1:nrow(mydata.df)){
    choicecheck.df <- rbind(choicecheck.df,data.frame(
                                               choicematches=sum(mysamples[,paste0("generated_choice.",arow)]==mydata.df[arow,"choice"]),
                                               trialtype=mydata.df[arow,"trialtype"],
                                               modeltype= substr(arecovery,1,6),
                                               hm.attractionstim = sum(mydata.df$trialtype=="attraction")/nrow(sim.k)
                                           )
                            )
}

} #end for arecovery

ccsummary.df <- choicecheck.df%>%group_by(hm.attractionstim,modeltype,trialtype)%>%summarize(meanmatches=mean(choicematches))
ggplot(ccsummary.df,aes(x=hm.attractionstim,y=meanmatches,color=modeltype))+geom_point()+theme_bw()+facet_wrap(.~trialtype)

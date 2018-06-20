library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

rm(list=ls())
targdata= "fit.RData";
dataname = strsplit(targdata,".RData")[1]
load(targdata)

mysamples <- as.data.frame(extract(fit, permuted = TRUE))

trialplot <- function(trialid){ #assumes mysamples and stim.df visible.
    opt1.df <- data.frame(x=mysamples[,paste0("est_trial_option_attribute.",trialid,".1.1")],y=mysamples[,paste0("est_trial_option_attribute.",trialid,".1.2")])
    opt2.df <- data.frame(x=mysamples[,paste0("est_trial_option_attribute.",trialid,".2.1")],y=mysamples[,paste0("est_trial_option_attribute.",trialid,".2.2")])
    opt3.df <- data.frame(x=mysamples[,paste0("est_trial_option_attribute.",trialid,".3.1")],y=mysamples[,paste0("est_trial_option_attribute.",trialid,".3.2")])

    cloudalpha=.2
    attribute.plot <- ggplot(data=data.frame(),aes(x=x,y=y))+
        geom_point(data=opt1.df,aes(color="1"),alpha=cloudalpha) +
        geom_point(data=opt2.df,aes(color="2"),alpha=cloudalpha)+
        geom_point(data=opt3.df,aes(color="3"),alpha=cloudalpha)+
        geom_point(data=stim.df[trialid,], aes(x=width1,y=height1,fill="1",shape="1"),color="black",pch=21,size=5)+
        geom_point(data=stim.df[trialid,], aes(x=width2,y=height2,fill="2",shape="2"),color="black",pch=21,size=5)+
        geom_point(data=stim.df[trialid,], aes(x=width3,y=height3,fill="3",shape="3"),color="black",pch=21,size=5)+
        theme_bw()+xlim(c(0,500))+ylim(c(0,500))+
        ggtitle(paste("Trial",trialid,paste0("area",1:3,"=",signif(stim.df[trialid,c("area1","area2","area3")],3)," ",collapse="")))
    

    choice.plot <- ggplot(mysamples%>%select(starts_with(paste0("generated_choice.",trialid))),aes_string(x=paste0("generated_choice.",trialid)))+
        geom_bar(stat="count",aes_string(fill=paste0("generated_choice.",trialid)))+
        theme_bw()

    return(attribute.plot/choice.plot)

    
}#end trialplot

for(i in unique(stim.df$trialid)){
     ggsave(trialplot(i),file=paste0("plots/",dataname,"_trial",i,".png"))
}


##NEXT:
##The current stimuli are not good tests. You want to replicate howes16 (again), possibly do the vary-noise survey, then shift to your proposed triangle stimuli without similarity structure, then triangle stimuli with a guess at how similarity might push around ordobs. Maybe a range of guesses, like is vary-noise very different from vary-tolerance? What do you sincerely reckon is plausible?
##What aspect of this is going to make adherents of alternative accounts sit up and take notice? What's the MLBA story with comparisons-are-basic-input (for example)
##Also, don't forget you've got placeholder TODO's hanging over your head re priors (that lower bound limit) and inits.

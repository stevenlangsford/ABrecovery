library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

rm(list=ls())

survey.df <- data.frame()

    
for(targdata in list.files(pattern=paste0("demofit0*"))){

    dataname = strsplit(targdata,".RData")[1]
    print(dataname)
    load(targdata)

    for(trialid in 1:nrow(rawstim.df)){
        print(trialid)
        mysamples <- as.data.frame(extract(fit, permuted = TRUE))

        choices <- mysamples%>%select(matches(paste0("generated_choice.",trialid,"$")))
        ones <- sum(choices==1)/nrow(choices)
        twos <- sum(choices==2)/nrow(choices)
        threes <- sum(choices==3)/nrow(choices)
        survey.df <- rbind(survey.df, data.frame(trial=trialid, noise=basenoise, tolerance=tolerance, ones=ones,twos=twos,threes=threes,choicecolor= rgb(red=ones,green=twos, blue=threes), stimid=as.character(rawstim.df[trialid,"stimid"])))
    }
}

for(atol in unique(survey.df$tolerance)){
    print(atol)
ggsave(
    ggplot(survey.df,aes(x=noise,y=stimid))+geom_point(size=5,aes(color=ones/(ones+twos)))+
    theme_bw()+labs(color="'target' choice proportion")+ggtitle(paste("tolerance",atol)),
    file=paste0("noiserangesurvey",atol,".png"),
    width=15,height=15
    )
}
## ggplot(survey.df,aes(x=noise,y=stimid))+geom_point(size=5,color=survey.df$choicecolor)+
##     theme_bw()

## ggplot(survey.df,aes(y=stimid))+
##     geom_point(size=5,aes(x=noise,color=threes,shape="decoy"))+
##     geom_point(size=5,aes(x=noise+.0007,color=twos,shape="comp"))+
##         geom_point(size=5,aes(x=noise+.0014,color=ones,shape="targ"))+
##     theme_bw()

## ggplot(survey.df,aes(x=noise,y=stimid))+geom_point(size=5,color=survey.df$choicecolor)+
##     theme_bw()


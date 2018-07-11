library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())
saveplots=FALSE

postpred.summary.df <- data.frame();
load("simk.RData")
for(targdata in list.files("recovery/",pattern=".*RData")){
    dataname=substr(targdata,1,nchar(targdata)-6)
    load(paste0("recovery/",targdata))
    
mysamples <- as.data.frame(extract(recovery.fit[[1]], permuted=TRUE))
mydata.df <-  recovery.fit[[2]]

genchoice.df <- data.frame();

for(atrial in 1:nrow(mydata.df)){
    genchoice.df <- rbind(genchoice.df,
                          data.frame(
                              dataname=dataname,
                              trialid = atrial,
                              matches = sum(mysamples[,paste0("generated_choice.",atrial)]==mydata.df[atrial,"choice"]),
                              trialtype=mydata.df[atrial,"trialtype"],
                              ppntid=mydata.df[atrial,"ppntid"]
                          )
                          )
}
    postpred.summary.df <- rbind(postpred.summary.df,genchoice.df)
    
    if(saveplots){
        ggsave(
        ggplot(genchoice.df,aes(x=trialid,y=matches,fill=trialtype))+
        geom_bar(stat="identity")+
        facet_wrap(~ppntid,scales="free")+
        theme_bw()+ggtitle(targdata),
           file=paste0("plots/postpredbars/",dataname,".png")
        )
        }
}#end for each targdata

postpred.summary.df$modelid <- substr(postpred.summary.df$dataname,1,6)

bob <- postpred.summary.df%>%group_by(dataname,trialtype,modelid)%>%summarize(n=n()/nrow(sim.k),meanmatches=mean(matches))%>%ungroup()
ggplot(bob,aes(x=trialtype,y=meanmatches,fill=modelid))+geom_boxplot()+theme_bw() #whut?

library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())

##load("recovery/baselinerecovery.RData")
resultlist <- list()

for(afit in list.files("recovery/",pattern=".*RData")){
load(paste0("recovery/",afit))
fitname <- substr(afit,1,nchar(afit)-6)
recovery.data <- recovery.fit[[2]]
recovery.fit <- recovery.fit[[1]]

mysamples <- as.data.frame(extract(recovery.fit, permuted=TRUE))
recovered_k <- mysamples%>%select(starts_with("k"))%>%select(ends_with("1"))
resultlist[[fitname]] <- recovered_k
}

load("simk.RData")
for(aresult in names(resultlist)){
plotbase =  ggplot(resultlist[[aresult]])+theme_bw() #plot however many ppnts you have, as determined by rows of sim.k
for(ppnt in 1:nrow(sim.k)){
    plotbase <- plotbase+geom_violin(aes_string(x=sim.k[ppnt,1],y=paste0("k.",ppnt,".1")),alpha=.5)
    
    plotbase <- plotbase+geom_point(data=data.frame(original.k=sim.k[ppnt,1],recovered.k=mean(mysamples[,paste0("k.",ppnt,".1")])),aes(x=original.k,y=recovered.k))
}

plotbase <- plotbase+geom_abline(intercept=0,slope=1)+ylim(c(0,1))#? not sure about this abline. On probation...

ggsave(plotbase, file=paste0("plots/",aresult,"_violin.png"))
}

## for(aresult in names(resultlist)){
##      ggsave(
##          ggplot(resultlist[[aresult]])+
##          geom_violin(aes(x=.1,y=k.1.1,fill="0.1"))+
##          geom_violin(aes(x=.5,y=k.2.1,fill="0.5"))+
##          geom_violin(aes(x=.9,y=k.3.1,fill="0.9"))+
##          theme_bw()+ggtitle(aresult)+ylim(c(0,1)),
##          file=paste0("plots/",aresult,".png")
##      )
## }

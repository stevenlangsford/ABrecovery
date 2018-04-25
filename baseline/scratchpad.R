library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)
rm(list=ls())
#compare the recovery performance of howes and baseline.
load("howes_recovery.RData")
howes.k.samples <- recovery.samples%>%select(contains("k."))
howes.mean.k <- apply(howes.k.samples,2,mean)
howes.datalist <- datalist
rm(list=setdiff(ls(),c(#"howes.k.samples",
                         "howes.mean.k","howes.datalist")))

load("regression_recovery.RData")
baseline.k.samples <- recovery.samples%>%select(contains("k."))
baseline.mean.k <- apply(baseline.k.samples,2,mean)
baseline.datalist <- datalist

rm(list=setdiff(ls(),c(#"baseline.k.samples",
                         "baseline.mean.k","baseline.datalist",
                         "howes.mean.k","howes.datalist")))

recovery.vis.df <- data.frame(as.data.frame(t(as.data.frame(t(howes.mean.k))%>%select(ends_with("1")))),
                 as.data.frame(t(as.data.frame(t(baseline.mean.k))%>%select(ends_with("1")))),
                 sim.k[,1]
                 )
names(recovery.vis.df) <- c("howes","baseline","simtruth")

recovery.comparison.plot <- 
ggplot(recovery.vis.df,aes(x=simtruth))+geom_point(aes(y=howes),color="blue")+theme_bw()+ggtitle("Howes16")+
ggplot(recovery.vis.df,aes(x=simtruth))+geom_point(aes(y=baseline),color="red")+theme_bw()+ggtitle("Baseline")

ggsave(recovery.comparison.plot,file="recoverycomparisonplot.png",width=15)
print(recovery.comparison.plot)



##Pull up differences in choice patterns for howes vs baseline, trial-by-trial (over range passed as first arg to lapply)
## load("baseline_choicegen.RData")
## baseline <- choicegen.samples%>%select(contains("choice"))
## howes <- mysamples%>%select(contains("choice"))
## compare.df <- reduce(
##     lapply(1:10,function(i){as.data.frame(table(baseline[,i]))%>%mutate(mydata="baseline",trial=i)%>%
##     bind_rows(
##         as.data.frame(table(howes[,i]))%>%mutate(mydata="howes",trial=i)
##     )
## }), bind_rows)
## ggplot(compare.df,aes(x=Var1,y=Freq,fill=mydata))+geom_bar(stat="identity",position="dodge")+facet_wrap(~trial)+theme_bw()

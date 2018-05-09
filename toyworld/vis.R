library(tidyverse)
library(shinystan)
rm(list=ls())
load("test.RData")

data.df$true_ord = with(data.df, ifelse(abs(x1-x2)<tolerance, 2, ifelse(x1<x2,1,3)))


for(targpair in 1:nrow(data.df)){
    sansord="sans"
    withord="with"
    x11(); print(ggplot()+ geom_point(data=target.samples,aes_string(x=paste0("x1_est.",targpair),y=paste0("x2_est.",targpair),color="withord"),alpha=.2)+
    geom_point(data=sans.samples,aes_string(x=paste0("x1_est.",targpair),y=paste0("x2_est.",targpair),color="sansord"),alpha=.2)+
    theme_bw()+ggtitle(paste("relation:",c("x1 <","=","x1 >")[data.df[targpair,"true_ord"]]))
    )
}
#ggplot(withord.samples)+geom_point(aes(x=x1.1,y=x2.1))+theme_bw()

#ggplot(sans.samples)+geom_point(aes(x=x1.1,y=x2.1,color=x1.1>x2.1))+theme_bw()

print(data.df)

library(tidyverse)
library(shinystan)
library(patchwork)
rm(list=ls())
load("test.RData")

data.df$true_ord = with(data.df, ifelse(abs(x1-x2)<tolerance, 2, ifelse(x1<x2,1,3)))

for(targpair in 1:nrow(data.df)){
    sansord="sans"
    withord="with"

    identity.df=data.frame(x=seq(from=-2,to=2,length=100))
                           
    samples.plot <-
        ggplot()+
        geom_point(data=sans.samples,aes_string(x=paste0("x1_est.",targpair),y=paste0("x2_est.",targpair),color="sansord"),alpha=.2)+
        geom_point(data=target.samples,aes_string(x=paste0("x1_est.",targpair),y=paste0("x2_est.",targpair),color="withord"),alpha=.2)+
        theme_bw()+ggtitle(paste("relation:",c("x1 <","=","x1 >")[data.df[targpair,"true_ord"]]))+
        geom_line(data=identity.df,aes(x=x,y=x+tolerance),color="black")+
        geom_line(data=identity.df,aes(x=x,y=x-tolerance),color="black")+
    geom_line(data=identity.df,aes(x=x,y=with(data.df[targpair,],x1+x2-x)),color="yellow")+
            geom_point(data=data.df[targpair,],aes(x=x1,y=x2,color="Truth"),size=5)+
        xlim(c(-2.3,2.3))+ylim(c(-2.3,2.3))
        
        
        my_ordobs <- target.samples%>%select(starts_with(paste0("ordprob_tracker.",targpair,".")))
    names(my_ordobs) <- c("lessthan","equal","greaterthan")
    my_ordobs <-  data.frame(status=factor(names(my_ordobs),ordered=TRUE,levels=c("lessthan","equal","greaterthan")),sample.mean=apply(my_ordobs,2,mean))
    
    ordobs.plot <- ggplot(my_ordobs,aes(x=status,y=sample.mean))+geom_bar(stat="identity")+theme_bw()
    
    ggsave(samples.plot,file=paste("demo",targpair,".png"))
#    print(samples.plot/ordobs.plot)
 
}
print(data.df)

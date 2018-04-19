library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)
#rm(list=ls())
#afile=list.files(pattern="^calc.*RData")
#load(afile)
#targfolder=paste0(strsplit(afile,".RData")[1],"plots")
#dir.create(targfolder);


fixedcolors <- c("red","blue","green")#works with scale_fill_maunual
colorgetter <- function(id){ #works with independent geoms
    if(id==1) return("red")
    if(id==2) return("blue")
    if(id==3) return("green")
    stop(paste("bad id in colorgetter ",id))
}


stim.plot <- function(trialnumber){
    estcloud.df <- data.frame()
    for(anoption in 1:3){
        x <- mysamples[,paste("est_trial_option_attribute",trialnumber,anoption,1,sep=".")]
        y <- mysamples[,paste("est_trial_option_attribute",trialnumber,anoption,2,sep=".")]
        est.value <-     mysamples[,paste("estval_tracker_raw",trialnumber,anoption,sep=".")]
        estcloud.df <- rbind(estcloud.df,data.frame(option=anoption,x=x,y=y,est.value=est.value))
    }
    rndsample <- sample_n(mysamples,1)%>%select(contains(paste0("est_trial_option_attribute.",trialnumber,".")))
    rndsample <- data.frame(option=1:hm_options,x=as.numeric(select(rndsample,ends_with("1"))),y=as.numeric(select(rndsample,ends_with("2"))))
    ggplot(simexp.df[trialnumber,])+
                                        #est cloud and est-cloud centre
        geom_point(data=estcloud.df,aes(x=x,y=y,color=as.factor(option)),alpha=.05,size=2)+
        geom_point(
            data=estcloud.df%>%group_by(option)%>%summarize(x=mean(x),y=mean(y),est.value=mean(est.value))%>%as.data.frame,
            aes(x=x,y=y,color=as.factor(option)),alpha=1,size=5,shape=13)+
        scale_color_manual(1:3,values=fixedcolors)+
                                        #actual stim truth
                                        #            geom_line(data=data.frame(x=c(0,1),y=c(1,0)),aes(x=x,y=y),alpha=.9,color="grey",linetype="dotted",size=2)+
        geom_point(aes(x=option1attribute1,y=option1attribute2),color=colorgetter(1),size=5)+
        geom_point(aes(x=option2attribute1,y=option2attribute2),color=colorgetter(2),size=5)+
        geom_point(aes(x=option3attribute1,y=option3attribute2),color=colorgetter(3),size=5)+
        ##highlight a single-sample set of estimates (to look for non-independence)
        geom_point(data=rndsample,aes(x=x,y=y,color=as.factor(option)),size=5,alpha=1,shape=18)+
        ##cleanup
        guides(color=FALSE)+
        theme_bw()#+xlim(c(0,1))#+ylim(c(0,1))
}

for(i in 1:400){
    ggsave(stim.plot(4),file=paste0(targfolder,"/frame",i,".jpg"))
}
View("done")
##a command to stitch these plots together into a gif is:
##> convert -resize 20% -delay 25 -loop 0 *.jpg flipbook.gif

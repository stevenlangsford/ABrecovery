library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)
rm(list=ls())

#setwd("vary_tolerance_doubleuniform_II")

load("calc0.15ord0.15tolerance0.1modelgetchoices.stanfit.RData")
mysamples.with <- mysamples
load("calc0.15ord0.15tolerance0.1modelgetchoices_ORD_OFF.stanfit.RData")
mysamples.sans <- mysamples


    targfolder="withvssans"
    dir.create(targfolder);
        
    fixedcolors <- c("red","blue","green")#works with scale_fill_maunual
    colorgetter <- function(id){ #works with independent geoms
        if(id==1) return("red")
        if(id==2) return("blue")
        if(id==3) return("green")
        stop(paste("bad id in colorgetter ",id))
    }


    stim.plot <- function(trialnumber){
        estcloud.with.df <- data.frame()
        estcloud.sans.df <- data.frame()
for(anoption in 1:3){
    x <- mysamples.with[,paste("est_trial_option_attribute",trialnumber,anoption,1,sep=".")]
    y <- mysamples.with[,paste("est_trial_option_attribute",trialnumber,anoption,2,sep=".")]
    est.value <-     mysamples.with[,paste("estval_tracker_raw",trialnumber,anoption,sep=".")]
    estcloud.with.df <- rbind(estcloud.with.df,data.frame(option=anoption,x=x,y=y,est.value=est.value))

    x <- mysamples.sans[,paste("est_trial_option_attribute",trialnumber,anoption,1,sep=".")]
    y <- mysamples.sans[,paste("est_trial_option_attribute",trialnumber,anoption,2,sep=".")]
    est.value <-     mysamples.sans[,paste("estval_tracker_raw",trialnumber,anoption,sep=".")]
    estcloud.sans.df <- rbind(estcloud.sans.df,data.frame(option=anoption,x=x,y=y,est.value=est.value))
}
    
    ggplot(simexp.df[trialnumber,])+
        #est cloud and est-cloud centre WITH
        geom_point(data=estcloud.with.df,aes(x=x,y=y,color=as.factor(option)),alpha=.05,size=2)+
        geom_point(
            data=estcloud.with.df%>%group_by(option)%>%summarize(x=mean(x),y=mean(y),est.value=mean(est.value))%>%as.data.frame,
            aes(x=x,y=y,color=as.factor(option)),alpha=1,size=5,shape=13)+
        scale_color_manual(1:3,values=fixedcolors)+
                                        #SANS
                geom_point(data=estcloud.sans.df,aes(x=x,y=y,color=as.factor(option)),alpha=.05,size=2,shape=4)+
        geom_point(
            data=estcloud.sans.df%>%group_by(option)%>%summarize(x=mean(x),y=mean(y),est.value=mean(est.value))%>%as.data.frame,
            aes(x=x,y=y,color=as.factor(option)),alpha=1,size=5,shape=14)+
        scale_color_manual(1:3,values=fixedcolors)+

                                        #actual stim truth
#            geom_line(data=data.frame(x=c(0,1),y=c(1,0)),aes(x=x,y=y),alpha=.9,color="grey",linetype="dotted",size=2)+
            geom_point(aes(x=option1attribute1,y=option1attribute2),color=colorgetter(1),size=5)+
            geom_point(aes(x=option2attribute1,y=option2attribute2),color=colorgetter(2),size=5)+
        geom_point(aes(x=option3attribute1,y=option3attribute2),color=colorgetter(3),size=5)+
        guides(color=FALSE)+
            theme_bw()#+xlim(c(0,1))#+ylim(c(0,1))
    }

stim.plot(1)

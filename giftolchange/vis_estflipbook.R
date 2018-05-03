library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

rm(list=ls()); hm_options=3

allfiles = list.files(pattern="^calc.*RData")
for(tolset in 1:length(allfiles)){

    tolsetname <- as.character(tolset)
    while(nchar(tolsetname)<3)tolsetname <- paste0("0",tolsetname) #left pad with 0's te get these suckers in a good lexical ordering.
    
    afile=allfiles[tolset]
    
    load(afile)
    targfolder="flipbook" #paste0(strsplit(afile,".RData")[1],"plots")
    dir.create(targfolder);


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
        estcloud.df <- sample_n(estcloud.df,150) #limit estcloud so you can linger and watch it swirl. Note alpha needs to go up as hm_samples goes down.
        
        rndsample <- sample_n(mysamples,1)%>%select(contains(paste0("est_trial_option_attribute.",trialnumber,".")))
        rndsample <- data.frame(option=1:hm_options,x=as.numeric(select(rndsample,ends_with("1"))),y=as.numeric(select(rndsample,ends_with("2"))))
        ggplot(simexp.df[trialnumber,])+
                                        #est cloud and est-cloud centre
            geom_point(data=estcloud.df,aes(x=x,y=y,color=as.factor(option)),alpha=.25,size=2)+
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
                                        #        geom_point(data=rndsample,aes(x=x,y=y,color=as.factor(option)),size=5,alpha=1,shape=18)+
            ##cleanup
            guides(color=FALSE)+
            geom_text(aes(x=2,y=2,label=paste("tolerance: ",as.character(signif(datalist$tolerance_level,3),sep=" "))))+
            theme_bw()+xlim(c(-2,3))+ylim(c(-2,3))
    }

    for(i in 1:50){
        ggsave(stim.plot(7),file=paste0(targfolder,"/tolset",tolsetname,"frame",i,".jpg"))
        print(paste(tolsetname,i)) #progress tracker
    }

}#end for each tolset

View("done")
##a command to stitch these plots together into a gif is:
##> convert -resize 20% -delay 25 -loop 0 *.jpg flipbook.gif
#if you get killed, consider > ... -limit memory 2mb

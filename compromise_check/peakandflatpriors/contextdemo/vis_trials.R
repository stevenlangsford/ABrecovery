library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)
rm(list=ls())

for(afile in list.files(pattern="^calc.*RData")){

    load(afile)

    targfolder=paste0(strsplit(afile,".RData")[1],"plots")
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
        guides(color=FALSE)+
            theme_bw()#+xlim(c(0,1))#+ylim(c(0,1))
    }

    ordprobs <- mysamples%>%select(contains("ordprob_tracker"))

    for(trialnumber in unique(simexp.df$trialid)){

        statusprobs.df <- data.frame();

        ##label code:
        labeller <- function(variable,value){
            optionlabels = list("1"="red",
                                "2"="blue",
                                "3"="green")
            attributelabels = list("1"="X",
                                   "2"="Y")
            
            if(variable%in%c("option1","option2")){
                return(optionlabels[value])
            }else{
                return(attributelabels[value])
            }
        }
        
        
        for(option1 in 1:3){
            for(option2 in 1:3){
                
                for(attribute in 1:2){
                    statusprobs.df <- rbind(statusprobs.df,
                                            data.frame(trialnumber=trialnumber,
                                                       option1=option1,
                                                       option2=option2,
                                                       attribute=as.factor(attribute),
                                                       status=c("<","=",">"),
                                                       prob=sapply(1:3,function(status){mean(mysamples[,paste("ordprob_tracker",trialnumber,option1,option2,attribute,status,sep=".")])})
                                                       ))
                }
            }
        }

        statusprobs.plot <- ggplot(statusprobs.df,aes(x=status,y=prob,fill=attribute))+geom_bar(stat="identity")+theme_bw()+facet_grid(attribute~option1+option2,labeller=labeller)+guides(fill=FALSE)

        choicesummary.df <- mysamples%>%dplyr::select(paste0("generated_choice.",trialnumber))%>%table%>%as.data.frame
        names(choicesummary.df) <- c("optionchosen","count")

        endorsement.plot <- (ggplot(choicesummary.df,aes(x=optionchosen,y=count,fill=optionchosen))+geom_bar(stat="identity")+scale_fill_manual(values=fixedcolors)+theme_bw()+xlab("")+guides(fill=FALSE))
        ggsave(statusprobs.plot+stim.plot(trialnumber)+endorsement.plot+plot_layout(ncol=1),
               file=paste0(targfolder,"/trial",trialnumber,".png"),
               width=15,
               height=15
               )
    }#for each unique trialnumber

}#for each fit (tolerance level)
View("done")

library(tidyverse)
library(patchwork)
rm(list=ls())

saveplots=TRUE; #admin
load(file="contexttest1_fit.RData")

#hacky multiplicity of methods for setting consistent colors.... ouch
fixedcolors <- c("red","blue","green")#works with scale_fill_maunual
colorgetter <- function(id){ #works with independent geoms
    if(id==1) return("red")
    if(id==2) return("blue")
    if(id==3) return("green")
    stop(paste("bad id in colorgetter ",id))
}


stim.plot <- function(trialnumber){
    ggplot(simexp.df[trialnumber,])+
        geom_line(data=data.frame(x=c(0,1),y=c(1,0)),aes(x=x,y=y),alpha=.9,color="grey",linetype="dotted",size=2)+
        geom_point(aes(x=option1attribute1,y=option1attribute2),color=colorgetter(1),size=5)+
        geom_point(aes(x=option2attribute1,y=option2attribute2),color=colorgetter(2),size=5)+
        geom_point(aes(x=option3attribute1,y=option3attribute2),color=colorgetter(3),size=5)+
        theme_bw()+xlim(c(0,1))+ylim(c(0,1))
    }

#stim.plot(1)+stim.plot(2)+stim.plot(3)+stim.plot(4) #groovy.


trialeval.plot <- function(whichtrial,mysamples,hm_options){
valuehist <- ggplot(mysamples)
for(whichoption in 1:hm_options){ #could/should work out hm_options from mysamples? todo.
    valuehist <- valuehist+
        geom_histogram(aes_string(x=paste0("estval_tracker.",whichtrial,".",whichoption),alpha=.5),fill=colorgetter(whichoption))+xlab("")#+
#        geom_vline(aes_string(xintercept=paste0("trueval_tracker.",whichtrial,".",whichoption)[1],color=as.factor(whichoption)))+theme_bw()+
#        ggtitle("Lines: simulation truth")
}

choicesummary.df <- mysamples%>%dplyr::select(paste0("generated_choice.",whichtrial))%>%table%>%as.data.frame
names(choicesummary.df) <- c("optionchosen","count")
return(
    (valuehist+guides(fill=FALSE,alpha=FALSE,color=FALSE)+theme_bw())+
    (
        ggplot(choicesummary.df,aes(x=optionchosen,y=count,fill=optionchosen))+geom_bar(stat="identity")+scale_fill_manual(values=fixedcolors)+theme_bw()+xlab("")+guides(fill=FALSE)            
    )
)
}

##plot individual trials
if(saveplots){
for(i in 1:4){ #1:4 # 5:8 #9:12 #13:16
    ggsave(
               stim.plot(i)/
                (
                    (trialeval.plot(i,withdecoy.samples,3))+
                   (trialeval.plot(i,twooption.samples,2))

               )
           ,file=paste0("plots/trial",i,".png"),width=15)
}
}

##inspect aggregate
##rm(list=setdiff(ls(),c("twooption.samples","withdecoy.samples"))) #just for fun.

twooption.choicesummary <- t(rbind(
     twooption.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==1)}),
     twooption.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==2)}),
    twooption.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==3)})
))%>%as.data.frame
#names(twooption.choicesummary) <- paste0("twoOption",names(twooption.choicesummary))

withdecoy.choicesummary <- t(rbind(
     withdecoy.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==1)}),
     withdecoy.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==2)}),
    withdecoy.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==3)})
))%>%as.data.frame
#names(withdecoy.choicesummary) <- paste0("withdecoy",names(withdecoy.choicesummary))

twooption.choicesummary$exptype = "twooptions"
withdecoy.choicesummary$exptype = "withdecoys"

twovsthreeoptions.df <- rbind(twooption.choicesummary,withdecoy.choicesummary)%>%
    mutate(trialid = rep(simexp.df$trialid,2))%>%group_by(trialid,exptype)%>%summarize_all(sum)%>%ungroup()%>%gather(option,endorsement,V1:V3)

if(saveplots){
    ggsave(


    (ggplot(twovsthreeoptions.df,aes(x=option,y=endorsement,fill=option))+geom_bar(stat="identity")+
     facet_grid(exptype~trialid)+
     geom_text(aes(label=signif(endorsement/40000,2),y=endorsement+650))+
scale_fill_manual(values=c("red3","blue3","green3"))+guides(fill=FALSE)+
    theme_bw())/
(stim.plot(1)+stim.plot(2)+stim.plot(3)+stim.plot(4)+plot_layout(ncol=4))


,file="plots/contextstim.png",width=15)
}


library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)
rm(list=ls())

targfolder="noisesurvey/vary_calc/"


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


noisesurvey.df <- data.frame();

for(afile in list.files(targfolder,pattern=".RData")){

    load(file=paste0(targfolder,afile))

    choicesummary.df <- t(rbind(
        withdecoy.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==1)}),
        withdecoy.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==2)}),
        withdecoy.samples%>%dplyr::select(contains("generated_choice"))%>%summarize_all(function(x){sum(x==3)})
    ))%>%as.data.frame

    choicesummary.df$trialid <- simexp.df$trialid
    choicesummary.df$optiondiff <- with(choicesummary.df, ifelse(trialid==4,V3-V1,V1-V2))
    choicesummary.df$trialtype <- sapply(choicesummary.df$trialid,
                                         function(x){
                                             if(x==1)return("attractionLeft");
                                             if(x==2)return("attractionDown");
                                             if(x==3)return("attractionBoth");
                                             if(x==4)return("compromise");
                                             if(x==5)return("simOutside");
                                             if(x==6)return("simInside");
                                         });
    choicesummary.df$calcsd <- datalist$calcsd_level
    choicesummary.df$ordsd <- datalist$ordsd_level

    noisesurvey.df <- rbind(noisesurvey.df,choicesummary.df)
}



all_trialtype_lines.plot <- ggplot(noisesurvey.df%>%group_by(trialtype,calcsd,ordsd)%>%summarize(optiondiff=mean(optiondiff))%>%ungroup(),aes(x=calcsd,y=optiondiff,group=trialtype,color=trialtype))+
    geom_point()+
    geom_line()+
    theme_bw()

#for stimtype in 1:6


sbs.plot <- function(stimtype){
    (ggplot(noisesurvey.df%>%filter(trialid==stimtype)%>%group_by(calcsd,ordsd)%>%summarize_at(vars(starts_with("V")),mean)%>%ungroup())+
    geom_point(aes(x=calcsd,y=V1),color="red4")+
    geom_line(aes(x=calcsd,y=V1),color="red4")+
    geom_point(aes(x=calcsd,y=V2),color="blue4")+
    geom_line(aes(x=calcsd,y=V2),color="blue4")+
    geom_point(aes(x=calcsd,y=V3),color="green4")+
    geom_line(aes(x=calcsd,y=V3),color="green4")+
#    ylim(c(0,1))+
    theme_bw())
}

for(i in 1:6){
    ggsave( (sbs.plot(i)+stim.plot(i)), file=paste0(targfolder,"plots/stimtype",i,".png"),width=15)
}

ggsave(all_trialtype_lines.plot,file=paste0(targfolder,"plots/noisesurvey_summary.png"))


#repeat, switching ord for calc
targfolder="noisesurvey/vary_ord/"

noisesurvey.df <- data.frame();

all_trialtype_lines.plot <- ggplot(noisesurvey.df%>%group_by(trialtype,calcsd,ordsd)%>%summarize(optiondiff=mean(optiondiff))%>%ungroup(),aes(x=ordsd,y=optiondiff,group=trialtype,color=trialtype))+
    geom_point()+
    geom_line()+
    theme_bw()

#for stimtype in 1:6


sbs.plot <- function(stimtype){
    (ggplot(noisesurvey.df%>%filter(trialid==stimtype)%>%group_by(calcsd,ordsd)%>%summarize_at(vars(starts_with("V")),mean)%>%ungroup())+
    geom_point(aes(x=ordsd,y=V1),color="red4")+
    geom_line(aes(x=ordsd,y=V1),color="red4")+
    geom_point(aes(x=ordsd,y=V2),color="blue4")+
    geom_line(aes(x=ordsd,y=V2),color="blue4")+
    geom_point(aes(x=ordsd,y=V3),color="green4")+
    geom_line(aes(x=ordsd,y=V3),color="green4")+
#    ylim(c(0,1))+
    theme_bw())
}

for(i in 1:6){
    ggsave( (sbs.plot(i)+stim.plot(i)), file=paste0(targfolder,"plots/stimtype",i,".png"),width=15)
}

ggsave(all_trialtype_lines.plot,file=paste0(targfolder,"plots/noisesurvey_summary.png"))


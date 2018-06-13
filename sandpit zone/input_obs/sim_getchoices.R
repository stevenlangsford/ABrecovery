library(tidyverse) #this runs out of memory and crashes. It's bad to fit independent stan models over and over again in a loop. Don't do that.
library(rstan)
library(shinystan)
library(patchwork)
rm(list=ls())
                                        #set.seed(4);
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#setup. Goal is just to generate choices according to howes16 for now, but with the possibility of making individual observations harder/easier, esp. ord ones.
calcobs.df = data.frame(trialid=c(),optionid=c(),value=c(),noisesd=c())
ordobs.df = data.frame(trialid=c(),option1=c(),option2=c(),attribute=c(),value=c(),noisesd=c(),tolerance=c())

##sim stim
##These demo stim made sense for sum land and you're now in product land, they need re-doing.
rawstim.df <- read.table(text="trialid,option1attribute1,option1attribute2,option2attribute1,option2attribute2,option3attribute1,option3attribute2
1,0.25,0.75,0.75,0.25,.10,.75
2,0.25,0.75,0.75,0.25,.25,.60
3,0.25,0.75,0.75,0.25,.15,.65
4,0.6,1,1,0.6,0.8,0.8
5,0.25,0.75,0.75,0.25,0.2,0.8
6,0.25,0.75,0.75,0.25,0.3,0.7",
header=TRUE, sep=",")

## ##diag
## accumulator <- rawstim.df;
## for(i in 1:10)accumulator <- rbind(accumulator,rawstim.df)
## rawstim.df <- accumulator

##add some useful stim info:

rawstim.df$value1 = with(rawstim.df,option1attribute1*option1attribute2)
rawstim.df$value2 = with(rawstim.df,option2attribute1*option2attribute2)
rawstim.df$value3 = with(rawstim.df,option3attribute1*option3attribute2)


##generate the observations you want to use from the raw stim
combineattr <- function(a,b){ #hard assume 2 features
    return(a*b); #appropriate combination rule for areas (h*w) or gambles (p*v)
}

##all calc obs. one per stim, three stim per trial.
for(i in 1:nrow(rawstim.df)){
    for(j in 1:3){
        myattributes <- rawstim.df%>%filter(row_number()==i)%>%select(starts_with(paste0("option",j)))

        calcobs.df = rbind(calcobs.df,data.frame(
                                          trialid=rawstim.df$trialid[i],
                                          optionid=j,
                                          value=combineattr(myattributes[1,1],myattributes[1,2]),
                                          noisesd = .1 #arbitrary?
                                      )
                           )
    }
}#end populate all calc obs.


##all ord obs.
  for(atrial in 1:nrow(rawstim.df)){
    for(option1 in 2:3){
      for(option2 in 1:(option1-1)){
          for(anattribute in 1:2){
              ordobs.df=rbind(ordobs.df,data.frame(
                                            trialid=atrial,
                                            option1=option1,
                                            option2=option2,
                                            attribute=anattribute,
                                            value=rawstim.df[atrial,paste0("option",option1,"attribute",anattribute)]-
                                                rawstim.df[atrial,paste0("option",option2,"attribute",anattribute)],
                                            noisesd=.1,
                                            tolerance=.1
                                        )
                              )
          }#for each attribute
      }#end option2
    }#end option1
  }#end for each trial

ordobs.df$ord_status <- with(ordobs.df,ifelse(abs(value)<tolerance,2,ifelse(value<0,1,3)))

datalist = list(
    hm_trials = length(unique(rawstim.df$trialid)),
    hm_options = 3,
    hm_attributes = 2,
    hm_calcobs = nrow(calcobs.df),
    hm_ordobs = nrow(ordobs.df),
    ord_trialid = ordobs.df$trialid,
    ord_option1=ordobs.df$option1,
    ord_option2=ordobs.df$option2,
    ord_attribute=ordobs.df$attribute,
    ord_value=ordobs.df$ord_status,
    ord_noisesd = ordobs.df$noisesd,
    ord_tolerance = ordobs.df$tolerance,
    calc_trialid = calcobs.df$trialid,
    calc_optionid = calcobs.df$optionid,
    calc_noisesd=calcobs.df$noisesd,
    calc_value=calcobs.df$value)
    

fit <- stan(file="obsinput.stan",
            data=datalist,
            iter=1000,
            init=function(){
                zeros <- rep(0.5,nrow(rawstim.df)*3*2) #trials x options x attributes. Need to consider what counts as a good init value!
                dim(zeros)=c(nrow(rawstim.df),3,2)
                list(est_trial_option_attribute=zeros)
            },
            chains=4)

save.image("demofit.RData")

mysamples <- as.data.frame(extract(fit, permuted = TRUE))
#launch_shinystan(fit)

View("done")

## A="A"
## B="B"
## C="C"
## for(targtrial in 1:10){

##     ggsave(file=paste0("imgcan/targ",targtrial,"_opts_",paste0(options,collapse="_"),".png"),
## ggplot(mysamples)+
##     geom_point(aes_string(x=paste0("options.",targtrial,".1.1"),y=paste0("options.",targtrial,".1.2"),color="A"),alpha=.3)+
##     geom_point(aes_string(x=paste0("options.",targtrial,".2.1"),y=paste0("options.",targtrial,".2.2"),color="B"),alpha=.3)+
##     geom_point(aes_string(x=paste0("options.",targtrial,".3.1"),y=paste0("options.",targtrial,".3.2"),color="C"),alpha=.3)+
##     #geom_point(aes_string(x=mean(options.1.1.1),y=mean(options.1.1.2),color="A"),shape=10,size=5)+
##     #geom_point(aes_string(x=mean(options.1.2.1),y=mean(options.1.2.2),color="B"),shape=10,size=5)+
##     #geom_point(aes_string(x=mean(options.1.3.1),y=mean(options.1.3.2),color="Decoy"),shape=10,size=5)+
##     theme_bw()+geom_point(data=as.data.frame(options),aes(x=V1,y=V2,color="truth"),size=5)+
##     ggtitle(paste(as.character(table(mysamples[,paste0("choice.",targtrial)])),c("A","B","C"),collapse=" "))
## );#end print
##     }#end 10 demo runs
## }#end for each option in optionlist

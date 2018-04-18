library(tidyverse)
library(rstan)
library(patchwork)
library(MCMCpack)#supplies rdirichlet, but overrides 'sample'
rm(list=ls())
set.seed(4)

##sim exp set up: ppnts and stim, which are held constant over different iterations of a survey. These are referenced inside do_a_survey, it's not stand-alone.

hm_ppnts=20 #ppnts are identical for now, but provide repetition over the stimuli to check choice-proportions: each participant sees each stim once.
hm_options=3 #can change to 2 if you want, extra options will just be ignored if present.
hm_attributes=2; #Must match the stim df provided, it's up to you to stay consistent.

#Options for sim.k:
even.sim.k <- matrix(1/hm_attributes,ncol=hm_attributes,nrow=hm_ppnts)#,# demo1: even weight on all attributes: all ppnts are identical
random.sim.k <- rdirichlet(hm_ppnts,rep(1,hm_attributes)) #demo2, random weights, all ppnts are different. Doesn't make much sense with the context effect demo stim, which are set up fo .5,.5 weights.

#Options for stimuli:                   
context_demo_stim <- read.table(text="trialid,option1attribute1,option1attribute2,option2attribute1,option2attribute2,option3attribute1,option3attribute2
1,0.25,0.75,0.75,0.25,.15,.75
2,0.25,0.75,0.75,0.25,.25,.60
3,0.25,0.75,0.75,0.25,.15,.65
4,0.25,0.75,0.75,0.25,0.5,0.5
5,0.25,0.75,0.75,0.25,0.2,0.8
6,0.25,0.75,0.75,0.25,0.3,0.7",
header=TRUE, sep=",") #'base' stim are {.25,.75} & its reflection, both have value .5 under weights {.5,.5}. '3rd option' decoys: match A worse on B, match B worse on A, worse on both, compromise candidate on equivalue line, similarity candidates close by on equivalue line.

shifted_compromise_stim <- read.table(text="trialid,option1attribute1,option1attribute2,option2attribute1,option2attribute2,option3attribute1,option3attribute2
1,-1,1,-0.5,0.5,1,-1
2,0.5,-0.5,0,0,-0.5,0.5
3,0.5,-0.1,0.2,0.2,-0.5,0.9
4,1,0,0,1,0.5,0.5",
header=TRUE, sep=",")#checking for centered-ness re priors on compromise stim specifically.

random_stim <- data.frame()
for(atrial in 1:30){
    for(anoption in 1:3){
        for(anattribute in 1:2){
            random_stim[atrial,paste0("option",anoption,"attribute",anattribute)] <- rnorm(1,0,1)
        }
    }
}
random_stim$trialid <- 1:nrow(random_stim)

#look at the effects of noise and tolerance levels.
do_a_survey <- function(calcsd_levels,ordsd_levels,tolerance_levels,model_names,targfolder){

    ##factors to manipulate should be vectors (of the same length), targfolder name a dir that exists in getwd() (passed without / suffix).
hm_surveypoints <- length(calcsd_levels)
if(!all(as.logical(map(list(calcsd_levels,ordsd_levels,tolerance_levels,model_names), function(x){length(x)==hm_surveypoints}))))stop("ragged setup lists")
if(!targfolder%in%list.files())dir.create(targfolder);
if(!all(model_names%in%list.files()))stop("model not found")



accumulator <- data.frame()
for(i in 1:hm_ppnts){
    simexp.df$ppntid = i;
    accumulator <- rbind(accumulator,simexp.df)
}
simexp.df <- accumulator; rm(accumulator);

#convert to stan-friendly format
ttoa = rep(NA,nrow(simexp.df)*hm_options*hm_attributes)
dim(ttoa) <- c(nrow(simexp.df),hm_options,hm_attributes)#convert to matrix
for(atrial in 1:nrow(simexp.df)){ #populate with values
    for(i in 1:hm_attributes){
        for(j in 1:hm_options){
            ttoa[atrial,j,i] <- simexp.df[atrial,paste0("option",j,"attribute",i)]
        }
    }
}


    for(surveypoint in 1:hm_surveypoints){
        
        calcsd_level = calcsd_levels[surveypoint]
        ordsd_level= ordsd_levels[surveypoint]
        tolerance_level = tolerance_levels[surveypoint]
        modelname = model_names[surveypoint]
        
datalist <<- list(hm_trials=nrow(simexp.df),
                hm_ppnts=hm_ppnts,
                ppntid=simexp.df$ppntid,

                hm_options=hm_options,
                hm_attributes=hm_attributes,

                truth_trial_option_attribute = ttoa,

                k=sim.k,
                calcsd_level=calcsd_level,
                ordsd_level=ordsd_level,
                tolerance_level=tolerance_level
                )

sim.fit <<- stan(file=modelname,
           data=datalist,
           iter=1000,
           init=function(){
               zeros <- rep(0,nrow(simexp.df)*hm_options*hm_attributes)
               dim(zeros)=c(nrow(simexp.df),hm_options,hm_attributes)
               list(est_trial_option_attribute=zeros)
           },
           chains=4,
           control = list(max_treedepth = 15));

mysamples <<- as.data.frame(extract(sim.fit, permuted = TRUE))# extract returns a list of arrays

save.image(file=paste0(targfolder,"/calc",calcsd_level,"ord",ordsd_level,"tolerance",tolerance_level,"model",modelname,"fit.RData"))
    }#for each surveypoint

#    View("done")#optional, creates popup alert when finished.
}#do a survey function



###Sandpit area for actually doing stuff:
#do_a_survey args: calcsd_levels,ordsd_levels,tolerance_levels,modelnames,targfolder
##stim options: context_demo or random
##sim.k options: even or random.

#How good is the return on decisions?
## simexp.df <- random_stim
## sim.k <- random.sim.k
## do_a_survey(
##     calcsd_levels=c(.1,.2,.3),
##     ordsd_levels=c(.1,.2,.3),
##     tolerance_levels=c(.1,.1,.1),
##     model_names=rep("getchoices.stan",3),
##     targfolder="rnd_stim_choices"
##     )

## #center stim.
## hm_trials = 1
## simexp.df <- shifted_compromise_stim
## sim.k <- even.sim.k

## do_a_survey(
## calcsd_levels=rep(.25,hm_trials),
## ordsd_levels=rep(.25,hm_trials),
## tolerance_levels=rep(.1,hm_trials),
## model_names=rep("getchoices.stan",hm_trials),
## targfolder="various_compromise_stim"
## )

## ##change tolerance survey
## hm_trials = 5
## simexp.df <- context_demo_stim
## sim.k <- even.sim.k

## do_a_survey(
## calcsd_levels=rep(.2,hm_trials),#ignored, calc_off model used here.
## ordsd_levels=rep(.2,hm_trials),
## tolerance_levels=seq(from=.01,to=.2,length=hm_trials),
## model_names=rep("getchoices.stan",hm_trials),
## targfolder="ordonly_varytolerance"
## )

simexp.df <- rbind(context_demo_stim,shifted_compromise_stim)
simexp.df$trialid <- 1:nrow(simexp.df)
sim.k <- even.sim.k
## do_a_survey(
##     calcsd_levels=c(.15,.15,.15),
##     ordsd_levels=c(.15,.15,.15),
##     tolerance_levels=c(.05,.1,.2),
##     model_names=rep("getchoices.stan",3),
##     targfolder="tolerance_survey"
##     )

do_a_survey(
    calcsd_levels=c(.15),
    ordsd_levels=c(.15),
    tolerance_levels=c(.2),
    model_names=rep("getchoices.stan",1),
    targfolder="inspect_ordprobs"
    )

library(tidyverse)
library(rstan)
library(patchwork)
#library(MCMCpack)#supplies rdirichlet, but overrides 'sample'

#look at the effects of noise and tolerance levels.
do_a_survey <- function(simexp.df, sim.k,
                        calcsd_levels,ordsd_levels,tolerance_levels,
                        model_names,targfolder,
                        hm_ppnts,
                        hm_options=3, #can change to 2 if you want, extra options will just be ignored if present.
                        hm_attributes=2 #Must match the stim df provided, it's up to you to stay consistent.
){
    ##factors to manipulate should be vectors (of the same length), targfolder name a dir that exists in getwd() (passed without / suffix).
    hm_options<<-hm_options #expose to save image, recovery wants to see it. Terrible style o...
    hm_ppnts<<-hm_ppnts
    
    hm_surveypoints <- length(calcsd_levels)
if(!all(as.logical(map(list(calcsd_levels,ordsd_levels,tolerance_levels,model_names), function(x){length(x)==hm_surveypoints}))))stop("ragged setup lists")
if(!targfolder%in%list.files())dir.create(targfolder);
if(!all(model_names%in%list.files()))stop("model not found")


accumulator <- data.frame()
    for(i in 1:hm_ppnts){
        simexp.df$ppntid = i;
        accumulator <- rbind(accumulator,simexp.df)
    }
    simexp.df <- accumulator; #local version for the rest of this function
    simexp.df <<- accumulator; #boot upstairs to the global .env, expose to save.image

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
        ##for convenience, attach a choice to simexp.df
        simexp.df$choice <<- as.numeric(mysamples%>%dplyr::select(contains("choice"))%>%sample_n(1)%>%t)
        
save.image(file=paste0(targfolder,"/calc",calcsd_level,"ord",ordsd_level,"tolerance",tolerance_level,"model",modelname,"fit.RData"))
    }#for each surveypoint

#    View("done")#optional, creates popup alert when finished.
}#do a survey function




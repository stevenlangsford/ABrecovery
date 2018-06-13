library(tidyverse) #this runs out of memory and crashes. It's bad to fit independent stan models over and over again in a loop. Don't do that.
library(rstan)
library(shinystan)
library(patchwork)
rm(list=ls())
                                        #set.seed(4);
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

##this is a single trial (wedell example).
hm_options = 3
hm_attributes = 2;

attributenoise = c(.1,.1)


optionlist = list(
    matrix(c(.3,.4,.5, #attraction 1: decoy left
             1,1,.8),
           nrow=hm_options,ncol=hm_attributes),
     matrix(c(.4,.4,.5, #attraction 2: decoy down
             .95,1,.8),
            nrow=hm_options,ncol=hm_attributes),
    matrix(c(.35,.4,.5, #attraction 3: decoy both
             .95,1,.8),
           nrow=hm_options,ncol=hm_attributes),
        matrix(c(.3,.4,.5, #similarity1 
             .4/.3,1,.8),
             nrow=hm_options,ncol=hm_attributes),
    matrix(c(.47,.4,.5, #attraction 4: low decoy
             .8,1,.8),
           nrow=hm_options,ncol=hm_attributes)
)# end optionslist

resultlist <- list();

for(options in optionlist){

uber.reckoning.df <- data.frame();
uber.comparison.df <- data.frame();
hm_stim <- 200

for(trialreps in 1:hm_stim){ #this repeats one stim, would be nice to walk through a few.
    ##reckoning observations
    hm_reckoning_passes = 1;
    hm_reckoning_samples = hm_options*hm_attributes; #min 1 obs each
    reckoning.df <- data.frame()
    for(apass in 1:hm_reckoning_passes){
        for(i in 1:hm_options){#walkthrough: could randomize (or choose optimally :-) )
            for(j in 1:hm_attributes){
                reckoning.df <- rbind(reckoning.df,
                                      data.frame(
                                          target=i,
                                          attribute=j,
                                          noise=attributenoise[j],
                                          sample=rnorm(1, options[i,j], attributenoise[j])
                                      ))
            }
        }
    }
    reckoning.df$trialnumber=trialreps
    
    ##comparison observations: 1 each
    hm_comparison_passes = 1;
    comparisons.df <- data.frame()

    for(apass in 1:hm_comparison_passes){
        for(anattribute in 1:hm_attributes){
            for(option1 in 2:hm_options){
                for(option2 in 1:(option1-1)){
                    comparisons.df <- rbind(comparisons.df,
                                            data.frame(
                                                attribute=anattribute,
                                                targ1=option1,
                                                targ2=option2,
                                                noise=attributenoise[anattribute],
                                        #continuous comparison
                                        #sample=rnorm(1,options[option1,anattribute]-options[option2,anattribute],attributenoise[anattribute])

                                        #discrete (binary) comparison
                                                  sample=ifelse(options[option1,anattribute]-options[option2,anattribute]>0,1,0)
                                            ))   
                }
            }
        }
    }
    comparisons.df$trialnumber=trialreps

    uber.reckoning.df <- rbind(uber.reckoning.df,reckoning.df)
    uber.comparison.df <- rbind(uber.comparison.df,comparisons.df)    
}#end for trialreps

reckoning.df <- uber.reckoning.df
comparisons.df <- uber.comparison.df

                                        #End data setup

datalist=list(hm_options=hm_options,
              hm_attributes=hm_attributes,
              hm_trials=hm_stim,

              hm_reckoning_samples=nrow(reckoning.df),
              reckoning_trialnumber=reckoning.df$trialnumber,
              reckoning_target = reckoning.df$target,
              reckoning_attribute = reckoning.df$attribute,
              reckoning_noise = reckoning.df$noise,
              reckoning_sample = reckoning.df$sample,

              hm_comparison_samples = nrow(comparisons.df),
              comparison_trialnumber=comparisons.df$trialnumber,
              comparison_attribute = comparisons.df$attribute,
              comparison_targ1 = comparisons.df$targ1,
              comparison_targ2 = comparisons.df$targ2,
              comparison_noise = comparisons.df$noise,
              comparison_sample = comparisons.df$sample
              )

fit <- stan(file="compest.stan",
            data=datalist,
            iter=1000,
            chains=4)

mysamples <- as.data.frame(extract(fit, permuted = TRUE))
                                        #launch_shinystan(fit)

prefcount <-
    mysamples%>%select(starts_with("choice"))%>%apply(2,function(x){c(sum(x==1),sum(x==2),sum(x==3))})
    resultlist[[length(resultlist)+1]] <- table(apply(prefcount,2,function(x){which(x==max(x))[sample(length(which(x==max(x))),1)]}))#hacky rnd tiebreaker.
                                        #ggplot(mysamples,aes(x=choice))+geom_bar()+theme_bw()+
A="A"
B="B"
C="C"
for(targtrial in 1:10){

    ggsave(file=paste0("imgcan/targ",targtrial,"_opts_",paste0(options,collapse="_"),".png"),
ggplot(mysamples)+
    geom_point(aes_string(x=paste0("options.",targtrial,".1.1"),y=paste0("options.",targtrial,".1.2"),color="A"),alpha=.3)+
    geom_point(aes_string(x=paste0("options.",targtrial,".2.1"),y=paste0("options.",targtrial,".2.2"),color="B"),alpha=.3)+
    geom_point(aes_string(x=paste0("options.",targtrial,".3.1"),y=paste0("options.",targtrial,".3.2"),color="C"),alpha=.3)+
    #geom_point(aes_string(x=mean(options.1.1.1),y=mean(options.1.1.2),color="A"),shape=10,size=5)+
    #geom_point(aes_string(x=mean(options.1.2.1),y=mean(options.1.2.2),color="B"),shape=10,size=5)+
    #geom_point(aes_string(x=mean(options.1.3.1),y=mean(options.1.3.2),color="Decoy"),shape=10,size=5)+
    theme_bw()+geom_point(data=as.data.frame(options),aes(x=V1,y=V2,color="truth"),size=5)+
    ggtitle(paste(as.character(table(mysamples[,paste0("choice.",targtrial)])),c("A","B","C"),collapse=" "))
);#end print
    }#end 10 demo runs
}#end for each option in optionlist

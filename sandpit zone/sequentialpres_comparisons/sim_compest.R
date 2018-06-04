library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())
#set.seed(4);
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

##this is a single trial (wedell example).
hm_options = 3
hm_attributes = 2;

attributenoise = c(.1,.1)
options = matrix(c(.3,.4,.5,
                   1,1,.8),
                 nrow=hm_options,ncol=hm_attributes)


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
                                            sample=rnorm(1,options[option1,anattribute]-options[option2,anattribute],attributenoise[anattribute])
                                        ))
                
            }
        }
    }
}

datalist=list(hm_options=hm_options,
              hm_attributes=hm_attributes,

              hm_reckoning_samples=nrow(reckoning.df),
              reckoning_target = reckoning.df$target,
              reckoning_attribute = reckoning.df$attribute,
              reckoning_noise = reckoning.df$noise,
              reckoning_sample = reckoning.df$sample,

              hm_comparison_samples = nrow(comparisons.df),
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
print(options)

ggplot(mysamples,aes(x=choice))+geom_bar()

library('tidyverse')
library('rstan')
library('shinystan')
library('patchwork')
rm(list=ls())
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#read simexp
simexp.df <- read.csv("simexp_pplgenerated.csv");

simexp.df$ppntid <- simexp.df$ppntid+1 # +1 converts between ppl friendly and stan friendly index schemes.

#convert to stand friendly data input (ppnt properties into vectors hm_ppnts long)
ppnt_ks <- sapply(unique(simexp.df$ppntid),function(x){simexp.df%>%filter(ppntid==x)%>%select(Bweight)%>%filter(row_number()==1)%>%as.numeric})

ppnt_calcsd <- sapply(unique(simexp.df$ppntid),function(x){simexp.df%>%filter(ppntid==x)%>%select(calcsd)%>%filter(row_number()==1)%>%as.numeric})


ppnt_tolerance <- sapply(unique(simexp.df$ppntid),function(x){simexp.df%>%filter(ppntid==x)%>%select(toleranceB)%>%filter(row_number()==1)%>%as.numeric})


ttoa = rep(NA,nrow(simexp.df)*3*2)# trials*options*attributes
dim(ttoa) <- c(nrow(simexp.df),3,2)#convert to matrix
for(atrial in 1:nrow(simexp.df)){ #populate with values
    ttoa[atrial,1,1]=simexp.df[atrial,"attributeA_option1"]
    ttoa[atrial,2,1]=simexp.df[atrial,"attributeA_option2"]
    ttoa[atrial,3,1]=simexp.df[atrial,"attributeA_option3"]
    ttoa[atrial,1,2]=simexp.df[atrial,"attributeB_option1"]
    ttoa[atrial,2,2]=simexp.df[atrial,"attributeB_option2"]
    ttoa[atrial,3,2]=simexp.df[atrial,"attributeB_option3"]
}
##truth_trial_option_attribute = matrix()

datalist = list(hm_trials=nrow(simexp.df),
                hm_ppnts=max(simexp.df$ppntid),
                ppntid=simexp.df$ppntid,

                hm_options=3,
                hm_attributes=2,

                truth_trial_option_attribute = ttoa,
                choice=simexp.df$choice,
                
                tolerance = ppnt_tolerance
                )

#fit: sensible things to recover are A, B, and estval.
fit <- stan(file="agentRecovery.stan",
           data=datalist,
           iter=1000,
           chains=4,
           control = list(max_treedepth = 15));


mysamples <- as.data.frame(extract(fit, permuted = TRUE)) # extract returns a list of arrays

save.image("sandpitfit.RData")

View("done fitting matrix version")
##sanity check/'done' popup
## estvals <- mysamples%>%select(contains("calcsd"))#contains "k"
## meanest <- apply(estvals,2,mean)
## x11(); ggplot(data.frame(sim_calcsd=ppnt_calcsd,recovered_calcsd=meanest),aes(x=sim_calcsd,y=recovered_calcsd))+geom_point()+theme_bw()#"ppnt_k"


estvals <- mysamples%>%select(contains("k."))#contains "k"
meanest <- apply(estvals,2,mean)

khist.df <- gather(estvals,whichk,asample,k.1:k.6)
x11(); (ggplot(khist.df,aes(x=asample))+geom_histogram()+facet_wrap(~whichk)+theme_bw()+
    geom_vline(data=data.frame(sim=ppnt_ks,whichk=paste0("k.",1:length(ppnt_ks))),aes(xintercept=sim,color="sim",linetype="sim"))+
    geom_vline(data=data.frame(x=.5,whichk=paste0("k.",1:length(ppnt_ks))),aes(xintercept=x,color="prior",linetype="prior"))+
    guides(linetype=FALSE))+
    (ggplot(data.frame(sim_k=ppnt_ks,recovered_k=meanest),aes(x=sim_k,y=recovered_k))+geom_point()+theme_bw())


launch_shinystan(fit);

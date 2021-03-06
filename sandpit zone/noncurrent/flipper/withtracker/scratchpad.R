library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())

many.relations <- list()

for(afile in list.files(pattern="*.RData")){
    load(afile)
    mysamples <- as.data.frame(rstan::extract(sim.fit,permuted=TRUE))
    relations <- mysamples%>%select(starts_with("relation_tracker"))
    many.relations[length(many.relations)+1] <- relations
    print(table(apply(relations,1,sum)))
}

sapply(many.relations,sum)/length(many.relations[[1]]) #does not change with orderr, should increase as orderr makes bad matches more and more unlikely? Current match rate ~46% 


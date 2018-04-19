library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

rm(list=ls())
afile=list.files(pattern="^calc.*RData")
load(afile)
trialnumber=4 #4=compromise effect
estvals <- mysamples%>%select(contains(paste("est_trial_option_attribute.",trialnumber,".",sep=""))) #this is the one with the extremification that actually drives choice

estvals <- data.frame(x1=select(estvals,ends_with("1.1"))%>%unlist%>%as.numeric,
                   x2=select(estvals,ends_with("2.1"))%>%unlist%>%as.numeric,
                   x3=select(estvals,ends_with("3.1"))%>%unlist%>%as.numeric,
                   y1=select(estvals,ends_with("1.2"))%>%unlist%>%as.numeric,
                   y2=select(estvals,ends_with("2.2"))%>%unlist%>%as.numeric,
                   y3=select(estvals,ends_with("3.2"))%>%unlist%>%as.numeric)

allmeans.df <- summarize_all(estvals,mean)

with(as.data.frame(allmeans.df),c(x1+y1,x2+y2,x3+y3)/2)

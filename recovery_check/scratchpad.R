library(tidyverse)
library(rstan)
library(shinystan)
rm(list=ls())

load("calc0.2ord0.2tolerance0.1modelgetchoices.stanfit.RData")


bob <- mysamples%>%select(starts_with("estval_tracker.5."))
names(bob) <- c("A","B","D")
ggplot(bob)+geom_histogram(aes(x=A,fill="A"))+geom_histogram(aes(x=B,fill="B"))+geom_histogram(aes(x=D,fill="D"))+theme_bw()


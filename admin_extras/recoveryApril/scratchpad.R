library(tidyverse)
library(rstan)
library(shinystan)

load("recoveryfit.RData")

mysamples <<- as.data.frame(extract(recovery.fit, permuted = TRUE))# extract returns a list of arrays
launch_shinystan(recovery.fit)

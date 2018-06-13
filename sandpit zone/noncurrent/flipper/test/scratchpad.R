library(rstan)
library(shinystan)
library(tidyverse)

load("calc0.15ord0.01tolerance0.1modelgetchoices_flipper.stanfit.RData")
launch_shinystan(sim.fit)

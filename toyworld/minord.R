library(tidyverse)
library(rstan)
library(shinystan)

set.seed(1)

n_obs <- 10
##sim data

tolerance = .5

x1 <- rnorm(n_obs)
x2 <- rnorm(n_obs)

data.df <- data.frame(x1=x1,
                      x2=x2#,
#                      ord=ifelse(abs(x1-x2)<tolerance, 2 , ifelse(x1<x2, 1 , 3))
                      )

datalist <- list(n=nrow(data.df),
                 x1=data.df$x1,
                 x2=data.df$x2,
                 tolerance=tolerance,
                 calcnoise = .1,
                 orderr = .1
                )

##Nicest-so-far bern implementation, doesn't mix though :-(
## target.fit <- stan(file="full.stan",
##                 data=datalist,
##                 chains=4,
##                 iter=1000
##                 )


target.fit <- stan(file="likecurrent.stan",
            data=datalist,
            chains=4,
            iter=1000
            )


sans.fit <- stan(file="sansord.stan",
            data=datalist,
            chains=4,
            iter=1000
            )

sans.samples <- as.data.frame(extract(sans.fit,permuted=TRUE))
target.samples <-as.data.frame(extract(target.fit,permuted=TRUE))
save.image(file="test.RData")

source("vis.R");

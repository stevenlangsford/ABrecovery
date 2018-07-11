                                        #library(MCMCpack)#supplies rdirichlet, but overrides 'sample'
library(tidyverse)
library(purrr)
library(patchwork)
rm(list=ls())
set.seed(42)
##Setup config:
hm_ppnts = 10
k1 <- seq(from=.1,to=.9,length=hm_ppnts)
k2 <- 1-k1
sim.k <- matrix(c(k1,k2), nrow=hm_ppnts,ncol=2,byrow=FALSE) #k is the weight given to each attribute, rows sum to 1
save(sim.k,file="simk.RData") #used here, in simulate choices, and in plotting recovery success.

stim.per.ppnt = 40
stepsize = 10 #Should divide stim.per.ppnt evenly, to create the seq walkthrough below:

for(hm_rndstim in seq(from = 0, to = stim.per.ppnt, by=stepsize)){
    ##Setup functions
    randomstim <- function(){
        list(x=runif(1,-1,1),y=runif(1,-1,1))
    }

    stim.slider<- function(init,k,delta_x,delta_value){
        ##shift init by delta_x in x, set y such that value of alternative is initvalue+delta_value under weights k
        targvalue = k[1]*init$x + k[2]*init$y+delta_value #k must sum to 1.
        
        new.x=init$x+delta_x
        new.y = (targvalue-k[1]*new.x)/k[2]
        return(list(x=new.x,y=new.y))
    }
    
    stim.slider.y <- function(init,k,delta_y,delta_value){
        targvalue = k[1]*init$x + k[2]*init$y+delta_value #k must sum to 1.
        new.y=init$y+delta_y
        new.x = (targvalue-k[2]*new.y)/k[1]
        return (list(x=new.x,y=new.y))
    }

    list.to.df <- function(optionlist,ppntid,trialtype){
        return(data.frame(
            option1attribute1=optionlist[[1]]$x,
            option1attribute2=optionlist[[1]]$y,
            option2attribute1=optionlist[[2]]$x,
            option2attribute2=optionlist[[2]]$y,
            option3attribute1=optionlist[[3]]$x,
            option3attribute2=optionlist[[3]]$y,
            value1=optionlist[[1]]$x*sim.k[ppntid,1]+optionlist[[1]]$y*sim.k[ppntid,2],
            value2=optionlist[[2]]$x*sim.k[ppntid,1]+optionlist[[2]]$y*sim.k[ppntid,2],
            value3=optionlist[[3]]$x*sim.k[ppntid,1]+optionlist[[3]]$y*sim.k[ppntid,2],
            ppntid=ppntid,
            trialtype=trialtype
        )
        )
        
    }


    ##main() starts here:
    stim.df <- data.frame();

    for(appnt in 1:hm_ppnts){
        if(hm_rndstim>=1){
            for(atrial in 1:hm_rndstim){
                rndstim <- list.to.df(list(randomstim(),randomstim(),randomstim()),appnt,"random")#not added to stimdf atm.
                stim.df <- rbind(stim.df,rndstim);
            }
        }
        if((stim.per.ppnt-hm_rndstim)>=1){
            for(atrial in 1:(stim.per.ppnt-hm_rndstim)){
                if(runif(1,0,1)>.5){
                attraction_target <- randomstim();
                attraction_comp <- stim.slider(init=attraction_target,k=sim.k[appnt,],delta_x=1,delta_value=0)
                attraction_decoy <- stim.slider(init=attraction_target,k=sim.k[appnt,],delta_x=runif(1,.9,1.1),delta_value=-0.15)
                }else{
                    attraction_target <- randomstim();
                    attraction_comp <- stim.slider.y(init=attraction_target,k=sim.k[appnt,],delta_y=1,delta_value=0)
                    attraction_decoy <- stim.slider.y(init=attraction_target,k=sim.k[appnt,],delta_y=runif(1,.9,1.1),delta_value=-0.15)
                }
                stim.df <- rbind(stim.df,
                                 list.to.df(list(attraction_target,attraction_comp,attraction_decoy),appnt,"attraction")
                                 )
            }
        }
    }

    stim.df$trialid <- 1:nrow(stim.df)

                                        #Output:
    write.csv(stim.df,file=paste0( "stimsets/rndstim", hm_rndstim, "attnstim", (stim.per.ppnt-hm_rndstim), "stimuli.csv"), row.names=FALSE)

}



## ##Diag checks:
## plotstim <- function(dfrow){
##     x11(); print(
##                ggplot(dfrow)+
##                geom_point(aes(x=option1attribute1,y=option1attribute2,color="one"))+
##                geom_point(aes(x=option2attribute1,y=option2attribute2,color="two"))+
##                geom_point(aes(x=option3attribute1,y=option3attribute2,color="three"))+
##                theme_bw()+xlim(c(-2,2))+ylim(c(-2,2))+
##                ggtitle(paste("ppnt",dfrow$ppntid,",",paste(as.character(sim.k[dfrow$ppntid,]),collapse=":"),dfrow$trialtype))
##            )
## }


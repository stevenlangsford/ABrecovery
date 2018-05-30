library(MCMCpack)#supplies rdirichlet, but overrides 'sample'
library(tidyverse)
library(purrr)
library(patchwork)
rm(list=ls())
set.seed(1)
hm_ppnts=3 #low, med, high k
                                        #random.sim.k <- rdirichlet(hm_ppnts,rep(1,2)) #demo2, random weights, all ppnts are different. Doesn't make much sense with the context effect demo stim
lomedhi.sim.k <- matrix(c(.1,.9,.5,.5,.9,.1), nrow=3,ncol=2,byrow=TRUE)

tolerance = .1
stim.per.ppnt = 15
                                        #init = list(x=.25,y=.75)
randomstim <- function(){
    list(x=runif(1,0,1),y=runif(1,0,1))
}

getalternative <- function(init,k,delta_x,delta_value){
    ##shift init by d in x, set y such that value of alt==value of init given weights k
    targvalue = k[1]*init$x + k[2]*init$y+delta_value #k must sum to 1.

    new.x=init$x+delta_x
    new.y = (targvalue-k[1]*new.x)/k[2]
    return(list(x=new.x,y=new.y))
}

fun.trio <- function(k){
    init = list(x=runif(1,0,1),y=runif(1,0,1)) #maybe init from priors?
    alt.distance = runif(1,-1,1) #check this dist between options is reasonable?
    alternative = getalternative(init,k,alt.distance,0)
    decoy = getalternative(init,k,rnorm(1,alt.distance/2,tolerance),-0.15) #check this value difference is reasonable?

    return(list(init,alternative,decoy))
}

list.to.df <- function(optionlist){
    return(data.frame(
        option1attribute1=optionlist[[1]]$x,
        option1attribute2=optionlist[[1]]$y,
        option2attribute1=optionlist[[2]]$x,
        option2attribute2=optionlist[[2]]$y,
        option3attribute1=optionlist[[3]]$x,
        option3attribute2=optionlist[[3]]$y)
        )

}


list.to.plot <- function(optionlist){
    return(
        ggplot(data.frame(x=unlist(map(optionlist,function(option){option$x})),
                          y=unlist(map(optionlist,function(option){option$y})),optionid=1:3),
               aes(x=x,y=y,color=as.factor(optionid)))+geom_point(size=5)+theme_bw()#+xlim(-2,2)+ylim(-2,2)
    )
}


##main() starts here:

##version 1: 'random fun stim'
## stim.df <- data.frame()
## for(i in 1:hm_ppnts){
##     for(j in 1:stim.per.ppnt){
##     mytrio <- fun.trio(lomedhi.sim.k[i,])
##     stim.df <- rbind(stim.df,list.to.df(mytrio))
## #    x11(); print(list.to.plot(mytrio)+ggtitle(paste(signif(random.sim.k[i,],3),collapse=":")))
##     }
## }

## stim.df$trialid=1:nrow(stim.df)
## write.csv(stim.df,"lomedhi_funstim.csv",row.names=FALSE)

similarity.stim <- function(k){
    init=list(x=runif(1,0,1),y=runif(1,-1,1))
    alt = getalternative(init,k,1,0)
    decoy = getalternative(init,k,.05,0)
    return(list.to.df(list(init,alt,decoy)))
}

swingaround.stim.set <- function(n,alt_dist=1,k){ #'set' because returns a df with a n rows, not a list
    init = lapply(1:n,function(i)list(x=runif(1,0,1),y=runif(1,-1,1))) #maybe init from priors?
    alternative = lapply(1:n,function(i)getalternative(init[[i]],k,alt_dist,0))
    decoy = lapply(1:n,function(i){getalternative(init[[i]],k,seq(from=-.5,to=.5,length=n)[i],seq(from=-.15,to=0,length=n)[i])}) #value diff: little arc around option point
    simexp.df <- data.frame()
    getset <- function(i){list(init[[i]],alternative[[i]],decoy[[i]])}
    for(i in 1:n){simexp.df <- rbind(simexp.df,list.to.df(getset(i)))}
    return(simexp.df)
}

                                        #main() starts here: bunch of csv writing things. As functions just to archive how you created all those csvs.
write_swingaroundlomedhi <- function(){
    stim.df <- data.frame()
    sim.k <- lomedhi.sim.k #maybe switch in something else later?
    for(i in 1:nrow(sim.k)){
        stim.df <- rbind(stim.df,
                         swingaround.stim.set(5,1,sim.k[i,]),
                         similarity.stim(sim.k[i,])
                         )
    }
    stim.df$trialid <- 1:nrow(stim.df)
    stim.df <- rbind(stim.df,stim.df)#double up. Could consider triple?
    write.csv(stim.df,file="swingaround_lomedhi_stim.csv",row.names=FALSE)
}

write_rndstim <- function(){
    stim.df <- data.frame();
    sim.k <- lomedhi.sim.k #for now
    for(i in 1:20){
        stim.df <- rbind(stim.df, list.to.df(list(randomstim(),randomstim(),randomstim())))
    }
    stim.df$trialid <- 1:nrow(stim.df)
    write.csv(stim.df,file="rndstim.csv",row.names=FALSE)
}



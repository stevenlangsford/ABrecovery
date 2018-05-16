library(MCMCpack)#supplies rdirichlet, but overrides 'sample'
library(tidyverse)
library(purrr)
set.seed(1)
hm_ppnts=5
random.sim.k <- rdirichlet(hm_ppnts,rep(1,2)) #demo2, random weights, all ppnts are different. Doesn't make much sense with the context effect demo stim
tolerance = .1
stim.per.ppnt = 4
init = list(x=.25,y=.75)

getalternative <- function(init,k,delta_x,delta_value){
                                        #shift init by d in x, set y such that value of alt==value of init given weights k
    targvalue = k[1]*init$x + k[2]*init$y+delta_value #k must sum to 1.

    new.x=init$x+delta_x
    new.y = (targvalue-k[1]*new.x)/k[2]
    return(list(x=new.x,y=new.y))
}

fun.trio <- function(k){
    init = list(x=runif(1,0,1),y=runif(1,0,1)) #init from priors
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
               aes(x=x,y=y,color=as.factor(optionid)))+geom_point(size=5)+theme_bw()+xlim(-2,2)+ylim(-2,2)
        )
}

                                        #sandpit
stim.df <- data.frame()
for(i in 1:hm_ppnts){
    for(j in 1:stim.per.ppnt){
    mytrio <- fun.trio(random.sim.k[i,])
    stim.df <- rbind(stim.df,list.to.df(mytrio))
#    x11(); print(list.to.plot(mytrio)+ggtitle(paste(signif(random.sim.k[i,],3),collapse=":")))
    }
}

stim.df$trialid=1:nrow(stim.df)
write.csv(stim.df,"fun_for_ks_stim.csv",row.names=FALSE)

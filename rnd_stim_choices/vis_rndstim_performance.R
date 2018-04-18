library(tidyverse)
library(patchwork)
rm(list=ls())

if("vis_summary.csv"%in%list.files()){
    survey.df <- read.csv("vis_summary.csv")
}else{
    survey.df <- data.frame();
    for(afile in list.files(pattern="^calc")){
        load(afile)

        estvals <- mysamples%>%select(contains("estval"))
        truevals <- mysamples%>%select(contains("trueval"))
        choices <- mysamples%>%select(contains("choice"))
        agent_value <- matrix(NA,ncol=ncol(choices),nrow=nrow(choices))
        oracle_value <- matrix(NA,ncol=ncol(choices),nrow=nrow(choices))

        for(i in 1:nrow(choices)){#samples
            for(j in 1:ncol(choices)){#trials
                if(j%%100==0)print(paste(i,j)) #progress tracker
                agent_value[i,j] <- truevals[i,paste0("trueval_tracker.",j,".",choices[i,j])] #selects the true value of the option indicated by the sampled choice
                oracle_value[i,j] <- max(truevals[i,paste0("trueval_tracker.",j,".",1)],truevals[i,paste0("trueval_tracker.",j,".",2)],truevals[i,paste0("trueval_tracker.",j,".",3)])
                
            }
        }
        expected_return <- apply(agent_value,2,mean)
        oracle_return <- apply(oracle_value,2,mean)
        performance <- (oracle_return-expected_return)

        ret <- data.frame(simrun=afile,agent=expected_return,oracle=oracle_return, performance=performance)
        survey.df <- rbind(survey.df,ret)
    }

    write.csv(survey.df,file="vis_summary.csv")
}#end if didn't find csv already written

#hist(survey.df$performance)#popup

####################################################################################################
stripname <- function(aname){
    chunks <- unlist(strsplit(aname,"tolerance|calc|ord|model"))
#    browser()
    return(list(calcsd=chunks[2],ordsd=chunks[3],tolerance=chunks[4]))
    }
for(i in 1:nrow(survey.df)){
    simproperties = stripname(as.character(survey.df[i,"simrun"]))
    survey.df[i,"calcsd"]=simproperties$calcsd
    survey.df[i,"ordsd"]=simproperties$ordsd
    survey.df[i,"tolerance"]=simproperties$tolerance
}

ggplot(survey.df,aes(x=performance,fill=calcsd,group=calcsd))+geom_density(alpha=.5)+theme_bw()+xlab("oracle-agent returns") #Most obvious result ever.

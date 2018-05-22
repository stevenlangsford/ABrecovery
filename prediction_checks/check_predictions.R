library(tidyverse)
rm(list=ls())
load("test_choices/calc0.15ord0.15tolerance0.1modelgetchoices.stanfit.RData")
test_truth_choices <- mysamples%>%select(starts_with("generated_choice"))
test_simexp.df <- simexp.df
rm(list=setdiff(ls(),c("test_truth_choices","test_simexp.df")))


get.plottables <- function(rdatafile){
    load(rdatafile)
    
test_predicted_choices <- recovery.samples%>%select(starts_with("predicted"))

for(i in 1:nrow(simexp.df)){
    predsummary <- test_predicted_choices%>%select(i)%>%summarize(ones=sum(.==1),twos=sum(.==2),threes=sum(.==3))
    simsummary <- test_truth_choices%>%select(i)%>%summarize(ones=sum(.==1),twos=sum(.==2),threes=sum(.==3))
    difference.df <- predsummary-simsummary
    simexp.df[i,"ones"]=difference.df$ones
    simexp.df[i,"twos"]=difference.df$twos
    simexp.df[i,"threes"]=difference.df$threes
}

    return(simexp.df%>%gather(response,count,ones:threes))

}

baseline <- get.plottables("regression_recovery.RData");
blplus <- get.plottables("blplus_recovery.RData")

ggplot(baseline,aes(x=response,y=count))+geom_bar(stat="identity")+facet_grid(trialid~ppntid)+ggtitle("baseline")
x11();
ggplot(blplus,aes(x=response,y=count))+geom_bar(stat="identity")+facet_grid(trialid~ppntid)+ggtitle("plus")

## ##diag zone

trialeval.plot <- function(whichtrial,mysamples){
valuehist <- ggplot(mysamples)
for(whichoption in 1:hm_options){
    valuehist <- valuehist+
        geom_histogram(aes_string(x=paste0("estval_tracker.",whichtrial,".",whichoption),fill=as.factor(whichoption),alpha=.5))+xlab("")#+
#        geom_vline(aes_string(xintercept=paste0("trueval_tracker.",whichtrial,".",whichoption)[1],color=as.factor(whichoption)))+theme_bw()+
#        ggtitle("Lines: simulation truth")
}
choicesummary.df <- mysamples%>%select(paste0("generated_choice.",whichtrial))%>%table%>%as.data.frame
names(choicesummary.df) <- c("optionchosen","count")

return(
    (valuehist+guides(fill=FALSE,alpha=FALSE,color=FALSE)+theme_bw())+
    (ggplot(choicesummary.df,aes(x=optionchosen,fill=as.factor(optionchosen),y=count))+geom_bar(stat="identity")+theme_bw()+xlab("")+guides(fill=FALSE))

)
}

gen.vs.recover.eval.plot <- function(atrial,simsamples,recoversamples){
trialeval.plot(atrial,simsamples)/trialeval.plot(atrial,recoversamples)#+plot_layout(ncol=2)
}

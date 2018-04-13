functions{

  real ordprob(real a,real b, real tolerance, int whichoutcome){
    real orderr;
    int truerelation;

    orderr=0.1;
    truerelation = fabs(a-b)<tolerance ? 2 : a<b ? 1 : 3;
      
    if(truerelation==whichoutcome) return 1-(2.0/3.0)*orderr;
    else return (1.0/3.0)*orderr;
  }
    
}

data{
  int hm_trials; //hm_trials
  int hm_ppnts;
  int ppntid[hm_trials];

  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes]  truth_trial_option_attribute[hm_trials];

  //supply this for fixed-sim-generation:
  vector[hm_attributes] k[hm_ppnts];
  
  //  int choice[hm_trials]; //ppnt responses!
}

parameters{
  //*inferred ppnt parameters*
  //  simplex[hm_attributes] k[hm_ppnts];//implicit prior is uniform over valid simplexes

  //attribute estimates
  matrix[hm_options,hm_attributes]  est_trial_option_attribute[hm_trials];
}//end parameters

model{
  //todo: move these to params and infer them.
  real calcsd[hm_ppnts];
  real tolerance[hm_ppnts]; //?reasonable bounds?

  //actual local variables:
  vector[hm_options] estval[hm_trials];
  vector[3] ordprob_trial_option1_option2_attribute_status[hm_trials,hm_options,hm_options,hm_attributes];

  //model:
  //setup ppnts
  for(appnt in 1:hm_ppnts){ //to replace with priors?
    calcsd[appnt]=0.3;
    tolerance[appnt]=0.05;
  }
  //populate estimated attribute values (with a calculation observation)
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      for(anattribute in 1:hm_attributes){
	est_trial_option_attribute[atrial,anoption,anattribute]~normal(truth_trial_option_attribute[atrial,anoption,anattribute],calcsd[ppntid[atrial]]);
      }
    }
  }
  //apply an ordinal observation:
  for(atrial in 1:hm_trials){
    for(option1 in 2:hm_options){//indexes for lower triangle only: comparisons are directionless & not with self
      for(option2 in 1:(option1-1)){
	for(anattribute in 1:hm_attributes){
	  //prob status '<': phi(-tolerance)
	  for(outcome in 1:3){
	    ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,outcome]=ordprob(truth_trial_option_attribute[atrial,option1,anattribute],truth_trial_option_attribute[atrial,option2,anattribute],tolerance[ppntid[atrial]],outcome);
	    }
	  
	  target += categorical_lpmf(fabs(truth_trial_option_attribute[atrial,option1,anattribute]-truth_trial_option_attribute[atrial,option2,anattribute])<tolerance[ppntid[atrial]] ? 2 : truth_trial_option_attribute[atrial,option1,anattribute]<truth_trial_option_attribute[atrial,option2,anattribute] ? 1 : 3 | ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute]);//Before the pipe: nested ternary if, evaluates to true relation between options 1 & 2 on target attribute {1:'<',2:'=',3:'>'}: after pipe: the probability of each outcome given the current attribute estimates, a vector length 3 over the possible status values {<,=,>}
	}//attribute
      }//option2
    }//option1
  }//trial

  
  //in recovery version, observe a choice. This is choice generation version, choice generation happens in generated quantities block.
  /* for(atrial in 1:hm_trials){ */
  /*   for(anoption in 1:hm_options){ */
  /*     estval[atrial,anoption]=est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]; */
  /*   } */
  /*   choice[atrial]~categorical_logit(estval[atrial]); */
  /* } */
  
}//model block

generated quantities{//diagnostics, can delete
  int generated_choice[hm_trials];
  vector[hm_options] estval_tracker[hm_trials];
  vector[hm_options] trueval_tracker[hm_trials];
  
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estval_tracker[atrial,anoption]=(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]+1)^7;//effect of the power is to move softmax towards hard-max, could consider higher powers? Better make them odd though to be sign preserving, estval could be negative?
      trueval_tracker[atrial,anoption]=truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]];
    }
    generated_choice[atrial]=categorical_logit_rng(estval_tracker[atrial]);
  }
}

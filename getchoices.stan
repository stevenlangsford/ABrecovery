data{
  int hm_trials; //hm_trials
  int hm_ppnts;
  int ppntid[hm_trials];

  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes] truth_trial_option_attribute[hm_trials];

  //supply this for fixed-sim-generation:
  vector[hm_attributes] k[hm_ppnts];
  real calcsd_level;
  real ordsd_level;
  //  int choice[hm_trials]; //ppnt responses!
}

parameters{
  //*inferred ppnt parameters*
  //  simplex[hm_attributes] k[hm_ppnts];//implicit prior is uniform over valid simplexes

  //attribute estimates
  matrix[hm_options,hm_attributes]  est_trial_option_attribute[hm_trials];

  vector[hm_options] calcobs[hm_trials];
}//end parameters

model{
  //todo: move these to params and infer them.
  real calcsd[hm_ppnts];
  real ordsd[hm_ppnts];
  real tolerance[hm_ppnts]; //?reasonable bounds?

  //actual local variables:
  vector[hm_options] estval[hm_trials];
  vector[3] ordprob_trial_option1_option2_attribute_status[hm_trials,hm_options,hm_options,hm_attributes];

  //model:
  //setup ppnts
  for(appnt in 1:hm_ppnts){ //to replace with priors?
    calcsd[appnt]=calcsd_level;
    ordsd[appnt]=ordsd_level;//I would really rather merge these two sd params into one, but this version is interesting to dissociate impact of each obs type.
    tolerance[appnt]=0.1;
  }
  //populate estimated attribute values from their priors:
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      for(anattribute in 1:hm_attributes){
	est_trial_option_attribute[atrial,anoption,anattribute]~normal(0,1);
      }
    }
  }
  //apply the calculation observation
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      //this double ~  looks weird if you think of ~ as sampling, but makes sense if you think of it as incrementing target prob, which is what it's actually doing.
      calcobs[atrial,anoption]~normal(truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]],calcsd[ppntid[atrial]]);//calcobs & truth consistency is good
      calcobs[atrial,anoption]~normal(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]],calcsd[ppntid[atrial]]); //calcobs & attribute-estimate consistency is also good.
    }
  }
  
  //apply the ordinal observation:
  for(atrial in 1:hm_trials){
    for(option1 in 2:hm_options){//indexes for lower triangle only: comparisons are directionless & not with self
      for(option2 in 1:(option1-1)){
	for(anattribute in 1:hm_attributes){
	  //prob status '<': phi(-tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1]=normal_cdf(-tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]]);
	  //prob of status '=':phi(tolerance)-phi(-tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,2]=normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]])-ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1];
	  //prob of status '>': 1-phi(tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,3]=1-normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]]);

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

generated quantities{
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

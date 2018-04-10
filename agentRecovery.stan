data{
  int hm_trials; //hm_trials
  int hm_ppnts;
  int ppntid[hm_trials];

  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes]  truth_trial_option_attribute[hm_trials];
  
  int choice[hm_trials]; //ppnt responses!
  
}

parameters{
  //*inferred ppnt parameters*
  simplex[hm_attributes] k[hm_ppnts];//implicit prior is uniform over valid simplexes

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
  for(appnt in 1:hm_ppnts){ //FIXED VALS to replace with distributions...
    calcsd[appnt]=0.2;
    tolerance[appnt]=0.1;
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
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1]=normal_cdf(-tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*calcsd[ppntid[atrial]]);
	  //prob of status '=':phi(tolerance)-phi(-tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,2]=normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*calcsd[ppntid[atrial]])-ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1];
	  //prob of status '>': 1-phi(tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,3]=1-normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*calcsd[ppntid[atrial]]);

	  target += categorical_lpmf(fabs(truth_trial_option_attribute[atrial,option1,anattribute]-truth_trial_option_attribute[atrial,option2,anattribute])<tolerance[ppntid[atrial]] ? 2 : truth_trial_option_attribute[atrial,option1,anattribute]<truth_trial_option_attribute[atrial,option2,anattribute] ? 1 : 3 | ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute]);//Before the pipe: nested ternary if, evaluates to true relation between options 1 & 2 on target attribute {1:'<',2:'=',3:'>'}: after pipe: the probability of each outcome given the current attribute estimates, a vector length 3 over the possible status values {<,=,>}
	}//attribute
      }//option2
    }//option1
  }//trial

  
  //get estimated values & make a choice: in generated block, this is choice generation version.
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estval[atrial,anoption]=est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]];
    }
    choice[atrial]~categorical_logit(estval[atrial]);
  }
  
}//model block

generated quantities{//diagnostics.
  int generated_choice[hm_trials];
  vector[hm_options] estval_tracker[hm_trials];
  vector[hm_options] trueval_tracker[hm_trials];
  
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estval_tracker[atrial,anoption]=est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]];
            trueval_tracker[atrial,anoption]=truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]];
    }
    generated_choice[atrial]=categorical_logit_rng(estval_tracker[atrial]);
  }
}

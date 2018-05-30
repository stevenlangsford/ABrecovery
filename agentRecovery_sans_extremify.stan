data{
  int hm_trials;
  int hm_ppnts;
  int ppntid[hm_trials];

  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes] truth_trial_option_attribute[hm_trials];

  int choice[hm_trials];

  real<lower=0> calcsd[hm_ppnts];
  real<lower=0> ordsd[hm_ppnts];
  real<lower=0> tolerance[hm_ppnts];  
}

parameters{
  //*inferred ppnt parameters*
  simplex[hm_attributes] k[hm_ppnts];//implicit prior is uniform over valid simplexes
  //attribute estimates
  matrix[hm_options,hm_attributes]  est_trial_option_attribute[hm_trials];
  vector[hm_options] calcobs[hm_trials];
  //ordobs appears as a local variable in the model rather than defined in params block (because it takes assignment on the fly, and declaration-assignment need to be in the same block)
}//end parameters

model{
    //local variables:
  vector[hm_options] estval[hm_trials];
  vector[3] ordprob_trial_option1_option2_attribute_status[hm_trials,hm_options,hm_options,hm_attributes]; //where 'status' means <,=,>, coded as 1,2,3
  
  /* for(appnt in 1:hm_ppnts){ */
  /*   calcsd[appnt]~ normal(.5,2); */
  /*   ordsd[appnt]~ normal(.5,2); */
  /*   tolerance[appnt]~normal(0,.5); */
  /* } */

  //model:

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
      calcobs[atrial,anoption]~normal(truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]],calcsd[ppntid[atrial]]);//calcobs & truth consistency is good
      calcobs[atrial,anoption]~normal(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]],calcsd[ppntid[atrial]]); //calcobs & attribute-estimate consistency is also good.
      //together these leave the true attribute values 'communicating' with the estimates indirectly via the calculation observation.
    }
  }
  
  //apply the ordinal observation:
  for(atrial in 1:hm_trials){
    for(option1 in 2:hm_options){//indexes for lower triangle only: comparisons are directionless & not with self
      for(option2 in 1:(option1-1)){
	for(anattribute in 1:hm_attributes){//should consider switching to phi_approx instead of normal_cdf for better numerical performance.
	  //prob status '<': phi(-tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1]=normal_cdf(-tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]]);
	  //prob of status '=':phi(tolerance)-phi(-tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,2]=normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]])-ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1];
	  //prob of status '>': 1-phi(tolerance)
	  ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,3]=1-normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]]);

	  target += categorical_lpmf(fabs(truth_trial_option_attribute[atrial,option1,anattribute]-truth_trial_option_attribute[atrial,option2,anattribute])<tolerance[ppntid[atrial]] ? 2 : truth_trial_option_attribute[atrial,option1,anattribute]<truth_trial_option_attribute[atrial,option2,anattribute] ? 1 : 3 | ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute]);//Before the pipe: nested ternary if, evaluates to true relation between options 1 & 2 on target attribute {1:'<',2:'=',3:'>'}: after pipe: the probability of each outcome given the current attribute estimates, a vector length 3 over the possible status values {<,=,>}. Results in a 'reward' being added to the target variable when attribute estimates are 'more consistent' with the true ordinal relations.
	}//attribute
      }//option2
    }//option1
  }//trial

  //observe a choice
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estval[atrial,anoption]=(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]); //was *10 then ^7 to extremify
    }
    choice[atrial]~categorical_logit(estval[atrial]);
  }
  
}//model block

//generated quantities{
//  int generated_choice[hm_trials];//required: return value.
//  vector[hm_options] estval_tracker[hm_trials];//required to generate choices
//  vector[hm_options] estval_tracker_raw[hm_trials];//diag check
//  vector[hm_options] trueval_tracker[hm_trials];//diag only
//  vector[hm_options] ordprob_tracker[hm_trials,hm_options,hm_options,hm_attributes]; //diag only

  //track the ordinal observation probs
  /* for(atrial in 1:hm_trials){ */
  /*   for(option1 in 1:hm_options){//fill whole array in tracker even though it's redundant/meaningless: otherwise you get an invalid stan object. */
  /*     for(option2 in 1:hm_options){ */
  /* 	for(anattribute in 1:hm_attributes){//should consider switching to phi_approx instead of normal_cdf for better numerical performance. */
  /* 	  //	    -est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]] */
  /* 	  //prob status '<': phi(-tolerance) */
  /* 	  ordprob_tracker[atrial,option1,option2,anattribute,1]=normal_cdf(-tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]]); */
  /* 	  //prob of status '=':phi(tolerance)-phi(-tolerance) */
  /* 	  ordprob_tracker[atrial,option1,option2,anattribute,2]=normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]])-ordprob_tracker[atrial,option1,option2,anattribute,1]; */
  /* 	  //prob of status '>': 1-phi(tolerance) */
  /* 	  ordprob_tracker[atrial,option1,option2,anattribute,3]=1-normal_cdf(tolerance[ppntid[atrial]],est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute],2*ordsd[ppntid[atrial]]); */
  /* 	}//attribute */
  /*     }//option2 */
  /*   }//option1 */
  /* }//trial */

  
//track est and true value to agent, generate a choice from (extremified) est value  
  /* for(atrial in 1:hm_trials){ */
  /*   for(anoption in 1:hm_options){ */
  /*     estval_tracker[atrial,anoption]=(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]*10)^7;//effect of the power is to move softmax towards hard-max, could consider higher powers? Better make them odd though to be sign preserving, estval could be negative. Mult-10 intended to avoid underflow. Is this even legit? what's the better way? */
  /*     estval_tracker_raw[atrial,anoption]=(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]); //check the move towards hardmax is not insane. */
  /*     trueval_tracker[atrial,anoption]=truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]; */
  /*   } */
  /*   generated_choice[atrial]=categorical_logit_rng(estval_tracker[atrial]); */
  /* } */
//}

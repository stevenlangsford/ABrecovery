data{
  int hm_trials;
  int hm_ppnts;
  int ppntid[hm_trials];

  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes] truth_trial_option_attribute[hm_trials];

  real<lower=0> calcsd[hm_ppnts]; //fixed ATM
  real<lower=0> ordsd[hm_ppnts];
  real<lower=0> tolerance[hm_ppnts];  
  
  int choice[hm_trials];
  //****************************************************************************************************//
  int test_hm_trials;
  int test_hm_ppnts;
  int test_ppntid[test_hm_trials];
  matrix[hm_options,hm_attributes] test_truth_trial_option_attribute[test_hm_trials];
}

parameters{
  //*inferred ppnt parameters*
  simplex[hm_attributes] k[hm_ppnts];//shared between original fit and predictions run
  
  //value estimate
  vector[hm_options] estval[hm_trials];
   vector[hm_options] calcobs[hm_trials];
 matrix[hm_options,hm_attributes]  est_trial_option_attribute[hm_trials];
  //estimated for test-set predictions:
  vector[hm_options] test_estval[test_hm_trials];
  matrix[hm_options,hm_attributes] test_est_trial_option_attribute[test_hm_trials];
  vector[hm_options] test_calcobs[test_hm_trials];
}//end parameters

model{
  //new to baseline plus: an ordinal observation:
    vector[3] ordprob_trial_option1_option2_attribute_status[hm_trials,hm_options,hm_options,hm_attributes]; //where 'status' means <,=,>, coded as 1,2,3
    vector[3] test_ordprob_trial_option1_option2_attribute_status[test_hm_trials,hm_options,hm_options,hm_attributes];
    
  //populate estimated attribute values
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      for(anattribute in 1:hm_attributes){
	est_trial_option_attribute[atrial,anoption,anattribute]~normal(0,1); //prior, necessary for compromise effect
	estval[atrial,anoption]~normal(truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]], 0.2);//pin (with some noise) to true attribute values: these are visible to agent-recovery (but not to agents when making decisions). Is this legit? Makes more sense in 'baseline' version without all the other stuff.
      }
    }
  }
  //calc and ord obs:
    for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      //this double ~  looks weird if you think of ~ as sampling, but makes sense if you think of it as incrementing target prob, which is what it's actually doing.
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
    choice[atrial]~categorical_logit(estval[atrial]);
  }

  //REPEAT everything all over again for the test-set predictions, sharing only k estimates
  //populate estimated attribute values
  for(atrial in 1:test_hm_trials){
    for(anoption in 1:hm_options){
      for(anattribute in 1:hm_attributes){
  	test_est_trial_option_attribute[atrial,anoption,anattribute]~normal(0,1); //prior, necessary for compromise effect
  	test_estval[atrial,anoption]~normal(test_truth_trial_option_attribute[atrial,anoption]*k[test_ppntid[atrial]], 0.2);//pin (with some noise) to true attribute values: these are visible to agent-recovery (but not to agents when making decisions). Is this legit? Makes more sense in 'baseline' version without all the other stuff.
      }
    }
  }
  //calc and ord obs:
    for(atrial in 1:test_hm_trials){
    for(anoption in 1:hm_options){
      //this double ~  looks weird if you think of ~ as sampling, but makes sense if you think of it as incrementing target prob, which is what it's actually doing.
      test_calcobs[atrial,anoption]~normal(test_truth_trial_option_attribute[atrial,anoption]*k[test_ppntid[atrial]],calcsd[test_ppntid[atrial]]);//test_calcobs & truth consistency is good
      test_calcobs[atrial,anoption]~normal(test_est_trial_option_attribute[atrial,anoption]*k[test_ppntid[atrial]],calcsd[test_ppntid[atrial]]); //test_calcobs & attribute-estimate consistency is also good.
      //together these leave the true attribute values 'communicating' with the estimates indirectly via the calculation observation.
    }
  }
  
  //apply the ordinal observation:
  for(atrial in 1:test_hm_trials){
    for(option1 in 2:hm_options){//indexes for lower triangle only: comparisons are directionless & not with self
      for(option2 in 1:(option1-1)){
  	for(anattribute in 1:hm_attributes){//should consider switching to phi_approx instead of normal_cdf for better numerical performance.
  	  //prob status '<': phi(-tolerance)
  	  test_ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1]=normal_cdf(-tolerance[test_ppntid[atrial]],test_est_trial_option_attribute[atrial,option1,anattribute]-test_est_trial_option_attribute[atrial,option2,anattribute],ordsd[test_ppntid[atrial]]);
  	  //prob of status '=':phi(tolerance)-phi(-tolerance)
  	  test_ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,2]=normal_cdf(tolerance[test_ppntid[atrial]],test_est_trial_option_attribute[atrial,option1,anattribute]-test_est_trial_option_attribute[atrial,option2,anattribute],ordsd[test_ppntid[atrial]])-test_ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,1];
  	  //prob of status '>': 1-phi(tolerance)
  	  test_ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute,3]=1-normal_cdf(tolerance[test_ppntid[atrial]],test_est_trial_option_attribute[atrial,option1,anattribute]-test_est_trial_option_attribute[atrial,option2,anattribute],ordsd[test_ppntid[atrial]]);

  	  target += categorical_lpmf(fabs(test_truth_trial_option_attribute[atrial,option1,anattribute]-test_truth_trial_option_attribute[atrial,option2,anattribute])<tolerance[test_ppntid[atrial]] ? 2 : test_truth_trial_option_attribute[atrial,option1,anattribute]<test_truth_trial_option_attribute[atrial,option2,anattribute] ? 1 : 3 | test_ordprob_trial_option1_option2_attribute_status[atrial,option1,option2,anattribute]);//Before the pipe: nested ternary if, evaluates to true relation between options 1 & 2 on target attribute {1:'<',2:'=',3:'>'}: after pipe: the probability of each outcome given the current attribute estimates, a vector length 3 over the possible status values {<,=,>}. Results in a 'reward' being added to the target variable when attribute estimates are 'more consistent' with the true ordinal relations.
  	}//attribute
      }//option2
    }//option1
  }//trial
  
}//model block

generated quantities{
 int predictedchoice[test_hm_trials];
  
  for(atrial in 1:test_hm_trials){
   predictedchoice[atrial]=categorical_logit_rng(test_estval[atrial]);
 }
 
}

data{
  int hm_options;
  int hm_attributes;
  int hm_trials;
  
  int hm_reckoning_samples;
  int reckoning_trialnumber[hm_reckoning_samples];
  int reckoning_target[hm_reckoning_samples];
  int reckoning_attribute[hm_reckoning_samples];
  real reckoning_noise[hm_reckoning_samples];
  real reckoning_sample[hm_reckoning_samples];
  
  int hm_comparison_samples;
  int comparison_trialnumber[hm_comparison_samples];
  int comparison_attribute[hm_comparison_samples];
  int comparison_targ1[hm_comparison_samples];
  int comparison_targ2[hm_comparison_samples];
  real comparison_noise[hm_comparison_samples];
  real comparison_sample[hm_comparison_samples];
}

parameters{
  matrix[hm_options,hm_attributes] options[hm_trials];
}
  
model{
  int comparison_consistency[hm_comparison_samples];
  //init from priors
  for(atrial in 1:hm_trials){
    for(i in 1:hm_options){
      //standardized arbitrary attributes:
      for(j in 1:hm_attributes){
      	options[atrial,i,j]~normal(0,1);
      }
    }
  }

  //reckoning observations
  for(i in 1:hm_reckoning_samples){
    reckoning_sample[i] ~ normal(options[reckoning_trialnumber[i],reckoning_target[i],reckoning_attribute[i]],reckoning_noise[i]);
  }

  //comparison observations
  for(i in 1:hm_comparison_samples){
    // comparison_sample[i] ~ normal(options[comparison_trialnumber[i],comparison_targ1[i],comparison_attribute[i]]-options[comparison_targ2[i],comparison_attribute[i]], comparison_noise[i]);
    comparison_consistency[i] = (options[comparison_trialnumber[i],comparison_targ1[i],comparison_attribute[i]]-options[comparison_trialnumber[i],comparison_targ2[i],comparison_attribute[i]] > 0) == (comparison_sample[i] == 1) ? 1 : 0;
    comparison_consistency[i]~bernoulli(.99);//ord obs error rate
  }

  
}//end model

generated quantities{
  vector[hm_options] estval[hm_trials];
  int choice[hm_trials];

  for(atrial in 1:hm_trials){
    for(i in 1:hm_options){
      estval[atrial,i]=1; //init for product version, init to 0 if taking sum.
      for(j in 1:hm_attributes){
	estval[atrial,i]*=options[atrial,i,j];//estimated value is the product of all attributes: here just prob&payoff. Could be a sum if you prefer.
      }
      estval[atrial,i]=(estval[atrial,i]*10)^7;//extremification      
    }
    choice[atrial] = categorical_logit_rng(estval[atrial]);
  }
}

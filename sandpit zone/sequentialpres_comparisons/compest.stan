data{
  int hm_options;
  int hm_attributes;
  
  int hm_reckoning_samples;
  int reckoning_target[hm_reckoning_samples];
  int reckoning_attribute[hm_reckoning_samples];
  real reckoning_noise[hm_reckoning_samples];
  real reckoning_sample[hm_reckoning_samples];
  
  int hm_comparison_samples;
  int comparison_attribute[hm_comparison_samples];
  int comparison_targ1[hm_comparison_samples];
  int comparison_targ2[hm_comparison_samples];
  real comparison_noise[hm_comparison_samples];
  real comparison_sample[hm_comparison_samples];
}

parameters{
  matrix[hm_options,hm_attributes] options;
}
  
model{
  //init from priors
  for(i in 1:hm_options){
    //standardized arbitrary attributes:
      for(j in 1:hm_attributes){
      	options[i,j]~normal(0,1);
      }
  }

  //reckoning observations
  for(i in 1:hm_reckoning_samples){
    reckoning_sample[i] ~ normal(options[reckoning_target[i],reckoning_attribute[i]],reckoning_noise[i]);
  }

  //comparison observations
  for(i in 1:hm_comparison_samples){
    comparison_sample[i] ~ normal(options[comparison_targ1[i],comparison_attribute[i]]-options[comparison_targ2[i],comparison_attribute[i]], comparison_noise[i]);
  }
}

generated quantities{
  vector[hm_options] estval;
  int choice;
  
  for(i in 1:hm_options){
    estval[i]=1; //init for product version, init to 0 if taking sum.
    for(j in 1:hm_attributes){
      estval[i]*=options[i,j];//estimated value is the product of all attributes: here just prob&payoff. Could be a sum if you prefer.
      estval[i]=(estval[i]*10)^7;//extremification
    }
  }
  
  choice = categorical_logit_rng(estval);//non-extremified version.
}

data{
  int hm_trials;
  int hm_ppnts;
  int ppntid[hm_trials];
  
  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes] truth_trial_option_attribute[hm_trials];

  real calcsd_level;
  simplex[hm_attributes] k[hm_ppnts];

  //  int choice[hm_trials];
}

parameters{
  //*inferred ppnt parameters*
  //value estimate
  vector[hm_options] estval[hm_trials];
  //  real alpha; //extremification param?
}//end parameters

model{
  //observe a choice
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estval[atrial,anoption]~normal(truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]], calcsd_level);
    }
  }
}//model block
generated quantities{
  int generated_choice[hm_trials];
  for(atrial in 1:hm_trials){
    generated_choice[atrial]=categorical_logit_rng(estval[atrial]);
  }
}

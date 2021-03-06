data{
  int hm_trials;
  int hm_ppnts;
  int ppntid[hm_trials];

  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes] truth_trial_option_attribute[hm_trials];

  int choice[hm_trials];

  //****************************************************************************************************//
  int test_hm_trials;
  int test_hm_ppnts;
  int test_ppntid[test_hm_trials];
  matrix[hm_options,hm_attributes] test_trial_option_attribute[test_hm_trials];
  
}

parameters{
  //*inferred ppnt parameters*
  simplex[hm_attributes] k[hm_ppnts];//implicit prior is uniform over valid simplexes
  //value estimate
  vector[hm_options] estval[hm_trials];
  //  real alpha; //extremification param?
  vector[hm_options] test_estval[test_hm_trials]; //need to infer this (in the model block), but keep distinct from anything training-data-related.
}//end parameters

model{
  //This part does the fit to the training data:
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estval[atrial,anoption]~normal(truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]], 0.2); //baseline still wants some noise in estval?
    }
    choice[atrial]~categorical_logit(estval[atrial]);
  }

  //this part just does the option value inference bit for the test-set options:
  for(atesttrial in 1:test_hm_trials){
    for(anoption in 1:hm_options){
      test_estval[atesttrial,anoption]~normal(test_trial_option_attribute[atesttrial,anoption]*k[test_ppntid[atesttrial]], 0.2); //k from fit-to-training, other factors from testset.
    }
  }
}//model block

generated quantities{
  int predicted_choice[test_hm_trials];
  for(atrial in 1:test_hm_trials){
    predicted_choice[atrial]=categorical_logit_rng(test_estval[atrial]);

  }
}

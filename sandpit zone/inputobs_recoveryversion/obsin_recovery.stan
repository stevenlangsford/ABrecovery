data{
  int hm_trials;
  int hm_options;
  int hm_attributes;
  
  int choices[hm_trials];
  matrix[hm_options,hm_attributes] option_attribute[hm_trials];

}

parameters{
  real<lower=0> tolerance;
  real<lower=0> noise;
}

model{
  //unobserved agent 'impressions'.
  vector[hm_options] truevalue[hm_trials];
  vector[6] ord_obs[hm_trials]; //attr1{1v2,1v3,2v3} attr2{1v2,1v3,2v3}
  matrix<lower=0>[hm_options,hm_attributes] est_option_attribute[hm_trials];
  vector[hm_options] estvalue[hm_trials];
  //priors
  tolerance~normal(.01,.1); //??
  noise~normal(.5,.1);//??
  for(anoption in 1:hm_options){
    for (anattribute in 1:hm_attributes){
      for(atrial in 1:hm_trials){
	est_option_attribute[] ~ normal(.5,1)//?? prior
	  }
    }
  }
  
  //calculation observation
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      truevalue[anoption]=option_attribute[atrial,anoption,1]*option_attribute[atrial,anoption,2];
      truevalue[atrial,anoption]~N(est_option_attribute[atrial,anoption,1]*est_option_attribute[atrial,anoption,2],noise);
    }    
  }

  //ordinal observation: ??
  //MEH TODO

  //get est value from est attributes (no new inference, just convenient format for making a choice)
    for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estvalue[atrial,anoption]~est_option_attribute[atrial,anoption,1]*est_option_attribute[atrial,anoption,2];
    }    
  }

  //make a choice:
  for(atrial in 1:hm_trials){
    choices[atrial]=categorical_logit_rng(estval[atrial]);
  }
}//end model

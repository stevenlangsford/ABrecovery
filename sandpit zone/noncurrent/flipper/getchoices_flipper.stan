data{
  int hm_trials;
  int hm_ppnts;
  int ppntid[hm_trials];

  int hm_options;
  int hm_attributes;

  matrix[hm_options,hm_attributes] truth_trial_option_attribute[hm_trials];

  //choice generation and agent recovery differ here.
  //fixed when generating choices, otherwise inferred:
  vector[hm_attributes] k[hm_ppnts];
  real calcsd_level; //scalars at the moment (no individual differences), but could easily pass as vectors like for k.
  real orderr;
  real tolerance_level;
  //absent when generating choices, otherwise included:
  //  int choice[hm_trials]; //ppnt responses!
}

transformed data{
  //for agent recovery, move all this stuff to model block and replace assignment with sampling statements.
  real calcsd[hm_ppnts];
  real tolerance[hm_ppnts];
  matrix[hm_options,hm_options] true_relation_trial_attribute_option1_option2[hm_trials,hm_attributes];
  
    //setup ppnts
  for(appnt in 1:hm_ppnts){
    calcsd[appnt]=calcsd_level;//No individual differences ATM, but no barrier to adding them.
    tolerance[appnt]=tolerance_level;
  }

  //  setup true relations
  for(atrial in 1:hm_trials){
    for(option1 in 1:hm_options){
      for(option2 in 1:hm_options){
	for(anattribute in 1:hm_attributes){
	  true_relation_trial_attribute_option1_option2[atrial,anattribute,option1,option2] = fabs(truth_trial_option_attribute[atrial,option1,anattribute]-truth_trial_option_attribute[atrial,option2,anattribute])<tolerance[ppntid[atrial]] ? 2 : truth_trial_option_attribute[atrial,option1,anattribute]<truth_trial_option_attribute[atrial,option2,anattribute] ? 1 : 3;
	}
      }
    }
  }
  
}

parameters{
  //*inferred ppnt parameters*
  //  simplex[hm_attributes] k[hm_ppnts];//implicit prior is uniform over valid simplexes

  //attribute estimates
  matrix[hm_options,hm_attributes]  est_trial_option_attribute[hm_trials];
  vector[hm_options] calcobs[hm_trials];
  //ordobs appears as a local variable in the model rather than defined in params block (because it takes assignment on the fly, and declaration-assignment need to be in the same block)
}//end parameters

model{
  //local variables:
  vector[hm_options] estval[hm_trials];//for calc obs
    int relationcounter;
    int relationsuccess[hm_trials,hm_options*(hm_options-1)/2*hm_attributes];
  
    //populate estimated attribute values from their priors:
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      for(anattribute in 1:hm_attributes){
	est_trial_option_attribute[atrial,anoption,anattribute]~normal(0,1);
      }
    }
  }


  //These are the est relations successes and failures from the current attribute estimates:
  for(atrial in 1:hm_trials){
    relationcounter=0;
    for(option1 in 2:hm_options){
      for(option2 in 1:(option1-1)){//only compare in one direction, no self comparisons.
	for(anattribute in 1:hm_attributes){
	  relationcounter=relationcounter+1;
	  relationsuccess[atrial,relationcounter]=((fabs(est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute])<tolerance[ppntid[atrial]] ? 2 : est_trial_option_attribute[atrial,option1,anattribute]<est_trial_option_attribute[atrial,option2,anattribute] ? 1 : 3)==true_relation_trial_attribute_option1_option2[atrial,anattribute,option1,option2]) ? 1 : 0;
	  
	}//end attribute
      }//end option1
    }//end option2
    
    /* print("trial"); */

    target += bernoulli_lpmf(relationsuccess[atrial] | 1-orderr); 
  }   //end atrial
    
   //model:
  //apply the calculation observation
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      //this double ~  looks weird if you think of ~ as sampling, but makes sense if you think of it as incrementing target prob, which is what it's actually doing.
      calcobs[atrial,anoption]~normal(truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]],calcsd[ppntid[atrial]]);//calcobs & truth consistency is good
      calcobs[atrial,anoption]~normal(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]],calcsd[ppntid[atrial]]); //calcobs & attribute-estimate consistency is also good.
      //together these leave the true attribute values 'communicating' with the estimates indirectly via the calculation observation.
    }
  }
  
  //in recovery version, observe a choice here .This is the choice generation version, choice generation happens in generated quantities block.
  /* for(atrial in 1:hm_trials){ */
  /*   for(anoption in 1:hm_options){ */
  /*     estval[atrial,anoption]=est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]; */
  /*   } */
  /*   choice[atrial]~categorical_logit(estval[atrial]); */
  /* } */

}//model block

generated quantities{
  int generated_choice[hm_trials];//required: return value.
  vector[hm_options] estval_tracker[hm_trials];//required to generate choices
  vector[hm_options] estval_tracker_raw[hm_trials];//diag check
  vector[hm_options] trueval_tracker[hm_trials];//diag only
  int relation_tracker[hm_trials,hm_options*(hm_options-1)/2*hm_attributes];
  int relationcounter;
  
//track est and true value to agent, generate a choice from (extremified) est value  
  for(atrial in 1:hm_trials){
    for(anoption in 1:hm_options){
      estval_tracker[atrial,anoption]=(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]*10)^7;//effect of the power is to move softmax towards hard-max, could consider higher powers? Better make them odd though to be sign preserving, estval could be negative. Mult-10 intended to avoid underflow, values are typically <|1|, and collapse to zero if used raw. Is this even legit? what's the better way?
      estval_tracker_raw[atrial,anoption]=(est_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]]); //check the move towards hardmax is not insane.
      trueval_tracker[atrial,anoption]=truth_trial_option_attribute[atrial,anoption]*k[ppntid[atrial]];
    }
    generated_choice[atrial]=categorical_logit_rng(estval_tracker[atrial]);
  }

  //track ord obs success
    for(atrial in 1:hm_trials){
    relationcounter=0;
    for(option1 in 2:hm_options){
      for(option2 in 1:(option1-1)){//only compare in one direction, no self comparisons.
	for(anattribute in 1:hm_attributes){
	  relationcounter=relationcounter+1;
	  relation_tracker[atrial,relationcounter]=((fabs(est_trial_option_attribute[atrial,option1,anattribute]-est_trial_option_attribute[atrial,option2,anattribute])<tolerance[ppntid[atrial]] ? 2 : est_trial_option_attribute[atrial,option1,anattribute]<est_trial_option_attribute[atrial,option2,anattribute] ? 1 : 3)==true_relation_trial_attribute_option1_option2[atrial,anattribute,option1,option2]) ? 1 : 0;
	  
	  /* print("trial ",atrial, " relationindex ", relationcounter, " is option1 ",option1," vs option2 ",option2, " attribute ",anattribute); */
	  /* print("est:",est_trial_option_attribute[atrial,option1,anattribute], "vs", est_trial_option_attribute[atrial,option2,anattribute], */
	  /* 	"|| truth ", truth_trial_option_attribute[atrial,option1,anattribute],"vs",truth_trial_option_attribute[atrial,option2,anattribute]); */
	  /* print("success: ",relation_tracker[atrial,relationcounter]) */
	  
	}//end attribute
      }//end option1
    }//end option2
    
    /* print("trial"); */

    //    relationsuccess[atrial]~bernoulli(1-orderr); 
  }   //end atrial

  
}

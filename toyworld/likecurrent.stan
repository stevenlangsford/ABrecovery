data{
  int n;
  real x1[n];
  real x2[n];
  
  real tolerance;
  real calcnoise;
  real orderr;
}

transformed data{
  real mysum[n];
  int ord[n];

  for(i in 1:n){
    mysum[i]=x1[i]+x2[i];
    ord[i] = (fabs(x1[i]-x2[i]) < tolerance) ? 2 : x1[i]<x2[i] ? 1 : 3;
  }
  
}

parameters{
  real x1_est[n];
  real x2_est[n];
}

model{
  vector[3] ordprobs[n];

  //priors
  x1_est~normal(0,1);
  x2_est~normal(0,1);

  for(i in 1:n){
    mysum[i]~normal(x1_est[i]+x2_est[i],calcnoise);//dead-reckoning estimate, subject to some noise.

    //ordinal observation
    ordprobs[i,1]=normal_cdf(-tolerance,x1_est[i]-x2_est[i],orderr);
	  //prob of status '=':phi(tolerance)-phi(-tolerance)
    ordprobs[i,2]=normal_cdf(tolerance,x1_est[i]-x2_est[i],orderr)-ordprobs[i,1];
	  //prob of status '>': 1-phi(tolerance)
    ordprobs[i,3]=1-normal_cdf(tolerance,x1_est[i]-x2_est[i],orderr);
    
    target += categorical_lpmf(fabs(x1[i]-x2[i])<tolerance ? 2 : x1[i] < x2[i] ? 1 : 3 | ordprobs[i]);
  }
}

generated quantities{
    vector[3] ordprob_tracker [n];
    
      for(i in 1:n){
    //ordinal observation
    ordprob_tracker[i,1]=normal_cdf(-tolerance,x1_est[i]-x2_est[i],orderr);
	  //prob of status '=':phi(tolerance)-phi(-tolerance)
    ordprob_tracker[i,2]=normal_cdf(tolerance,x1_est[i]-x2_est[i],orderr)-ordprob_tracker[i,1];
	  //prob of status '>': 1-phi(tolerance)
    ordprob_tracker[i,3]=1-normal_cdf(tolerance,x1_est[i]-x2_est[i],orderr);    
  }
}

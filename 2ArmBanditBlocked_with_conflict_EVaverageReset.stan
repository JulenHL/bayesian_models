data {
// this section reads in the data that is piped through the r-command, and is only executed once
  int<lower=1> N; #where N is the number of subjects
  int<lower=1> T; #max number of trials per subject
  int<lower=1> TBlock; #number of trials before new symbols are used
  int<lower=1,upper=T> Tsubj[N]; #vector containing the number of trials for each subject
 
 #data for conflict paradigm(M)
    int<lower=1,upper=2> choiceC[N,T]; #Matrix of N subjects x T trials of 1 and 2 indicating whether participants chose option 1 or 2 on every trial
  real rewlosCM[N,T];  # Matrix of N subjects x T trials with no lower and upper bounds, indicating the Money reward or loss incurred on ever trial.   
    real rewlosCS[N,T];  # Matrix of N subjects x T trials with no lower and upper bounds, indicating the Shock incurred on ever trial.   


}

transformed data {
// this section hard codes the initial value of the EV for the two options to zero
  vector[2] initV;  # initial values for EV
  initV = rep_vector(0.0, 2); # rep_vector(0.0,2) creates a vector c(0,0), i.e. zeros the EV for both options
}
// This section defines each subjects learning rate (A) and temperature (tau) as the sum of the population mean (mu_p[1] for learning rate A Money and mu_p[2] for the temperature and mu_p[3] for learning rate A for shock) plus a subject specific deviation from that mean. This deviation is obtained using a scaling factor sigma[1] for A and sigma[2] for tau and sigma[3] for learning rate shock and a N(0,1) subject specific raw offset (A_pr[N] for money learning rate and tau_pr[N] for temperature and B_pr[N] for the shock). Note that the parameter section contains those parameters that STAN will really estimate, i.e. the ones you are interested in. The transformed parameter section that can contain statements, is used to generate the actual A and tau, that have been transformed to 0-1 using Phi_approx and will be used to perform the categorical_logit. 

parameters {


# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters  
  vector[3] mu_p;  
  vector<lower=0>[3] sigma;
    
  # Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;    # learning rate for money
  vector[N] tau_pr;  # inverse temperature
  vector[N] B_pr;    # learning rate for shock
  

}

transformed parameters {
  # subject-level parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=5>[N] tau;
  vector<lower=0,upper=1>[N] B;

  
  for (i in 1:N) {
    A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );
    tau[i] = Phi_approx( mu_p[2] + sigma[2] * tau_pr[i] ) * 5;
    B[i]   = Phi_approx( mu_p[3]  + sigma[3]  * B_pr[i] );
  }
}

model {
  # Hyperparameters
  mu_p  ~ normal(0, 1); # priors on the group learning rate(mu_p[1] and mu_p[3]) and temperature (mu_p[2])
  sigma ~ cauchy(0, 5); # prior on the standard deviation of the individual differences in A, B and tau
  
  # individual parameters
  A_pr   ~ normal(0,1); #priors on individual deviations, but this will be scaled with sigma[1]
  tau_pr ~ normal(0,1); #priors on individual deviation, but this will be scaled with sigma[2]
  B_pr   ~ normal(0,1); #priors on individual deviations, but this will be scaled with sigma[3]
  
  # subject loop and trial loop
  for (i in 1:N) { #subject loop with i indexing subject
  

###### Variables for the conflict only model 
    vector[2] evM; # declares expected value for the two alternatives, hence [2], as a local variable
    vector[2] evS; # declares expected value for the two alternatives, hence [2], as a local variable
    real PEM;      # declares prediction error as a local variable. There is only one PE per trial hence no [2]
    real PES;      # declares prediction error as a local variable. There is only one PE per trial hence no [2]
    real avEVM;
    real avEVS;


      ###### the conflict only model 

    evM = initV; # setting both to zero before going through the trials
    evS = initV; # setting both to zero before going through the trials

    for (t in 1:(Tsubj[i])) { #sets a loop going through Tsubj=number of trials of that participant 
      if ((t % TBlock)==1) {   #operator% in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc 
        avEVM=(evM[1]+evM[2])/2;
        evM[1]=avEVM;evM[2]=avEVM;
        avEVS=(evS[1]+evS[2])/2;
        evS[1]=avEVS;evS[2]=avEVS;
        
      }
      choiceC[i,t] ~ categorical_logit( tau[i] * (evM+evS) ); # this establishes that the actual choice (as provided in data) should follow the caterogiral logit regression with temperature * expected values. 

      # prediction error 
      PEM = rewlosCM[i,t] - evM[choiceC[i,t]]; #i.e. actual reward or loss – expected value of the choice.
      PES = rewlosCS[i,t] - evS[choiceC[i,t]]; #i.e. actual reward or loss – expected value of the choice.

      # value updating (learning) 
      evM[choiceC[i,t]] = evM[choiceC[i,t]] + A[i] * PE; # this now updates the expected value based on learning rate
      evS[choiceC[i,t]] = evS[choiceC[i,t]] + B[i] * PE; # this now updates the expected value based on learning rate
    }

  }
}

generated quantities {

// for each sampling this will generate the actual values to use as output
// this includes the population A and tau, that are properly scaled, as well as a cumulative log_lik over all trails for each participant. 

  # For group level parameters
  real<lower=0,upper=1> mu_A; 
  real<lower=0,upper=5> mu_tau;
    real<lower=0,upper=1> mu_B; 

  
  # For log likelihood calculation
  real log_likC[N]; # for the conflict choices


  mu_A   = Phi_approx(mu_p[1]);
  mu_tau = Phi_approx(mu_p[2]) * 5;
  mu_B   = Phi_approx(mu_p[3]);


  { # local section, this saves time and space
    for (i in 1:N) {
 
 # declaring variables for the money only model first, then for the money and shock conflict
 
      vector[2] evM; # expected value for money 
      real PEM;      # prediction error
      vector[2] evS; # expected value for shock
      real PES;      # prediction error
      real avEV;
      real avEVM;
      real avEVS;

# Define the conflict model 

      # Initialize values
      evM = initV;
      evS = initV;
      
      log_likC[i] = 0;
      
      for (t in 1:(Tsubj[i])) {
        if ((t % TBlock)==1) {   #operator% in stan is modulus, and 1 mod 15 = 1 , 15 mod 15 = 0, 16 mod 15 = 1 etc 
        avEVM=(evM[1]+evM[2])/2;
        evM[1]=avEVM;evM[2]=avEVM;
        avEVS=(evS[1]+evS[2])/2;
        evS[1]=avEVS;evS[2]=avEVS;
        
      }
       
        # compute action probabilities
        log_likC[i] = log_likC[i] + categorical_logit_lpmf(choiceC[i,t] | tau[i] * (evM+evS));
        
        # prediction error 
        PEM = rewlosCM[i,t] - evM[choiceC[i,t]];
        PES = rewlosCS[i,t] - evS[choiceC[i,t]];


        # value updating (learning) 
        evM[choiceC[i,t]] = evM[choiceC[i,t]] + A[i] * PEM; 
        evS[choiceC[i,t]] = evS[choiceC[i,t]] + B[i] * PES; 
      }

    }   
  }
}


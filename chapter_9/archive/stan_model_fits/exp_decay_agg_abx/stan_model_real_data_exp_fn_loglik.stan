// Stan model for msm style interval censored model, on real data, exponential function
      
      functions {
      
      // Differential state equations fer solving    
      
      real[] twostateODE(real t,        // time
      real[] y,      // state
      real[] theta,  // parameters
      real[] x_r,    // data (real)
      int[] x_i) {   // data (integer) 
      
      real dydt[2]; 
      real lambda;
      real mu;
      real ab_alpha0;
      real ab_beta0;
      real hosp_alpha1;
      real hosp_beta1;
      real gamma;
      
      real lambda_beta_sum;
      real mu_alpha_sum;
      
      lambda= theta[1] ;
      mu = theta[2];
      ab_alpha0 = theta[3];
      ab_beta0 = theta[4];
      hosp_alpha1 = theta[5];
      hosp_beta1 = theta[6];
      gamma = theta[7];
      
      lambda_beta_sum = 0;
      mu_alpha_sum = 0;
      
      // first coef, abx, start x_r[1] and end time x_r[2]
      // ab_flags are x_i: ab_prev_step, ab_this_step
      // ab_prev_start time is x_r[5]
      
      if (x_i[1] == 0) {      // if the ab_prev_step flag is 0
      
      if (x_r[1] == 999) {
      // don't do anything, there is nothing for this covariate
      } else if (t <= x_r[2] && t >= x_r[1]) {
      lambda_beta_sum = lambda_beta_sum + ab_beta0;
      mu_alpha_sum = mu_alpha_sum + ab_alpha0;
      } else if (t > x_r[2]) {
      lambda_beta_sum = lambda_beta_sum + ab_beta0*exp((t-x_r[2])/(-2*gamma));
      mu_alpha_sum = mu_alpha_sum + ab_alpha0*exp((t-x_r[2])/(-2*gamma));
      }
      
      } else if (x_i[1] == 1) {   // if the ab_prev step flag is 1
      
      if (x_i[2] == 0) {      // if the ab_current_step flag is 0
      
      lambda_beta_sum = lambda_beta_sum + ab_beta0*exp((t-x_r[5])/(-2*gamma));
      mu_alpha_sum = mu_alpha_sum + ab_alpha0*exp((t-x_r[5])/(-2*gamma));
      
      } else if (x_i[2] == 1) { // if the ab_current step flag is 1
      
      if (t < x_r[1]) {
      lambda_beta_sum = lambda_beta_sum + ab_beta0*exp((t-x_r[5])/(-2*gamma));
      mu_alpha_sum = mu_alpha_sum + ab_alpha0*exp((t-x_r[5])/(-2*gamma));
      } else if (t <= x_r[2] && t >= x_r[1]) {
      lambda_beta_sum = lambda_beta_sum + ab_beta0;
      mu_alpha_sum = mu_alpha_sum + ab_alpha0;
      } else if (t > x_r[2]) {
      lambda_beta_sum = lambda_beta_sum + ab_beta0*exp((t-x_r[2])/(-2*gamma));
      mu_alpha_sum = mu_alpha_sum + ab_alpha0*exp((t-x_r[2])/(-2*gamma));
      }
      
      }
      }
      
      // second coef coef, abx, start x_r[3] and end time x_r[4]
      
      
      if (x_r[3] == 999) {
      // don't do anything, there is nothing for this covariate
      } else if (t <= x_r[4] && t >= x_r[3]) {
      lambda_beta_sum = lambda_beta_sum + hosp_beta1;
      mu_alpha_sum = mu_alpha_sum + hosp_alpha1;
      }      
      
      
      dydt[1] = -y[1]*lambda*exp(lambda_beta_sum) + y[2]*mu*exp(mu_alpha_sum);
      dydt[2] = y[1]*lambda*exp(lambda_beta_sum) - y[2]*mu*exp(mu_alpha_sum);     
      
      
      return dydt;
      } // end of function
      } // end of block
      
      data {
      int < lower = 1 > N; // Sample size
      real t[N]; // end time
      real start_state[N,2]; // start state (at t_start) in form [p0,p1]
      int end_state[N];   // end state (at t) as integer
      real covariates[N,5]; // covariate start and end times (as ab_start, ab_end, hosp_start, hosp_end, prev_ab_start)
      int ab_flags[N,2]; // ab flags as (ab_prev_step, ab_this_step)
      }
      
      transformed data {
      // real x_r[0];
      // int x_i[0];
      
      }
      
      parameters {
      real < lower = 0 > lambda;
      real < lower = 0 > mu;
      real ab_alpha0;
      real ab_beta0;
      real hosp_alpha1;
      real hosp_beta1;
      real < lower = 0 > gamma;
      
      }
      
      transformed parameters {
      real theta[7];
      theta[1] = lambda;
      theta[2] = mu;
      theta[3] = ab_alpha0;
      theta[4] = ab_beta0;
      theta[5] = hosp_alpha1;
      theta[6] = hosp_beta1;
      theta[7] = gamma;
      }
      
      model {
      real temp[1,2];
      lambda ~ normal(0,0.2);
      mu ~ normal(0,0.2);
      ab_alpha0 ~ normal(0,2);
      ab_beta0 ~ normal(0,2);
      hosp_alpha1 ~ normal(0,2);
      hosp_beta1 ~ normal(0,2);
      gamma ~ normal(0,50);
      
      for (n in 1:N) {
      temp = integrate_ode_rk45(twostateODE, start_state[n], 0, t[n:n], theta, covariates[n], ab_flags[n], 1E-6,1E-6, 1E6);
      if (end_state[n] == 1) {
      target += log(temp[1,2]);
      } else {
      target += log(temp[1,1]);
      }
      }
      }
      
      generated quantities {
      vector[N] log_lik;
      real temp[1,2];
      for (n in 1:N) {
      temp = integrate_ode_rk45(twostateODE, start_state[n], 0, t[n:n], theta, covariates[n], ab_flags[n], 1E-6,1E-6, 1E6);
      if (end_state[n] == 1) {
      log_lik[n] = log(temp[1,2]);
      } else {
      log_lik[n] = log(temp[1,1]);
      }
      }
      }  
      
      // The posterior predictive distribution

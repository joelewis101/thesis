// Stan model for msm style interval censored model, on real data, exponential function
      
      functions {
      
      // Time varying covariate value calculation
      // Needs to be passed a 1d array of covariates 
      // each 3 entries are  (cov_start_time, cov_end_time, prev_cov_end_time)
      // prev_cov_end_time is coded as
      // t of prev cov end time if has been exposure
      // Needs to return a matrix with n_cov rows and 1 column
      // to act on the alphas and betas of the model
      
      // cov matrix should be
      // col 1 = start time
      // col 2 = end time
      // row 3 = prev exp
      
      // n_covs is an array with integer for each cov
      // 1 = not tme varying and coded with prev time- present if > 0 and absent of < 0
      // 2 = time varying but no decay; prev time is ignored
      // 3 = time varying with decay. If there is no exposure in this block, set stop_time to < 0
      real[] return_time_varying_coefs_exp_flat(
         real[] cov_mat_passed,
         real t1,
         int[] n_covs_passed,
         real[] gamma_passed
      ) {
         real out_vars[size(n_covs_passed)];
         int s;
         int f;
         int p;
        // matrix[n_covs,3] cov_mat;
         //print(cov_1d_array)
        // cov_mat = to_matrix(cov_1d_array,n_covs,3,0);
         
        // print(cov_mat)
         
         for (n in 1:size(n_covs_passed)) {
             s = 1 + ((n-1)*3);
             f = s + 1;  
             p = f + 1;
               // for each row in cov matrix (ie each covariate)
              if (n_covs_passed[n] == 3) {
               // gamma decay  
                 if (cov_mat_passed[f] > 0) {   //if there is exposure of this covariate in this block
                     
               if (t1 <= cov_mat_passed[f] && t1 >= cov_mat_passed[s]) {
                    // if exposure is happening now
                    // set value to 1
               out_vars[n] = 1;
               } else if (t1 > cov_mat_passed[f]) {
                    // otherwise if there is exposure in this block
                    // and this covariate is set to have a decaying effect
                    // and time is after it has stopped
                    // set value to decay from stop time
               out_vars[n] = exp((t1-cov_mat_passed[f])/(-2*gamma_passed[n]));
                
               } else if (t1 < cov_mat_passed[s] && cov_mat_passed[p] < 0) {
                    // otherwise, if time is before start time
                    // and there is previous exposure
                    // set value to decay from previous time
               out_vars[n] = exp((t1-cov_mat_passed[p])/(-2*gamma_passed[n]));
               } else {
                     //  otherwise set to 0
                    out_vars[n] = 0; 
               }
                 } else {   // if there is no exposure in this block
                   if (cov_mat_passed[p] < 0) {    // if there is previous exposure
                     out_vars[n] = exp((t1-cov_mat_passed[p])/(-2*gamma_passed[n]));
                   } else {
                     out_vars[n] = 0;
                   }
                 }
               
              } else if (n_covs_passed[n] == 2) {

               if (t1 <= cov_mat_passed[f] && t1 >= cov_mat_passed[s]) {
                    // if exposure is happening now
                    // set value to 1
               out_vars[n] = 1;
               } else {
                    out_vars[n] = 0; 
               }
               
              } else if (n_covs_passed[n] == 1) {
                if (cov_mat_passed[p] > 0) {
                   out_vars[n] = 1;
                } else {
                   out_vars[n] = 0;
                }
                 
              }
              // remember s = start time, f = finish, p = previous
           
         }  // end of for loop
         return out_vars;
         } // end of fn
         
      // function to return lambda(t) and mu(t) 
      // this should take a vector of length n_cov of time
      // varying coefficients of the betas (vrom the time varying coef fn)
      // and two vectors of length n_cov of parameters
      //the alphas (that act on mu)
      // and the betas (that act on lambda)
      // and return a vect or of length two for the 
      // values of lamba(t) and mu(t)
      
    //  real[] return_time_var_transition_hazard(
   //      real
    //  ) 
      
      // differential state equation
      
      real[] twostateODE2_flat(real t,   // time
      real[] y,                     // state
      real[] theta,                 // parameters
      real[] x_r,                   //data
      int[] x_i) {                 // data
         
         // y is state as [p0,p1]
         // theta defined as 
         // [ lambda, mu, gamma0, ... gamman,
         //  alpha0, alpha1, ... alphan,
         //  beta0 ... betan ]
         // where n is number of covariateseode
         // data x_r is 1d array of covariates, 3 for each covariate
         // x_i is array of covariate type as
          // [number of non-timedep var, number of timedep nongamma var, number of gamma var,
          // then an integer for each cov 1,2 or 3]
         
         real dydt[2];
         real coefs[size(x_i[])-3];  //vector of coefs
         real alphaz[size(x_i[])-3]; // vector of alphas
         real betaz[size(x_i[])-3]; // vector of betas
         real gammaz[x_i[3]];
         real lambda_pr;
         real mu_pr;
         real lambda0;
         real mu0;
         lambda0 = theta[1];
         mu0 = theta[2];
      gammaz =   theta[3:(2+ x_i[3])];
      alphaz = theta[(3+ x_i[3]):(3+x_i[3] + x_i[1] + x_i[2] + x_i[3] -1)] ;
      betaz = theta[(3+x_i[3] + x_i[1] + x_i[2] + x_i[3]):(2+x_i[3] + 2*(x_i[1] + x_i[2] + x_i[3]))];
         
       coefs = return_time_varying_coefs_exp_flat(x_r, t, x_i[4:size(x_i)], gammaz);
         lambda_pr = lambda0*exp(dot_product(coefs, betaz));
         mu_pr = mu0*exp(dot_product(coefs, alphaz));
         
      dydt[1] = -y[1]*lambda_pr + y[2]*mu_pr;
      dydt[2] = y[1]*lambda_pr - y[2]*mu_pr;   
         return dydt;
         
      }



}

  data {
      int < lower = 1 > N; // Number of segments

      int <lower = 0> n_covs[3];  //[nontimevary, timevarynogamma, timevarygamma]
      int covs_type[sum(n_covs)];
      real t[N]; // end time
      real cov_mat[N,sum(n_covs[])*3]; // array of covariates, first 3 are no of non tmevar, timevarm timevar_gamma covs
      real start_state[N,2]; // start state (at t=0) in form [p0,p1]
      int end_state[N];   // end state (at t) as integer
      }

      transformed data {
         int x_i_pass[3 + sum(n_covs)];
         x_i_pass[] = append_array(n_covs[], covs_type[]);
      }
      
      parameters {
      real < lower = 0 > lambda;
      real < lower = 0 > mu;
      real <lower = 0> gammas[n_covs[3]];
      real alphas[sum(n_covs[])];
      real betas[sum(n_covs[])];
      }
      
      transformed parameters {
      
      real theta[2 + 2*(sum(n_covs)) + n_covs[3]];
      theta[1] = lambda;
      theta[2] = mu;
      theta[3:(2+ n_covs[3])] = gammas[];
      theta[(3+ n_covs[3]):(3+n_covs[3] + sum(n_covs) -1)] = alphas[];
      theta[(3+n_covs[3] + sum(n_covs)):(2+n_covs[3] + 2*(sum(n_covs)))] = betas[];
      }
      
 model {
      real temp[1,2];
      lambda ~ normal(0,0.2);
      mu ~ normal(0,0.2);
      alphas ~ normal(0,2);
      betas~ normal(0,2);
      gammas ~ normal(0,50);
      
      
      
      for (n in 1:N) {
         // flatten covariates to pass to ode solver
     // covariates = to_array_1d(to_matrix(cov_mat[n,,]));
      
     temp = integrate_ode_rk45(twostateODE2_flat, start_state[n], 0, t[n:n], theta[], cov_mat[n], x_i_pass[], 1e-6,1e-6,1e6);
     
      if (end_state[n] == 1) {
      target += log(temp[1,2]);
      } else {
      target += log(temp[1,1]);
      }
      }
      }
      
      
      generated quantities {
   //   vector[N] log_lik;
//      real temp[1,2];
 //     for (n in 1:N) {
//       //covariates = to_array_1d(to_matrix(cov_mat[n,,])); 
 //        temp = integrate_ode_rk45(twostateODE2, start_state[n], 0, t[n:n], theta, cov_mat[n], temp_ncovs[], 1E-6,1E-6, 1E6);
  //    if (end_state[n] == 1) {
   //   log_lik[n] = log(temp[1,2]);
    //  } else {
   //   log_lik[n] = log(temp[1,1]);
   //   }
    //  }
      }  






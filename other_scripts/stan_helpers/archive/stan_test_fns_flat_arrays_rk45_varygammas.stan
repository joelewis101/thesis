// Stan model for msm style interval censored model, on real data, exponential function
      
      functions {
      
      // Time varying covariate value calculation
      // Needs to be passed a 1d array of covariates 
      // each 3 entries are  (cov_start_time, cov_end_time, prev_cov_end_time)
      // prev_cov_end_time is coded as
      // t of prev cov end time if has been exposure
      // 0 if no exposure yet
      // 999 if no exp decay for that variable
      // Needs to return a matrix with n_cov rows and 1 column
      // to act on the alphas and betas of the model
      
      // cov matrix should be
      // col 1 = start time
      // col 2 = end time
      // row 3 = prev exp
      
      real[] return_time_varying_coefs_exp_flat(
         real[] cov_mat,
         real t,
         int n_covs,
         real[] gamma
      ) {
         real out_vars[n_covs];
         int s;
         int f;
         int p;
        // matrix[n_covs,3] cov_mat;
         //print(cov_1d_array)
        // cov_mat = to_matrix(cov_1d_array,n_covs,3,0);
         
        // print(cov_mat)
         
         for (n in 1:n_covs) {
             s = 1 + ((n-1)*3);
             f = s + 1;  
             p = f + 1;
               // for each row in cov matrix (ie each covariate)
              
              // remember s = start time, f = finish, p = previous
             if (cov_mat[f] != 999) {   //if there is exposure of this covariate in this block
                     
               if (t <= cov_mat[f] && t >= cov_mat[s]) {
                    // if exposure is happening now
                    // set value to 1
               out_vars[n] = 1;
               } else if (t > cov_mat[f] && cov_mat[p] != 999) {
                    // otherwise if there is exposure in this block
                    // and this covariate is set to have a decaying effect
                    // and time is after it has stopped
                    // set value to decay from stop time
               out_vars[n] = exp(-(t-cov_mat[f])*gamma[n]);
                
               } else if (t < cov_mat[s] && cov_mat[p] < 0) {
                    // otherwise, if time is before start time
                    // and there is previous exposure
                    // set value to decay from previous time
               out_vars[n] = exp(-(t-cov_mat[p])*gamma[n]);
               } else {
                     //  otherwise set to 0
                    out_vars[n] = 0; 
               }
              
            } else {  // if there is no exposure in this block
               if (cov_mat[p] < 0) {
                     // if there has been previous exposure, set value to decaying value
                  out_vars[n] = exp(-(t - cov_mat[p])*gamma[n]);
               } else {
                     // if not, set to zero
                  out_vars[n] = 0;
               }
            } // end of big if-then
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
         
         // theta defined as 
         // [ lambda, mu, gamma
         //  alpha0, alpha1, ... alphan,
         //  beta0 ... betan ]
         // where n is number of covariates
         // data x_r is 1d array of covariates
         // x_i is number of covariates
         
         real dydt[2];
         real coefs[x_i[1]];  //vector of coefs
         real alphas[x_i[1]]; // vector of alphas
         real betas[x_i[1]]; // vector of betas
         real gammas[x_i[1]];
         real lambda_pr;
         real mu_pr;
         real lambda0;
         real mu0;
        // real x_r2[3,x_i[1]];
         
       //  print("meh")
         lambda0 = theta[1];
         mu0 = theta[2];
  
         gammas = theta[3:(2+ x_i[1])] ;
      
       alphas[] = theta[(3+ x_i[1]):(2+x_i[1]*2)] ;
       betas[] = theta[(3+x_i[1]*2):(2+x_i[1]*3)];
         
       coefs = return_time_varying_coefs_exp_flat(x_r, t, x_i[1], gammas);
         
         lambda_pr = lambda0*exp(dot_product(coefs, betas));
         mu_pr = mu0*exp(dot_product(coefs, alphas));
         
      dydt[1] = -y[1]*lambda_pr + y[2]*mu_pr;
      dydt[2] = y[1]*lambda_pr - y[2]*mu_pr;   
         return dydt;
         
      }



}

  data {
      int < lower = 1 > N; // Number of segments
      int< lower = 0 > n_covs; // number of covariates
      real t[N]; // end time
      real cov_mat[N,3*n_covs]; // array of covariates
                                 //  [N,,] number of segments
                                 // [,3,] cov start time, cov end time, cov prev start time
                                 // set cov prev start time 999 if no decay
                                 // and 0 if no exposure yet
                                 // [,,n_covs] one for each covariate
      real start_state[N,2]; // start state (at t=0) in form [p0,p1]
      int end_state[N];   // end state (at t) as integer
      }

      transformed data {
      // real x_r[0];
      // int x_i[0];
      int temp_ncovs[1];
      temp_ncovs[1] = n_covs;
      }
      
      parameters {
      real < lower = 0 > lambda;
      real < lower = 0 > mu;
      real <lower = 0> gammas[n_covs];
      real alphas[n_covs];
      real betas[n_covs];
      
            
      }
      
      transformed parameters {
      real theta[n_covs*3 + 2];
      theta[1] = lambda;
      theta[2] = mu;
      theta[3:(2+ n_covs)] = gammas[];
      
      theta[(3+ n_covs):(2+n_covs*2)] = alphas[];
      theta[(3+n_covs*2):(2+n_covs*3)] = betas[];
      }
      
 model {
      real temp[1,2];
      lambda ~ normal(0,2);
      mu ~ normal(0,2);
      alphas ~ normal(0,2);
      betas~ normal(0,2);
      gammas ~ normal(0,0.2);
      
      
      
      for (n in 1:N) {
         // flatten covariates to pass to ode solver
     // covariates = to_array_1d(to_matrix(cov_mat[n,,]));
      
     temp = integrate_ode_rk45(twostateODE2_flat, start_state[n], 0, t[n:n], theta[], cov_mat[n], temp_ncovs[], 1e-7,1e-5,1e6);
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






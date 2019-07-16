// Stan model for msm style interval censored model, on real data, exponential function
      
      functions {
      
      // Time varying covariate value calculation
      // Needs to be passed a matrix of covariates and time
      // 3 columns (cov_start_time, cov_end_time, prev_cov_end_time)
      // prev_cov_end_time is coded as
      // t of prev cov end time if has been exposure
      // 0 if no exposure yet
      // 999 if no exp decay for that variable
      // Needs to return a matrix with n_cov rows and 1 column
      // to act on the alphas and betas of the model
      
      // cov matrix should be
      // row 1 = start time
      // row 2 = end time
      // row 3 = prev exp
      
      real[] return_time_varying_coefs_exp(
         real[] cov_1d_array,
         real t,
         int n_covs,
         real gamma
      ) {
         real out_vars[n_covs];
         real cov_mat[n_covs,3];
         cov_mat = to_array_2d(to_matrix(cov_1d_array,n_covs,3,0));
         
         for (n in 1:n_covs) {
               // for each row in cov matrix (ie each covariate)
              
             if (cov_mat[n,2] != 999) {   //if there is exposure of this covariate in this block
                     
               if (t <= cov_mat[n,2] && t >= cov_mat[n,1]) {
                    // if exposure is happening now
                    // set value to 1
               out_vars[n] = 1;
               } else if (t > cov_mat[n,2] && cov_mat[n,3] != 999) {
                    // otherwise if there is exposure in this block
                    // and this covariate is set to have a decaying effect
                    // and time is after it has stopped
                    // set value to decay from stop time
               out_vars[n] = exp(-(t-cov_mat[n,2])/gamma);
                
               } else if (t < cov_mat[n,1] && cov_mat[n,3] < 0) {
                    // otherwise, if time is before start time
                    // and there is previous exposure
                    // set value to decay from previous time
               out_vars[n] = exp(-(t-cov_mat[n,3])/gamma);
               } else {
                     //  otherwise set to 0
                    out_vars[n] = 0; 
               }
              
            } else {  // if there is no exposure in this block
               if (cov_mat[n,3] < 0) {
                     // if there has been previous exposure, set value to decaying value
                  out_vars[n] = exp(-(t - cov_mat[n,3])/gamma);
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
      
      real[] twostateODE2(real t,   // time
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
         real gamma;
         real lambda;
         real mu;
        // real x_r2[3,x_i[1]];
         
       //  print("meh")
         lambda = theta[1];
         mu = theta[2];
         gamma = theta[3];
        // print(theta[4:(3+x_i[1])])
         
         alphas = theta[4:(3+x_i[1])];
     //    print("alphas")
     //    print(alphas)
         betas = theta[(4+x_i[1]):(3+2*x_i[1])];
      //   print("coefs")
         
       //  x_r2 = to_matrix(x_r, x_i[1],3);
         
       coefs = return_time_varying_coefs_exp(x_r, t, x_i[1], gamma);
      //   print(coefs);
       // coefs = return_time_varying_coefs_exp()
         
         lambda = exp(dot_product(coefs, alphas));
         mu = exp(dot_product(coefs, betas));
         
      dydt[1] = -y[1]*lambda + y[2]*mu;
      dydt[2] = y[1]*lambda- y[2]*mu;   
         return dydt;
         
      }
      
      // test passing to ode solver
      real[] test_pass_to_rk45(real[] t,   // time
      real[] start_state,
    //  real[] y,                     // state
      real[] theta,                 // parameters
      real[,] x_r2,                   //data
      int[] x_i) {         // number of covariates
         
         real out[2];
         real x_r[3*x_i[1]];
         x_r = to_array_1d(x_r2);
         print(x_r);
 //       out = integrate_ode_rk45(twostateODE2, start_state[], 0, t[], theta[], x_r[], x_i[], 1E-6,1E-6, 1E6);
      return out;    
      }
      

   
real[] test_flatten_array(real[,] array) {
   real flat[9];
   real arrayagain[3,3];
   flat = to_array_1d(array);
   print(flat);
   arrayagain[] = to_array_2d(to_matrix(flat,3,3,0));
   print(arrayagain);
   return flat;
}
}

  data {
      int < lower = 1 > N; // Sample size
      int < lower = 0 > n_covs; // number of covariates
      real t[N]; // end time
      real cov_mat[N,3,n_covs]; // matrix of covariates
      }

      transformed data {
      // real x_r[0];
      // int x_i[0];
      
      }
      
      parameters {
      real < lower = 0 > lambda;
      real < lower = 0 > mu;
      real < lower = 0 > gamma;
      real alphas[n_covs];
      real betas[n_covs];
            
      }
      
      transformed parameters {
      real theta[n_covs*2 + 3];
      theta[1] = lambda;
      theta[2] = mu;
      theta[3] = gamma;
      theta[4:(3+n_covs)] = alphas[];
      theta[(4+n_covs):(3+2*n_covs)] = betas[];
      }
      







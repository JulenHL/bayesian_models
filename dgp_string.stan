functions {
  /**
    * Return draws from a linear regression with data matrix X,
  * coefficients beta, and student-t noise with degrees of freedom nu
  * and scale sigma.
  *
    * @param X Data matrix (N x P)                                  \\ JHL - I guess these lines define each part of the regression?
  * @param beta Coefficient vector (P x 1)                        \\ JHL - Is the syntax equivalent to a Var_Alias system?
  * @param nu Residual distribution degrees of freedom. 
  * @param sigma Residual distribution scale.
  * @return Return an N-vector of draws from the model.
  */
    
    vector dgp_rng(matrix X, vector beta, real nu, real sigma) {      // JHL - OK
      vector[rows(X)] y; // define the output vector to be as long as the number of rows in X  // JHL - OK
      
      // Now fill it in
      for (n in 1:rows(X))                                 // JHL - OK
      y[n] <- student_t_rng(nu, X[n] * beta, sigma);      // JHL - OK
      return y;                                           // JHL - Syntax, I guess
    }
}
data {
  // If we were estimating a model, we'd define the data inputs here
}
parameters {
// ... and the parameters we want to estimate would go in here
}
model {
// This is where the probability model we want to estimate would go
}
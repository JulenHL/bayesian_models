# In R:
# Load necessary libraries and set up multi-core processing for Stan
options(warn=-1, message =-1) # JHL - This line turns of warnings, warum auch immer...
library(dplyr); library(ggplot2); library(rstan); library(reshape2) # JHL - This line loads necessary libraries
rstan_options(auto_write = TRUE) # JHL - automatically save a bare version of a compiled Stan program to the hard disk
options(mc.cores = parallel::detectCores()) # JHL - does not need to be recompiled and to execute multiple Markov chains in parallel



# In R, or you could save the contents of the string in a file with .stan file type

dgp_string <- " \\ JHL - Preallocation of string?

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
"

# Generate a matrix of random numbers, and values for beta, nu and sigma

set.seed(42) # Set the random number generator seed so that we get the same parameters
N <- 1000 # Number of observations
P <- 10 # Number of covariates
X <- matrix(rnorm(N*P), N, P) # generate an N*P covariate matrix of random data
nu <- 5 # Set degrees of freedom
sigma <- 5 # And scale parameter
beta <- rnorm(10) # Generate some random coefficients that we'll try to recover
# Make sure the first element of beta is positive as in our chosen DGP
beta[1] <- abs(beta[1])



# Compile the script
compiled_function <- stan_model(model_code = dgp_string) # you could use file = "path/to/yourfile.stan" if you have saved it as so
#compiled_function <- stan_model(model_code = "N:/R/dgp_string.stan") # you could use file = "path/to/yourfile.stan" if you have saved it as so


# And make the function available to the user in R
expose_stan_functions(compiled_function)




 
# In R:
# Load necessary libraries and set up multi-core processing for Stan
options(warn=-1, message =-1)
library(dplyr); library(ggplot2); library(rstan); library(reshape2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

In R, or you could save the contents of the string in a file with .stan file type

dgp_string <- "

functions {
/**
* Return draws from a linear regression with data matrix X,
* coefficients beta, and student-t noise with degrees of freedom nu
* and scale sigma.
*
* @param X Data matrix (N x P)
* @param beta Coefficient vector (P x 1)
* @param nu Residual distribution degrees of freedom.
* @param sigma Residual distribution scale.
* @return Return an N-vector of draws from the model.
*/

vector dgp_rng(matrix X, vector beta, real nu, real sigma) {
vector[rows(X)] y; // define the output vector to be as long as the number of rows in X

// Now fill it in
for (n in 1:rows(X))
y[n] <- student_t_rng(nu, X[n] * beta, sigma);
return y;
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
# And make the function available to the user in R
expose_stan_functions(compiled_function)




# In R:
# Load necessary libraries and set up multi-core processing for Stan
options(warn=-1, message =-1)
library(dplyr); 
library(rstan);
library(reshape2);
library(ggplot2);
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load Data

choiceC<-read.table('model_lever_data.csv',sep=";", header=FALSE,strip.white=TRUE)
choiceC=data.matrix(choiceC)

shock<-read.table('model_shock_data.csv',sep=";", header=FALSE,strip.white=TRUE)
shock=data.matrix(shock)

selfvalue<-choiceC

noshock<-read.table('noshock.csv',sep=";", header=FALSE,strip.white=TRUE)
noshock=data.matrix(noshock)

N=12;
T=48;
Tsubj=rep(T,N);
fitNoShock <- stan(file='2ArmBanditBlocked_wConfl_v2.stan',data = list(N,T,Tsubj,choice=choiceC,shock=noshock,selfvalue=selfvalue), cores = 1, thin=3, warmup=1000, chains = 3, iter = 3000)
fitShock <- stan(file='2ArmBanditBlocked_wConfl_v2.stan',data = list(N,T,Tsubj,choice=choiceC,shock=shock,selfvalue=selfvalue), cores = 1, thin=3, warmup=1000, chains = 3, iter = 3000)


print(fitShock)
#write.table(summary(fitShock),"summaryShock.csv",sep=",")

print(fitNoShock)
#write.table(summary(fitNoShock),"summaryNoShock.csv",sep=",")

# looks at fit with shinystan
library(shinystan)
shinystan::launch_shinystan(fitShock)


library(loo) # Load the library

# compare fit for hand and foot

log_likfitShock <- extract_log_lik(fitShock)
log_likfitNoShock <- extract_log_lik(fitNoShock)

loo_fitShock <- loo(log_likfitShock)  
loo_fitNoShock <- loo(log_likfitNoShock) 

print(compare(loo_fitNoShock,loo_fitShock), digits = 2) 

## ploting the parameter estimates against the known value for each model

log_likfitShock <- extract_log_lik(fitShock)

# Declare a data frame that contains the known parameter names in one column `variable` and their known values
# !!!!!!!!!!!TO DO!!!!!!!

# This extract only the parameter estimates
#fit_DOI = summary(fitShock, pars = c("B_pr"))$summary
# This extracts the estimates for each iteration
fit_DOI = extract(fitShock, permuted = F, pars = c("B_pr"))


## Looks at the expected values trial by trial



for(t in 1:T){
test[t] <- mean(rstan::extract(fitShock, 
                               permuted=TRUE,
                               inc_warmup = FALSE, 
                               include = TRUE))
}

plot(test)

for(t in 1:T){
  test[t] <- mean(rstan::extract(fitNoShock, 
                                 permuted=TRUE,
                                 inc_warmup = FALSE, 
                                 include = TRUE))
}

plot(test)

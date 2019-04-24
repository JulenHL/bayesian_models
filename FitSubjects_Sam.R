# In R:
# Load necessary libraries and set up multi-core processing for Stan
options(warn=-1, message =-1)
library(dplyr); 
library(rstan);
library(reshape2);
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#df = read.xls ("myfile.xlsx"), sheet = 1, header = TRUE)
################################################################################
# THE MONEY DATA.  ###################################################################
################################################################################

moneychoice<-read.table('Money_Choice.csv',sep=",", header=FALSE)
moneychoice=data.matrix(moneychoice)
moneyoutcome<-read.table('Money_Outcome.csv',sep=",", header=FALSE)
moneyoutcome=data.matrix(moneyoutcome)
# 2ArmBandits takes the following parameters
#int<lower=1> N; #where N is the number of subjects
#int<lower=1> T; #max number of trials per subject
#int<lower=1> TBlock; #number of trials before new symbols are used
#int<lower=1,upper=T> Tsubj[N]; #vector containing the number of trials for each subject
#int<lower=1,upper=2> choice[N,T]; #Matrix of N subjects x T trials of 1 and 2 indicating whether participants chose option 1 or 2 on every trial
#real rewlos[N,T];  # Matrix of N subjects x T trials with no lower and upper bounds, indicating the reward or loss incurred on ever trial.    

N=17;
T=90;
TBlock=15;
Tsubj=rep(T,N);

#the rewlos is coded as 2 for a large loss of -4Euro and 1 for a small loss of -1.5Euro. 
#because the learning rule asumes us to choose the highest EV, we need to change that so that the larger value is the desired choice.
#hence we need to recode. We could do 1->0 and 2->-1 (i.e. neutral vs. negative), to account for a recentering on the less worse option.
#we could as well use the average outcome as zero, and express the other as deviation from that ()
rewlos=(-moneyoutcome+1)*2.5+1.25
rewlos=(-moneyoutcome+1);
fitM <- stan(file='2ArmBanditBlocked_averageEVreset.stan',data = list(N,T,TBlock,Tsubj,choice=moneychoice,rewlos=rewlos), cores = 4, thin=3, warmup=1000, chains = 2, iter = 3000)

write.table(summary(fitM),"summaryMoneyOnly_avEV.csv",sep=",")

print(fitM)              

# looks at fit with shinystan
library(shinystan)
shinystan::launch_shinystan(fitM)


################################################################################
# THE CONFLICT DATA.  ###################################################################
################################################################################

conflictchoice<-read.table('Conflict_Choice.csv',sep=",", header=FALSE)
conflictchoice=data.matrix(conflictchoice)
conflictoutcomeM<-read.table('Conflict_Outcome_Money.csv',sep=",", header=FALSE) #1=low loss, 2=high loss
conflictoutcomeM=data.matrix(conflictoutcomeM)
conflictoutcomeS<-read.table('Conflict_Outcome_Shock.csv',sep=",", header=FALSE) #1=low shock and 2=high shock
conflictoutcomeS=data.matrix(conflictoutcomeS)
rewlosCM=(-conflictoutcomeM+1) #recodes so that lowloss=0 and highloss has value of -1
rewlosCS=-(conflictoutcomeS-1) #recodes so that lowshock=0 and high shock value of -1

# 2ArmBandits takes the following parameters
#int<lower=1> N; #where N is the number of subjects
#int<lower=1> T; #max number of trials per subject
#int<lower=1> TBlock; #number of trials before new symbols are used
#int<lower=1,upper=T> Tsubj[N]; #vector containing the number of trials for each subject
#int<lower=1,upper=2> choice[N,T]; #Matrix of N subjects x T trials of 1 and 2 indicating whether participants chose option 1 or 2 on every trial
#real rewlos[N,T];  # Matrix of N subjects x T trials with no lower and upper bounds, indicating the reward or loss incurred on ever trial.    
#data for money only paradigm(M)



# 2ArmBanditBlocked_with_without_conflict.stan takes the following arguments
# this section reads in the data that is piped through the r-command, and is only executed once
#int<lower=1> N; #where N is the number of subjects
#int<lower=1> T; #max number of trials per subject
#int<lower=1> TBlock; #number of trials before new symbols are used
#int<lower=1,upper=T> Tsubj[N]; #vector containing the number of trials for each subject

N=17;
T=90;
TBlock=15;
Tsubj=rep(T,17);


fitC1 <- stan(file='2ArmBanditBlocked.stan',data = list(N,T,TBlock,Tsubj,choice=conflictchoice,rewlos=rewlosCM), cores = 4, thin=3, warmup=1000, chains = 2, iter = 3000)
print(fitC1)
#data for money only paradigm(M)
#int<lower=1,upper=2> choiceM[N,T]; #Matrix of N subjects x T trials of 1 and 2 indicating whether participants chose option 1 or 2 on every trial 
#real rewlosM[N,T];  # Matrix of N subjects x T trials with no lower and upper bounds, indicating the reward or loss incurred on ever trial.   
#data with conflict (C)
#int<lower=1,upper=2> choiceC[N,T]; #Matrix of N subjects x T trials of 1 and 2 indicating whether participants chose option 1 or 2 on every trial
#real rewlosCM[N,T];  # Matrix of N subjects x T trials with no lower and upper bounds, indicating the Money reward or loss incurred on ever trial.   
#real rewlosCS[N,T];  # Matrix of N subjects x T trials with no lower and upper bounds, indicating the Shock incurred on ever trial.   


write.table(summary(fitC1),"summaryConflictOnly.csv",sep=",")

N=17;
T=90;
TBlock=15;
Tsubj=rep(T,17);

fitC2 <- stan(file='2ArmBanditBlocked_with_without_conflict.stan',data = list(N,T,TBlock,Tsubj,choiceM=moneychoice,rewlosM=rewlos,choiceC=conflictchoice,rewlosCM=rewlosCM,rewlosCS=rewlosCS), cores = 4, thin=3, warmup=1000, chains = 2, iter = 3000)


print(fitC2)
write.table(summary(fitC2),"summaryBoth.csv",sep=",")

# looks at fit with shinystan
library(shinystan)
shinystan::launch_shinystan(fitC2)












library(loo) # Load the library

# compare fit for hand and foot








## Looks at the expected values trial by trial
for(t in 1:153){
test[t] <- mean(rstan::extract(fitNUF, permuted=F,pars=paste("tbtPE[1,",t,"]")))
}

plot(test)

for(t in 1:153){
  test[t] <- mean(rstan::extract(fit2hb, permuted=F,pars=paste("tbtev1[1,",t,"]")))
}

plot(test)

for(t in 1:153){
  test[t] <- mean(rstan::extract(fit2hb, permuted=F,pars=paste("tbtev2[1,",t,"]")))
}

plot(test)




for(t in 1:153){
  test[t] <- mean(rstan::extract(fit2hb, permuted=F,pars=paste("tbtPE[2,",t,"]")))
}

plot(test)

for(t in 1:153){
  test[t] <- mean(rstan::extract(fit2hb, permuted=F,pars=paste("tbtPE[2,",t,"]")))
}

plot(test)


myllik<-extract_log_lik(fit2hb, parameter_name = "log_lik")

test<-mean(rstan::extract(fit2hb, permuted=F,pars="tbtev1[1,1]"))
}

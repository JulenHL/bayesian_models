require(hBayesDM)
require(rstan)
require(loo)

use_niter <- 3400
use_nwarmup <- 1000
use_nchain <- 1
use_ncore <- 1
use_nthin <- 3


SelfChData <- choiceC[,1:24]
SelfFBData <- selfvalue[,1:24]
SelfFBData[which(SelfFBData == 1)] = -1
SelfFBData[which(SelfFBData == 2)] = 1
SubjID <- c(1:12)

num_sbj <- length(SubjID)
num_trials <- dim(SelfChData)[2]

AllData <- c()

for (subj_num in 1:num_sbj) {
  this_subj_data <- cbind(rep(subj_num,num_trials),(SelfChData[subj_num,]),(SelfFBData[subj_num,]))
  
  AllData <- rbind(AllData,this_subj_data)
}

AllData <- as.data.frame(AllData)
rownames(AllData) <- c()
colnames(AllData) <- c("subjID","choice","outcome")


write.table(AllData,"SelfData.txt",sep="\t")

prl_rpout <- prl_rp(data = "SelfData.txt",
                 niter=use_niter,
                 nwarmup = use_nwarmup,
                 nchain = use_nchain,
                 ncore = use_ncore,
                 nthin = use_nthin)

prl_ewaout <- prl_ewa(data = "SelfData.txt",
                    niter=use_niter,
                    nwarmup = use_nwarmup,
                    nchain = use_nchain,
                    ncore = use_ncore,
                    nthin = use_nthin)

prl_fictout <- prl_fictitious(data = "SelfData.txt",
                     niter=use_niter,
                     nwarmup = use_nwarmup,
                     nchain = use_nchain,
                     ncore = use_ncore,
                     nthin = use_nthin)





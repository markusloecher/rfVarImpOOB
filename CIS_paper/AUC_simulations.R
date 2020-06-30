#The data has 1000 samples with 50 features. All
#features are discrete, with the j th feature containing j + 1 distinct values 0,1,...,j . We randomly
#select a set S of 5 features from the first ten as relevant features. The remaining features are noisy
#feature
library(foreach)
library(doParallel)
library(rfVarImpOOB)
library(ranger)

N=1000
p=50
ntree=100

Nsims = ncores = 100
set.seed(123)
verbose=FALSE

NoisyFeatureSim = function(N,p,ntree,verbose=0,MDIonly=FALSE, correctBias=TRUE){
  x=matrix(0,nrow=N,ncol=p)
  for (k in 1:p){
    x[,k] = sample(0:k,N,replace=TRUE)
  }
  #rlvFtrs = sample(10,5) #relevant Features
  rlvFtrs = rep(0,p)
  rlvFtrs[sample(10,5)] =  1
  y=Xrlv=rep(0,N)
  for (k in which(rlvFtrs==1)) {
    Xrlv=Xrlv+x[,k]/k
  }
  #hist(2*Xrlv/5-1) # nicely symmetric around 0 !
  y = factor(rbinom(N,1,plogis(2*Xrlv/5 - 1)))
  data=cbind.data.frame(x,y)
  colnames(data)[1:p] = paste0("x",1:p)
  
  rf <- randomForest(y ~ ., data = data, keep.inbag=TRUE,
                     importance = TRUE, ntree = ntree, mtry = 3)
  if (verbose>1) browser()
  impRf=NULL
  try({impRf = importance(rf)})
    
  if (MDIonly) return(cbind(impRf,rlvFtrs = rlvFtrs))
  
  rfobj <- ranger(y ~ ., data = data, keep.inbag=TRUE,  importance = "impurity_corrected", num.trees =ntree, mtry = 3)
  
  if (is.factor(data$y)) {
    #cat(i, "th converting factor to binary 0/1 label. \n")
    data$y = as.numeric(data$y)-1
  }
  
  VI_sim_00 = GiniImportanceForest(rf, data,score="PMDI00",Predictor=mean, ylabel="y", correctBias=correctBias, verbose=verbose)
#  VI_sim_01 = GiniImportanceForest(rf, data,score="PMDI01",Predictor=mean, ylabel="y", correctBias=correctBias, verbose=verbose)
  VI_sim_21 = GiniImportanceForest(rf, data,score="PMDI21",Predictor=mean, ylabel="y", correctBias=correctBias)
  VI_sim_22 = GiniImportanceForest(rf, data,score="PMDI22",Predictor=mean, ylabel="y", correctBias=correctBias)
  VI_sim_23 = GiniImportanceForest(rf, data,score="PMDI23",Predictor=mean, ylabel="y", correctBias=correctBias, verbose=verbose)
  
  Gini_OOB_0 = VI_sim_00[,"Gini_OOB",drop=FALSE]
#  Gini_OOB_01 = VI_sim_01[,"Gini_OOB",drop=FALSE]
  Gini_OOB_3 = VI_sim_23[,"Gini_OOB",drop=FALSE]
  AIR = rfobj$variable.importance # Nembrinin et al,
  Gini_OOB_1 = VI_sim_21[,"Gini_OOB",drop=FALSE]
  Gini_OOB_2 = VI_sim_22[,"Gini_OOB",drop=FALSE]
  
  if (verbose) browser()
  cbind(Gini_OOB_0=Gini_OOB_0,Gini_OOB_2=Gini_OOB_2,#Gini_OOB_1=Gini_OOB_1,
        Gini_OOB_3=Gini_OOB_3,AIR=AIR, MDI = impRf[,"MeanDecreaseGini"], 
        MDA = impRf[,"MeanDecreaseAccuracy "], rlvFtrs = rlvFtrs)
}

NoisyFeatureSim_Add = function(N,p,ntree,verbose=0,MDIonly=FALSE,nodesize=1){
  x=matrix(0,nrow=N,ncol=p)
  for (k in 1:p){
    x[,k] = sample(0:k,N,replace=TRUE)
  }
  #rlvFtrs = sample(10,5) #relevant Features
  rlvFtrs = rep(0,p)
  rlvFtrs[sample(10,5)] =  1
  y=Xrlv=rep(0,N)
  for (k in which(rlvFtrs==1)) {
    Xrlv=Xrlv+x[,k]/k
  }
  #hist(2*Xrlv/5-1) # nicely symmetric around 0 !
  y = factor(rbinom(N,1,plogis(2*Xrlv/5 - 1)))
  data=cbind.data.frame(x,y)
  colnames(data)[1:p] = paste0("x",1:p)
  
  rf <- randomForest(y ~ ., data = data, keep.inbag=TRUE,
                     importance = TRUE, ntree = ntree, mtry = 3,nodesize=nodesize)
  # if (verbose>1) browser()
  # impRf=NULL
  # try({impRf = importance(rf)})
  # 
  # if (MDIonly) return(cbind(impRf,rlvFtrs = rlvFtrs))
  # 
  #rfobj <- ranger(y ~ ., data = data, keep.inbag=TRUE,  importance = "impurity_corrected", num.trees =ntree, mtry = 3)
  
  if (is.factor(data$y)) {
    #cat(i, "th converting factor to binary 0/1 label. \n")
    data$y = as.numeric(data$y)-1
  }
  
  VI_sim_00 = GiniImportanceForest(rf, data,score="PMDI00",Predictor=mean, ylabel="y", correctBias=correctBias, verbose=verbose)
  # VI_sim_01 = GiniImportanceForest(rf, data,score="PMDI01",Predictor=mean, ylabel="y", correctBias=correctBias verbose=verbose)
  # VI_sim_21 = GiniImportanceForest(rf, data,score="PMDI21",Predictor=mean, ylabel="y")
  # VI_sim_22 = GiniImportanceForest(rf, data,score="PMDI22",Predictor=mean, ylabel="y")
  # VI_sim_23 = GiniImportanceForest(rf, data,score="PMDI23",Predictor=mean, ylabel="y", verbose=verbose)
  # 
   Gini_OOB_00 = VI_sim_00[,"Gini_OOB",drop=FALSE]
  # Gini_OOB_01 = VI_sim_01[,"Gini_OOB",drop=FALSE]
  # Gini_OOB_3 = VI_sim_23[,"Gini_OOB",drop=FALSE]
  # AIR = rfobj$variable.importance # Nembrinin et al,
  # Gini_OOB_1 = VI_sim_21[,"Gini_OOB",drop=FALSE]
  # Gini_OOB_2 = VI_sim_22[,"Gini_OOB",drop=FALSE]
  # 
  # if (verbose) browser()
  # cbind(Gini_OOB_0=Gini_OOB_0,Gini_OOB_1=Gini_OOB_1,Gini_OOB_2=Gini_OOB_2,
  #       Gini_OOB_3=Gini_OOB_3,AIR=AIR, MDI = impRf[,"MeanDecreaseGini"], 
  #       MDA = impRf[,"MeanDecreaseAccuracy "], rlvFtrs = rlvFtrs)
   cbind(Gini_OOB_00=Gini_OOB_00, rlvFtrs = rlvFtrs)
}

seriousRun=TRUE

if (seriousRun){
  if (ncores>1){
    cl <- makePSOCKcluster(ncores)
    registerDoParallel(cl)
     parSims = foreach(i=1:Nsims, .combine=rbind, .packages=c('rfVarImpOOB',"ranger")) %dopar% {
      NoisyFeatureSim(N,p,ntree,0,0)
    }
    # parSims2 = foreach(i=1:Nsims, .combine=rbind, .packages=c("randomForest")) %dopar% {
    #   NoisyFeatureSim(N,p,ntree,0,TRUE)
    # }
    # parSims3 = foreach(i=1:Nsims, .combine=rbind, .packages=c("rfVarImpOOB")) %dopar% {
    #   NoisyFeatureSim_Add(N,p,ntree,0,FALSE)
    # }
    #parSims3a = foreach(i=1:Nsims, .combine=rbind, .packages=c("rfVarImpOOB")) %dopar% {
    #  NoisyFeatureSim_Add(N,p,ntree,0,FALSE,nodesize=5)
    #}
    stopCluster(cl)
    #colnames(parSims)[1:4] = paste0("Gini_OOB", 0:3)
  } else {
    for (i in 1:Nsims) {
      parSims[[i]] = NoisyFeatureSim(N,p,ntree)
    }
  }
} else {
  NoisyFeatureSim(100,15,10,0,1)
}

save(parSims,parSims2,parSims3,parSims3a, file = paste0("AUCsims_",Sys.Date(),".rda"))

if (0){
  library(AUC)
  
  M=ncol(parSims)
  aucSims = matrix(0,nrow=1,ncol=M-1);k=1
  colnames(aucSims) = colnames(parSims)[1:(M-1)] #, "MDA", "MDI","Mod_GOOB_node1","Mod_GOOB_node5")
  options(digits=3)
  for (j in 1:(M-1)){
    aucSims[1,k] = auc(roc(parSims[,j],factor(parSims[,"rlvFtrs"])))
    cat(colnames(parSims)[j],aucSims[1,k], "\n")
    k=k+1
  }
  
  
  
  for (j in 3:4){
    aucSims[1,k] = auc(roc(parSims2[,j],factor(parSims2[,"rlvFtrs"])))
    cat(colnames(parSims2)[j],aucSims[1,k], "\n")
    k=k+1
  }
  for (j in 1:1){
    aucSims[1,k] = auc(roc(parSims3[,j],factor(parSims3[,"rlvFtrs"])))
    cat(colnames(parSims3)[j],aucSims[1,k], "\n")
    k=k+1
  }
  for (j in 1:1){
    aucSims[1,k] = auc(roc(parSims3a[,j],factor(parSims3a[,"rlvFtrs"])))
    cat(colnames(parSims3a)[j],aucSims[1,k], "\n")
    k=k+1
  }
  library(xtable)
  print(xtable(aucSims),comment=FALSE)

}
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

NoisyFeatureSim = function(N,p,ntree,verbose=0){
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
  ret=try({impRf = randomForest::importance(rf)})
  if (class(ret) == "try-error")  browser()
  
  return(cbind(impRf[,3:4],rlvFtrs = rlvFtrs))
  
}



NoisyFeatureSimMC = function(N,p,ntree,verbose=0, NoisyFeatureSim,Nsims=1,
                             ncores=1){
  if (ncores>1){
    cl <- makePSOCKcluster(ncores)
    on.exit(stopCluster(cl))
    
    registerDoParallel(cl)
    parSims = foreach(i=1:Nsims, .combine=rbind, .packages=c('rfVarImpOOB',"ranger")) %dopar% {
      NoisyFeatureSim(N,p,ntree,0)
    }
 
  } else {
    parSims=list()
    for (i in 1:Nsims) {
      parSims[[i]] = NoisyFeatureSim(N,p,ntree,0)
    }
    parSims = do.call("rbind",parSims)
  }
  colnames(parSims)[1:2] = c("MDA", "MDI")
  #colnames(parSims)[1:4] = paste0("GOOB_",0:3)
  attr(parSims, "createdAt") <- Sys.time()
  #save(parSims, file = paste0("AUCsims_",paste0(as.numeric(correctBias),collapse=""),"_",Sys.Date(),".rda"))#,parSims2,parSims3,parSims3a
  return(parSims)
}

myAUC = function(parSims, cols2Analyze = c("MDA", "MDI")){
  
  library(AUC)
  
  aucSims = matrix(0,nrow=1,ncol=length(cols2Analyze));k=1
  colnames(aucSims) = cols2Analyze
  options(digits=3)
  
  for (f in cols2Analyze){
    aucSims[1,k] = auc(roc(parSims[,f],factor(parSims[,"rlvFtrs"])))
    cat(colnames(parSims[,f]),aucSims[1,k], "\n")
    k=k+1
  }
  library(xtable)
  print(xtable(aucSims),comment=FALSE)
  aucSims
}


seriousRun=FALSE


if (seriousRun){
  parSims_FF = NoisyFeatureSimMC(N,p,ntree,verbose=0, NoisyFeatureSim,
                                 ncores=ncores)
  parSims_FF_AUC = myAUC(parSims_FF)  

  
} else {#no serious run!
  #NoisyFeatureSim(100,15,10,0,1) # small experiment
  N=100;p=15;ntree=10;Nsims = 5
  #tmp = NoisyFeatureSim(N,p,ntree,verbose=1)
  tmp = NoisyFeatureSimMC(N,p,ntree,verbose=0, NoisyFeatureSim,Nsims=Nsims,ncores=1)
  #colnames(tmp)[1:4] = paste0("GOOB_",0:3)
}




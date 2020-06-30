plotImp = function(NullSim,NullSim03,
  fname=paste0(baseDir, "figures/NullSim.pdf")
){
  pdf(fname, height=6, width=8)
  par(mfrow=c(2,3),cex=0.85, mar=c(3,3,4,1))
  boxplot(t(NullSim$MDI_sim),main=TeX("MDI"),ylab="");grid();abline(h=0,col=2,lty=2)
  mtext("Importance", side = 2,line=2)
  boxplot(t(NullSim03$MDI_sim),main=TeX("AIR"),ylab="");grid();abline(h=0,col=2,lty=2)
  boxplot(t(NullSim03$OOB_sim_0),main=TeX("PG_{OOB}^{(0)}"),ylab="", ylim = c(-1,2));grid();abline(h=0,col=2,lty=2)
  boxplot(t(NullSim$OOB_sim_1)/4,main=TeX("PG_{OOB}^{(1)}"),ylab="", ylim = c(-1,2));grid();abline(h=0,col=2,lty=2)
  boxplot(t(NullSim$OOB_sim_2),main=TeX("PG_{OOB}^{(2)}"),ylab="", ylim = c(-1,2));grid();abline(h=0,col=2,lty=2)
  boxplot(t(NullSim03$OOB_sim_3)/2,main=TeX("PG_{OOB}^{(3)}"),ylab="", ylim = c(-1,2));grid();abline(h=0,col=2,lty=2)
  dev.off()
  
}

plotVI3 = function# creates barplots for variable importances using only base R graphics
### creates barplots for variable importances
(
  rf, ##<< object retuned by a calls to randomForest (with importance = TRUE)
  decreasing = TRUE##<< which direction to sort
){
  rfImp = importance(rf)
  
  par(mfrow=c(1,3), mar=c(3,7,2,1), cex = 1)
  ii = order(rfImp[,"MeanDecreaseAccuracy"])
  barplot(rfImp[ii,"MeanDecreaseAccuracy"], names = rownames(rfImp)[ii], horiz=TRUE, las=1 , main = "permutation importance", col="darkgreen");grid()
  
  ii = order(rfImp[,"MeanDecreaseGini"])
  barplot(rfImp[ii,"MeanDecreaseGini"], names = rownames(rfImp)[ii], horiz=TRUE, las=1 , main = "Gini importance", col="darkblue");grid()
}

SimulateData = function(
  n=120, M=100, 
  relevance = 0.15, 
  prevResults= NULL,
  ntree = 50,
  verbose=0
){
  library(foreach)
  library(doParallel)
  Gini_OOB_1=Gini_OOB_2=MDI=list()

  
  set.seed(123)
  cl <- makePSOCKcluster(M)
  registerDoParallel(cl)
  parSims = foreach(i=1:M, .combine=cbind, .packages='rfVarImpOOB') %dopar% {
  #for (i in 1:M){
    
    x1 = rnorm(n)
    x2 = base::sample(gl(2,1),n,replace=TRUE)
    x3 = base::sample(gl(4,1),n,replace=TRUE)#rmultinom(n, 1, prob=rep(1/k,k))
    x4 = base::sample(gl(10,1),n,replace=TRUE)
    x5 = base::sample(gl(20,1),n,replace=TRUE)
    y= factor(rbinom(n,1,p=0.5 + relevance*c(-1,1)[as.numeric(x2)]))#NULL case
    #test: rbinom(8,1,p=0.5 + rep(c(-1,1),4)*0.4)
    
    data = cbind.data.frame(x1,x2,x3,x4,x5,y)
    
    RF3 <- randomForest(y ~ ., data = data, keep.inbag=TRUE,
                        importance = TRUE, ntree = ntree, mtry = 3)
    
    if (is.factor(data$y)) {
      cat(i, "th converting factor to binary 0/1 label. \n")
      data$y = as.numeric(data$y)-1
    }
    
    Gini_OOB = GiniImportanceForest(RF3, data,score="PMDI21",Predictor=mean, ylabel="y")
    # VI_sim_22 = GiniImportanceForest(RF3, data,score="PMDI22",Predictor=mean, ylabel="y")
    # Gini_OOB_1[[i]] = VI_sim_21[,"Gini_OOB",drop=FALSE]
    # Gini_OOB_2[[i]] = VI_sim_22[,"Gini_OOB",drop=FALSE]
    # MDI[[i]] = VI_sim_21[,"MeanDecreaseGini",drop=FALSE]
    # cbind(Gini_OOB_1=Gini_OOB_1[[i]],Gini_OOB_2=Gini_OOB_2[[i]],MDI=MDI[[i]])
    Gini_OOB
  }
  stopCluster(cl)
  
  return(parSims)
  
  parSimsList=list()
  parSimsList$OOB_sim_1= parSims[,seq(1,length=M,by=3)]
  parSimsList$OOB_sim_2= parSims[,seq(2,length=M,by=3)]
  parSimsList$MDI_sim= parSims[,seq(3,length=M,by=3)]
  
  return(parSimsList)
  
  OOB_sim_1 = t(do.call("cbind", Gini_OOB_1))
  OOB_sim_2 = t(do.call("cbind", Gini_OOB_2))
  MDI_sim = t(do.call("cbind", MDI))
  
  try({
    if (!is.null(prevResults)) {
      OOB_sim_1=rbind(OOB_sim_1,prevResults$OOB_sim_1)
      OOB_sim_2=rbind(OOB_sim_2,prevResults$OOB_sim_2)
      MDI_sim=rbind(MDI_sim,prevResults$MDI_sim)
    } 
  })
  return(list(OOB_sim_1=OOB_sim_1,OOB_sim_2=OOB_sim_2,MDI_sim=MDI_sim))
}

SimulateData03 = function#same as above but for importance scores 0 (OOB only) and 3
(n=120, M=100, 
 relevance = 0.15, ##<< signal strength (what a stupid name for this parameter)
 prevResults= NULL,
 ntree = 50,
# Compute  = c(OOB0=TRUE,OOB1=TRUE,AIR=TRUE, TI = TRUE), # which measures should actually be computed?
 verbose=0
){
  library(foreach)
  library(doParallel)
  library(ranger)
  library(tree.interpreter)
  
  Gini_OOB_0=Gini_OOB_3=AIR=TI=list()
  
  set.seed(123)
  cl <- makePSOCKcluster(M)
  registerDoParallel(cl)
  parSims = foreach(i=1:M, .combine=cbind, .packages=c('rfVarImpOOB',"ranger","tree.interpreter")) %dopar% {
    #for (i in 1:M){
    
    x1 = rnorm(n)
    x2 = base::sample(gl(2,1),n,replace=TRUE)
    x3 = base::sample(gl(4,1),n,replace=TRUE)#rmultinom(n, 1, prob=rep(1/k,k))
    x4 = base::sample(gl(10,1),n,replace=TRUE)
    x5 = base::sample(gl(20,1),n,replace=TRUE)
    y= factor(rbinom(n,1,p=0.5 + relevance*c(-1,1)[as.numeric(x2)]))#NULL case
    #test: rbinom(8,1,p=0.5 + rep(c(-1,1),4)*0.4)
    
    data = cbind.data.frame(x1,x2,x3,x4,x5,y)
    
    RF3 <- randomForest(y ~ ., data = data, keep.inbag=TRUE,
                        importance = TRUE, ntree = ntree, mtry = 3)
    
    rfobj <- ranger(y ~ ., data = data, keep.inbag=TRUE,  importance = "impurity_corrected", num.trees =ntree, mtry = 3)
    
    if (is.factor(data$y)) {
      cat(i, "th converting factor to binary 0/1 label. \n")
      data$y = as.numeric(data$y)-1
    }
    
    #TestRanger = GiniImportanceForest(rfobj, data,score="PMDI00",Predictor=mean, ylabel="y", verbose=verbose)
    VI_sim_00 = GiniImportanceForest(RF3, data,score="PMDI00",Predictor=mean, ylabel="y", verbose=verbose)
    VI_sim_23 = GiniImportanceForest(RF3, data,score="PMDI23",Predictor=mean, ylabel="y", verbose=verbose)
    Gini_OOB_0[[i]] = VI_sim_00[,"Gini_OOB",drop=FALSE]
    Gini_OOB_3[[i]] = VI_sim_23[,"Gini_OOB",drop=FALSE]
    AIR[[i]] = rfobj$variable.importance # Nembrinin et al, Actual impurity reduction importance
    cbind(Gini_OOB_0=Gini_OOB_0[[i]],Gini_OOB_3=Gini_OOB_3[[i]],AIR=AIR[[i]])
  }
  stopCluster(cl)
  
  parSimsList=list()
  parSimsList$OOB_sim_0= parSims[,seq(1,length=M,by=3)]
  parSimsList$OOB_sim_3= parSims[,seq(2,length=M,by=3)]
  parSimsList$AIR= parSims[,seq(3,length=M,by=3)]
  
  return(parSimsList)
  
}

SimulateDataTI = function#same as above but for importance scores from tree.interpreter
(n=120, M=100, 
 relevance = 0.15, ##<< signal strength (what a stupid name for this parameter)
 prevResults= NULL,
 ntree = 50,
 # Compute  = c(OOB0=TRUE,OOB1=TRUE,AIR=TRUE, TI = TRUE), # which measures should actually be computed?
 verbose=0
){
  library(foreach)
  library(doParallel)
  library(ranger)
  library(tree.interpreter)
  
  Gini_OOB_0=Gini_OOB_3=AIR=TI=list()
  
  set.seed(123)
  cl <- makePSOCKcluster(M)
  registerDoParallel(cl)
  parSims = foreach(i=1:M, .combine=cbind, .packages=c('rfVarImpOOB',"ranger","tree.interpreter")) %dopar% {
    #for (i in 1:M){
    
    x1 = rnorm(n)
    x2 = base::sample(gl(2,1),n,replace=TRUE)
    x3 = base::sample(gl(4,1),n,replace=TRUE)#rmultinom(n, 1, prob=rep(1/k,k))
    x4 = base::sample(gl(10,1),n,replace=TRUE)
    x5 = base::sample(gl(20,1),n,replace=TRUE)
    y= factor(rbinom(n,1,p=0.5 + relevance*c(-1,1)[as.numeric(x2)]))#NULL case
    #test: rbinom(8,1,p=0.5 + rep(c(-1,1),4)*0.4)
    
    data = cbind.data.frame(x1,x2,x3,x4,x5,y)
    
    #RF3 <- randomForest(y ~ ., data = data, keep.inbag=TRUE,
    #                    importance = TRUE, ntree = ntree, mtry = 3)
    
    rfobj <- ranger(y ~ ., data = data, keep.inbag=TRUE,  importance = "impurity_corrected", num.trees =ntree, mtry = 3)
    tidy.RF <- tidyRF(rfobj, data[, -6], data[, 6])
    MDIoob(tidy.RF, data[, -6], data[, 6])
    
    #cbind(Gini_OOB_0=Gini_OOB_0[[i]],Gini_OOB_3=Gini_OOB_3[[i]],AIR=AIR[[i]])
  }
  stopCluster(cl)
  
  
  return(parSims)
  
}

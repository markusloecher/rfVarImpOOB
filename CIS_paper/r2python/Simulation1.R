
TestFactorCoding  = function(
  x=gl(4,1), ##<< x values, alternative: x=1:4
  DummyCoding = FALSE, ##<< enforce dummy coding ?
  n=120, # number of rows in data
  relevance = 0.15, # signal srength (0 for NULL)
  ntree = 50, #number of trees in forest
  verbose=0
){
  library(randomForest)
  #test random forests for factor coding:
  #n=120;ntree = 50; relevance=0.15
  
  set.seed(123)
  
  x1 = rnorm(n)
  x3 = base::sample(x,n,replace=TRUE)
  y= factor(rbinom(n,1,p=0.5+ relevance*c(-2,-1,1,2)[as.numeric(x3)]))#NULL case
  
  data = cbind.data.frame(x1,x3,y)
  
  if (DummyCoding == TRUE) {
    xm = model.matrix(y ~ ., data = data)
    RF <- randomForest(x=xm,y=y, keep.inbag=TRUE,
                       importance = TRUE, ntree = ntree)
  } else {
    RF <- randomForest(y ~ ., data = data, keep.inbag=TRUE,
                       importance = TRUE, ntree = ntree)
  }
  
  
  print(RF)
  Imp = as.data.frame(randomForest::importance(RF))
  return(Imp)
}

SimulateData_simple = function(
  n=120, # number of rows in data
  M=100, # number of simulations
  nCores = M, # number of cores to use; set to 1 on Windows!
  relevance = 0.15, # signal srength (0 for NULL)
#  prevResults= NULL, #legacy, not used
  ntree = 50, #number of trees in forest
  #correctBias = c(inbag=TRUE,outbag=TRUE),
  verbose=0
){
  library(foreach)
  library(doParallel)
  library(randomForest)
  
  
  set.seed(123)
  cl <- makePSOCKcluster(nCores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  
  parSims = foreach(i=1:M, .combine=rbind, .packages='rfVarImpOOB') %dopar% {
    #for (i in 1:M){
    
    x1 = rnorm(n)
    x2 = base::sample(gl(2,1),n,replace=TRUE)
    x3 = base::sample(gl(4,1),n,replace=TRUE)#rmultinom(n, 1, prob=rep(1/k,k))
    x4 = base::sample(gl(10,1),n,replace=TRUE)
    x5 = base::sample(gl(20,1),n,replace=TRUE)
    y= factor(rbinom(n,1,p=0.5 + relevance*c(-1,1)[as.numeric(x2)]))#NULL case
    #test: rbinom(8,1,p=0.5 + rep(c(-1,1),4)*0.4)
    
    data = cbind.data.frame(x1,x2,x3,x4,x5,y)
    
    RF <- randomForest(y ~ ., data = data, keep.inbag=TRUE,
                        importance = TRUE, ntree = ntree, mtry = 3)
    
    #Gini_OOB = GiniImportanceForest(RF3, data,score="PMDI21",Predictor=mean, ylabel="y", correctBias = correctBias)
    VIbench = as.data.frame(randomForest::importance(RF))
    #vars = rownames(VIbench)
    VIbench[,3:4]
  }
  #stopCluster(cl)
  
  parSimsList = list()
  parSimsList$MDI = matrix(parSims[,"MeanDecreaseGini"],ncol=5,byrow = TRUE)
  parSimsList$MDA = matrix(parSims[,"MeanDecreaseAccuracy"],ncol=5,byrow = TRUE)
  
  attr(parSimsList, "createdAt") <- Sys.time()
  
  return(parSimsList)
  
}

plotImpList = function(parSimsList,
                       fname=NULL, ##<< paste0(baseDir, "figures/NullSim.pdf")
                       K= c(1:2), ##<< which list elements to plot
                       indTitles = c("MDI", "MDA"),
                       main = "Importance", ##<< title
                       mfrow=c(1,2)
){
  if (!is.null(fname)) pdf(fname, height=6, width=8)
  par(mfrow=mfrow,cex=0.85, mar=c(3,3,5,1))
  #boxplot(parSimsList$MDI,main=TeX("MDI"),ylab="");grid();abline(h=0,col=2,lty=2)
  for (k in 1:length(K)){
    boxplot(parSimsList[[K[k]]],main=indTitles[k],ylab="");grid();abline(h=0,col=2,lty=2)
  }
  mtext(main, side = 3,line=-1.5,outer=TRUE,col="darkblue")
  if (!is.null(fname)) dev.off()
  
}

if (0){

  # Tests:
  NullSim = SimulateData_simple(M=20,nCores=1,ntree=10, relevance = 0.0)
  PowerSim = SimulateData_simple(M=20,nCores=1,ntree=10, relevance = 0.15)
  
  #fname=paste0(baseDir, "figures/NullSim_noBiasCorr",Sys.Date(),".pdf")
  plotImpList(NullSim,fname=NULL, main = "Null Simulation, no BC")
  
  plotImpList(PowerSim,fname=NULL, main = "Power Simulation, no BC")
  
  TestFactorCoding()
  TestFactorCoding(x=1:4)
}

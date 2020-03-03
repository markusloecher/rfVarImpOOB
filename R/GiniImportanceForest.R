GiniImportanceForest <- structure(function#computes inbag and OOB Gini importance averaged over all trees in a forest
### workhorse function of this package
(
  RF, ##<< object returned by call to randomForest() 
  data, ##<< data which was used to train the RF. NOTE: assumes setting of inbag=TRUE while training 
  ylabel = "Survived", ##<< name of dependent variable
  zeroLeaf = TRUE, ##<< if TRUE discard the information gain due to splits resulting in n=1
  agg = c("mean","median","none")[1], ##<< method of aggregating importance scores across trees. If "none" return the raw arrays (for debugging)
  score=c("PMDI21", "MDI","MDA","MIA")[1], ##<< scoring method:MDI=mean decrease impurity (Gini),MDA=mean decrease accuracy (permutation),MIA=mean increase accuracy
  Predictor=mean, ##<< function to estimate node prediction, such as Mode or mean or median. Alternatively, pass an array of numbers as replacement for the yHat column of tree
  correctBias = FALSE, ##<< multiply by n/(n-1) for sample variance correction!
  verbose=0 ##<< level of verbosity
){
  stopifnot(!is.null(RF$inbag))
  vars = dimnames(attr(RF$terms, "factors"))
  stopifnot(ylabel == setdiff(vars[[1]], vars[[2]]))
  
  
  nTrees = ncol(RF$inbag)
  
  VIbench = as.data.frame(randomForest::importance(RF))
  vars = rownames(VIbench)
  Gini_OOB = list()
  for (j in 0:4)
    Gini_OOB[[j+1]] = matrix(0,nrow=length(vars),ncol=nTrees,dimnames=list(vars,1:nTrees))
  #Gini_OOB3=Gini_OOB2=Gini_OOB1=Gini_OOB0=Gini_OOB4
  #VItrain = c(PassengerId=0, Sex=0, Pclass=0)
  inTree=outTree=list()
  
  for (k in 1:nTrees){
    #trackingInfo=c(k=k,bag="in");#print(trackingInfo)
    inbag = rep(rownames(RF$inbag),time=RF$inbag[,k])
    trainBag=data[inbag,]
    rownames(trainBag)  = 1:nrow(trainBag)
    #cat("k=",k,"\n")
    #trackingInfo=c(k=k,bag="out");#print(trackingInfo)
    outbag = names((RF$inbag[RF$inbag[,k]==0,k]))
    OOB = data[outbag,]
    rownames(OOB)  = 1:nrow(OOB)
    Imp =GiniImportanceTree(trainBag, OOB, RF,k,ylabel=ylabel, zeroLeaf=zeroLeaf,
                            score=score,correctBias=correctBias)
    stopifnot(all(rownames(Imp) %in% rownames(Gini_OOB[[j+1]])))
    for (j in 0:4)
      Gini_OOB[[j+1]][rownames(Imp),k] = Imp[,paste0("IG_pgOOB",j)]
   
     #if (verbose<0) outTree[[k]] =GiniImportanceTree(OOB, RF, k,returnTree=TRUE,ylabel=ylabel, Predictor=inTree[[k]]$yHat, zeroLeaf=zeroLeaf,score=score,correctBias=correctBias)
   # outImp =GiniImportanceTree(OOB, RF, k,ylabel=ylabel, Predictor=inTree[[k]]$yHat, zeroLeaf=zeroLeaf,score=score,correctBias=correctBias,verbose=verbose)
    #browser()
    #yScore=colnames(outImp)[2]
    #Gini_OOB[,k] = outImp[vars,yScore]
    
 
  }
  if (verbose<0)  return(list(inTree=inTree, outTree=outTree))
  #browser()
  if (agg == "none"){
    return(Gini_OOB)
  } else if (agg == "mean"){
    for (j in 0:4)
      VIbench[,paste0("IG_pgOOB",j)] = rowMeans(Gini_OOB[[j+1]], na.rm = TRUE)
   
  } else if (agg == "median"){
    for (j in 0:4)
      VIbench[,paste0("IG_pgOOB",j)] = apply(Gini_OOB[[j+1]],1,median, na.rm = TRUE)
  }
  for (j in 0:4)
    VIbench[,paste0("IG_pgOOB",j, "_sd")] = apply(Gini_OOB[[j+1]],1,sd, na.rm = TRUE)/sqrt(ncol(Gini_OOB[[j+1]]))
  
  
  return(VIbench)
  ### matrix with variable importance scores and their stdevs
}  , ex = function(){
  
  data("titanic_train", package = "rfVarImpOOB",  envir = environment())
  set.seed(123)
  ranRows=sample(nrow(titanic_train), 300)
  data=titanic_train[ranRows,]
  
  RF = randomForest::randomForest(formula = Survived ~ Sex + Pclass + PassengerId,
                                  data=data,
                                  ntree=5,importance=TRUE,
                                  mtry=3,keep.inbag=TRUE, 
                                  nodesize = 20)
  data$Survived = as.numeric(data$Survived)-1
  VI_Titanic = GiniImportanceForest(RF, data,ylab="Survived")
})


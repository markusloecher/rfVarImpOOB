GiniImportanceTree <- structure(function# computes Gini information gain for one tree from randomForest
### computes importance scores for an individual tree. 
### These can be based on Gini impurity or Accuracy or logloss 
(
  inbag, ##<< inbag data 
  outbag, ## << out of bag data
  RF, ##<< object returned by call to randomForest() 
  k, ##<< which tree
  #  tree, ##<< tree to work on
  ylabel = "Survived", ##<< name of dependent variable
  returnTree = FALSE, ##<< if TRUE returns the tree data frame otherwise the aggregated Gini importance grouped by split variables
  zeroLeaf = TRUE, ##<< if TRUE discard the information gain due to splits resulting in n=1
  score=c("PMDI21", "MDI","MDA","MIA")[1], 
  ### scoring method:PMDI=mean decrease penalized Gini impurity (note:the last digit is the exponent of the penalty!), 
  ### MDI=mean decrease impurity (Gini), MDA=mean decrease accuracy (permutation),
  ### MIA=mean increase accuracy
  Predictor=mean, ##<< function to estimate node prediction, such as Mode or mean or median. Alternatively, pass an array of numbers as replacement for the yHat column of tree
  correctBias = c(inbag=TRUE,outbag=TRUE), ##<< multiply by n/(n-1) for sample variance correction!
  ImpTypes= 0:5, ##<< which scores should be computed
  verbose=0 ##<< level of verbosity
){
  #brain dead "solution"  to the CRAN note issues,
  #see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  IG_acc=IG_gini=`split var`=NULL
  
  stopifnot(all(sort(base::unique(inbag[,ylabel])) == c(0,1))) 
  
  #preorder(1,OOB)
  #proper inbag keeping duplicates:
  #inbag = as.numeric(names((rf3a$inbag[rf3a$inbag[,k]>0,k])))
  tree = as.data.frame(randomForest::getTree(RF, k, labelVar = TRUE))
  tree$gini_index = NA
  tree$IG_gini= NA
  if (score =="MIA"){
    tree$Accuracy = NA
    tree$IG_acc = NA
  }
  # if (class(Predictor) =="function") {
  #   tree$yHat=sample(inbag[,ylabel],nrow(tree))
  # } else {
  #   tree$yHat= Predictor
  # }
  tree$node=tree$pIn=tree$pOut=tree$pIn_gini=tree$Out_gini=NA
  tree$nNodeIn=tree$nNodeOut=NA
  tree$pgOOB5=tree$pgOOB4=tree$pgOOB3=tree$pgOOB2=tree$pgOOB1=tree$pgOOB0=NA
  
  
  tree=preorder2(1,inbag,tree)#uses unmodified row names now !! (e.g. "6.1)
  tree$node_in = tree$node;tree$node=NA
  tree=preorder2(1,outbag,tree)#uses unmodified row names now !! (e.g. "6.1)
  tree$node_out = tree$node;tree$node=NA
  
  #if (verbose>1) browser()
  
  for (j in 1:nrow(tree)){
    yIn=inbag[unlist(tree[j,"node_in"]),ylabel]#NEEDS to be (0,1) !!!!
    nNodeIn = length(na.omit(yIn))
    yOut=outbag[unlist(tree[j,"node_out"]),ylabel]#NEEDS to be (0,1) !!!!
    nNodeOut = length(na.omit(yOut))
    pIn=tree$pIn[j] = mean(yIn, na.rm=TRUE)
    pOut=tree$pOut[j] = mean(yOut, na.rm=TRUE)
    
    if (grepl("PMDI", score) ) {#penalized Gini for validation data only:
      p=as.numeric(substr(score,5,5))#exponent of the penalty
      kind=as.numeric(substr(score,6,6))#kind of penalty
      if (verbose) cat("exponent of the penalty:",p,",type: ", kind,"\n")
      gIn =pIn*(1.0 -pIn)
      gOut = pOut*(1.0 -pOut)
      #if (verbose) cat("pOut, pIn", pOut, pIn, "gOut, gIn", gOut, gIn, "\n")
      
      #try corrected sample variance!
      if (correctBias[1]) if (nNodeIn>1) gIn = nNodeIn/(nNodeIn-1)*gIn
      if (correctBias[2]) if (nNodeOut>1) gOut = nNodeOut/(nNodeOut-1)*gOut
        #if (nNode<7) cat("nNode=",nNode, "pOut=",pOut,  ",corrected gOut, gIn", gOut, gIn, "\n")
        #if (verbose) cat( "corrected gOut, gIn", gOut, gIn, "\n")
      
      tree$pIn_gini[j] = gIn;tree$pOut_gini[j] = gOut
      tree$nNodeIn[j] = nNodeIn;tree$nNodeOut[j] = nNodeOut
      tree$pgOOB0[j] = 2*gOut  
      tree$pgOOB1[j] = 2*gOut + abs(pOut-pIn)^2
      tree$pgOOB2[j] = gOut + gIn + abs(pOut-pIn)^2
      tree$pgOOB3[j] = gOut + gIn + 0.5*abs(pOut-pIn)^2
      tree$pgOOB4[j] = 2*gIn
      tree$pgOOB5[j] = pOut*(1-2*pIn) + pIn^2 #Li et all (tree.interpreter)
    } 
  }
  #tree$gini_index[j] = round(gini_index(titanic_train[unlist(tree[j,"node"]),"Survived"]),4)
  if (score =="MIA"){
    for (j in 1:nrow(tree)){
      y=bag[unlist(tree[j,"node"]),ylabel]
      #yMajority = Mode(y)
      #tree$Accuracy[j] = round(mean(y==yMajority),9)
      tree$Accuracy[j] = Accuracy(y, yHat = Mode(na.omit(y)))#tree$yHat[j])
      #if (abs(tree$Accuracy[j])>1) browser()
    }
    
  }
  
  if (verbose>1) browser()
  #return(tree)
  tree$node=NA
  for (k in ImpTypes){
    tree$gini_index = tree[,paste0("pgOOB",k)]
    #for now we take 
    if (k==4 ){
      tree$node = tree$node_in
    } else if (k==0 | k==1){
      tree$node = tree$node_out
    } else {
      tree$node = tree$node_in
   #   for (j in 1:nrow(tree))
   #     tree$node[j] = list(c(tree$node_in[j], tree$node_out[j])) #for now that is all I can think of!
    }
    tree=InfGain(tree, zeroLeaf=zeroLeaf,score=score)
    tree[,paste0("IG_pgOOB",k)] = tree$IG_gini
  }
  
  
  if (returnTree) return(tree)
  
  InnerNodesOnly = tree[!is.na(tree[,c('split var')]),]
  
  if (score =="MIA") {
    df = dplyr::group_by(InnerNodesOnly[,c('split var', 'IG_acc')], `split var`)
    pivot = dplyr::summarise(df,`sum_IG_acc`=sum(`IG_acc`))
    
  } else{
    #browser()
    #pivot=aggregate(cbind(IG_pgOOB0, IG_pgOOB1, IG_pgOOB2, IG_pgOOB3, IG_pgOOB4) ~ ., data = tree[,c('split var', paste0("IG_pgOOB",ImpTypes))], FUN=sum)
    f = as.formula(paste0("cbind(",paste0("IG_pgOOB",ImpTypes,collapse=","),") ~ ."))
    pivot=aggregate(f, data = InnerNodesOnly[,c('split var', paste0("IG_pgOOB",ImpTypes))], FUN=sum,na.rm=TRUE)
    tmp=aggregate(IG_pgOOB0 ~ ., data = InnerNodesOnly[,c('split var', paste0("IG_pgOOB",0))], FUN=function(x) length(na.omit(x)))
    pivot$`count` = tmp[,2]
    #df = dplyr::group_by(tree[,c('split var', paste0("IG_pgOOB",ImpTypes))], `split var`)
    #pivot = dplyr::summarise(df,`sum_IG_gini`=sum(`IG_gini`))
  }
  
  #count = dplyr::count(df)
  #pivot$`count` = count$n
  pivot = as.data.frame(pivot)
  
  pivot = pivot[!is.na(pivot$`split var`),]
  rownames(pivot) = pivot[,1]
  #browser()
  #if (pivot["PassengerId","sum_IG_gini"] < 0) browser()
  if (any(is.na(pivot))) browser()
  
  return(pivot)
  ### if returnTree==TRUE returns the tree data frame otherwise the aggregated Gini importance grouped by split variables
} , ex = function(){
  
  rfTit = rfTitanic(nRows = 500,nodesize=10)
  rfTit$data$Survived = as.numeric(rfTit$data$Survived)-1
  k=1
  inbag = rep(rownames(rfTit$RF$inbag),time=rfTit$RF$inbag[,k])
  #trainBag=titanic_train[inbag,]
  trainBag=rfTit$data[inbag,];rownames(trainBag)  = 1:nrow(trainBag)
  outbag = names((rfTit$RF$inbag[rfTit$RF$inbag[,k]==0,k]))
  OOB = rfTit$data[outbag,];rownames(OOB)  = 1:nrow(OOB)
  
  Imp =GiniImportanceTree(trainBag,OOB, RF,k,ylabel="Survived")
                            
  Tree = GiniImportanceTree(trainBag,OOB, RF,k,ylabel="Survived",returnTree=TRUE)
  
  
})





preorder2 <- structure(function# recursive traversal of tree assigning row numbers of data for each node and leaf
### Recursive calling stops at leaf after which the function propagates back up the tree
(
  treeRow, ##<< current row of tree dataframe to be 
  bag,  ##<< The data for the current row
  tree,  ##<< tree (from randomForest::getTree to be traversed
 # ASSIGN=TRUE, ##<<  unused!! no more global assignments
  verbose=0  ##<< level of verbosity
){
  #print(tree[treeRow, ])
  #if (length(tree[treeRow, "status"])==0) browser()
  if (tree[treeRow, "status"] ==1){ #no leaf!
    left_daughter = tree[treeRow, 'left daughter']
    right_daughter = tree[treeRow, 'right daughter']
    
    daughters = splitBag(treeRow,bag,tree)# returns row names !!
    
    tree=preorder2(left_daughter,bag[daughters$left_daughter,],tree, verbose=verbose)
    tree=preorder2(right_daughter,bag[daughters$right_daughter,],tree,verbose=verbose)
  }
  #rowNamesOrig = as.character(floor(as.numeric(rownames(bag))))
  rowNamesOrig = rownames(bag)
 
  if (nrow(bag)>0) {
    tree$node[treeRow] <- list(rowNamesOrig)
  }
  if (verbose) cat(treeRow,":", rowNamesOrig,"\n")
  return(tree)
  ### tree with rownames in column node
}, ex = function(){
  data("titanic_train", package = "rfVarImpOOB",  envir = environment())
  
  set.seed(123)
  ranRows=sample(nrow(titanic_train), 300)
  
  RF = randomForest::randomForest(formula = Survived ~ Sex + Pclass + PassengerId,
                      data=titanic_train[ranRows,],
                      ntree=5,importance=TRUE,
                      mtry=3,keep.inbag=TRUE, 
                      nodesize = 1)
  k=2
  tree = randomForest::getTree(RF, k, labelVar = TRUE) 
  tree$node=NA
  inbag = rep(rownames(RF$inbag),time=RF$inbag[,k])
  #trainBag=titanic_train[inbag,]
  trainBag=titanic_train[ranRows,][inbag,]
  tree=preorder2(1,trainBag,tree)
})

splitBag <- function# splits the data from parent node into left and right children
### The function properly splits on factor levels
(
  treeRow, ##<< current row of tree dataframe to be 
  bag,  ##<< The data for the current row
  tree   ##<< tree (from randomForest::getTree)
){
  
  split_var = as.character(tree[treeRow,'split var'])
  if (is.factor(bag[,split_var])){
    #levels = unique(bag[,split_var])
    #levels = sort(levels, decreasing = TRUE)
    levels = levels(bag[,split_var])
    n = length(levels)#} else{
    if (n<2) n=0;#browser()
    #print(treeRow)
    binCode =rev(binaryLogic::as.binary(tree[treeRow,'split point'],n=n))
    split_point = try(as.character(levels[binCode])) #rF encodes male as 10, female as 01
    if (class(split_point) == "try-error") browser()
    mask =  bag[,split_var] %in% split_point
    left_daughter = rownames(bag)[mask]
    right_daughter = rownames(bag)[!mask]
    
  } else{
    split_point = tree[treeRow,'split point']
    mask = bag[,split_var] <= split_point
    left_daughter = rownames(bag)[mask]
    right_daughter = rownames(bag)[!mask]
    #if (verbose >0) {diagnostics(in_node_df, left_daughter, right_daughter, split_var)} 
  }
  return(list(left_daughter = (left_daughter), right_daughter = (right_daughter)))
  ### list with elements left_daughter, right_daughter
} 

InfGain <- function#computes information gain for each parent node in a tree
### information gain for each parent node in a tree
(
  tree, ##<< tree (from randomForest::getTree)
  total=TRUE,  ##<< if TRUE compute the sum instead of the mean
  zeroLeaf = TRUE, ##<< if TRUE discard the information gain due to splits resulting in n=1
  score=c("PMDI21","MDI","MDA","MIA")[1], ##<< scoring method:MDI=mean decrease impurity (Gini),MDA=mean decrease accuracy (permutation),MIA=mean increase accuracy
  verbose=0 ##<< level of verbosity
){
  #browser()
  IG_result = MIA_result = rep(NA, nrow(tree))
  tree$n_node = 0
  
  for (i in 1:nrow(tree)){
    en_node = tree[i, 'gini_index'] # get the entropy of that row
    n_node = tree[i, 'node'] %>% unlist %>%  na.omit %>%length # get the number of elements of the bag
    tree[i, 'n_node'] = n_node
    
    ld = tree[i, 'left daughter'] # row numeber of the left daughter
    rd = tree[i, 'right daughter'] # right daughter
    
    
    if (ld==0|rd==0){ # information gain for terminal node = NA
      IG=NA
      IG_result[i] = IG
      next
    }
    
    en_ld = tree[ld, 'gini_index'] # index of the left daughter
    en_rd = tree[rd, 'gini_index'] # right daughter
    
    nld = tree[ld, 'node']%>% unlist %>% na.omit %>% length  # get the number of elements of left daughter
    nrd = tree[rd, 'node']%>% unlist %>% na.omit %>% length # right
    dfCorrection=0#idea for later
    IG = en_node - sum(en_ld*(nld-dfCorrection), en_rd*(nrd-dfCorrection))/(n_node-2*dfCorrection) # formula of information gain
    if (total) IG = IG*(n_node-2*dfCorrection)
    #new idea: discount inf gain entirely if one of the children only has one element.
    if (zeroLeaf) IG = IG*ifelse(pmin(nld,nrd)<2,0,1)
    #if (length(IG)!=1) browser()
    IG_result[i] = IG
    
    if (score =="MIA"){
      MIA = sum(tree$Accuracy[ld]*nld, tree$Accuracy[rd]*nrd)/(n_node) -tree$Accuracy[i]
      MIA=MIA*n_node
      MIA_result[i] = MIA
      #if (is.na(MIA) | MIA == 0) browser()
    }
  }
  #browser()
  tree$`IG_gini`=round(IG_result,3)
  if (score =="MIA") {
    tree$IG_acc = round(MIA_result,3)
    tree[is.na(tree[,"IG_acc"]),"IG_acc"] = 0 
  }
  
  tree[is.na(tree[,'IG_gini']),'IG_gini'] = 0 
  
  
  return(tree)
  ### tree object augmented with information gain at each node
} 
 

rfTitanic <- structure(function#fit a random forest model on the titanic data
 ### convenience function to reduce overhead of repeatedly fitting RF to titanic data
 (
   formel = Survived ~ Sex + Pclass + PassengerId, ##<< formula
   nRows=500, ##<< subsample size
   ntree=10, ##<< number of trees
   mtry=3, ##<< mtry
   nodesize = 1 ##<< nodesize
 ){
   #library(titanic)
   titanic_train=NULL
   data("titanic_train", package = "rfVarImpOOB",  envir = environment())
   
   set.seed(123)
   ranRows=sample(nrow(titanic_train), nRows)
   train=titanic_train[ranRows,]
   
   rf = randomForest::randomForest(formula = formel,
                                   data=train,
                                   ntree=ntree,importance=TRUE,
                                   mtry=mtry,keep.inbag=TRUE, 
                                   nodesize = nodesize)
   
   return(list(RF=rf,data=train))
   
 } , ex = function(){
   rfTit = rfTitanic(nRows = 500,nodesize=10)
 })


Mode <- structure(function#computes the mode of an array
### returns the mode of a vector
(
  x ##<< vector to find mode of
) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}, ex = function(){
  
  Mode(rep(letters[1:3],1:3))
  Mode(c(TRUE,TRUE,FALSE))
  Mode(c(TRUE,TRUE,FALSE,FALSE))
})

if (0){
  library(inlinedocs)
  setwd("C:/Users/loecherm/Dropbox/Markus/Research/codeandstats/randomforest_investigation/")
  package.skeleton.dx("rfVarImpOOB");system("rm rfVarImpOOB/man/rfVarImpOOB-package.Rd");
  system("rm rfVarImpOOB/man/InfGain.Rd");#system("rm rfVarImpOOB/man/InOutBags.Rd")
}


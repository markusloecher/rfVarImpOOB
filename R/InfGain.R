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

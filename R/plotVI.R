plotVI = structure(function# creates barplots for variable importances
### creates barplots for variable importances
(
  VIbench, ##<< matrix with importance scores as returned by GiniImportanceForest
  order_by = 'Gini_OOB', ##<< how to order
  decreasing = TRUE##<< which direction to sort
){
  #brain dead solution  to the CRAN note issues,
  #see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  Variable=Value=Gini_inbag_sd=NULL
  
  # Step 1 Message the VIbench
  VIbench$Variable = rownames(VIbench)
  
  Gini_OOB = VIbench[,c('Gini_OOB','Variable')]
  Gini_inbag = VIbench[,c('Gini_inbag','Variable')]
  
  Gini_OOB$type = 'Gini_OOB'
  Gini_inbag$type = 'Gini_inbag'
  
  colnames(Gini_OOB)[1] = 'Value'
  colnames(Gini_inbag)[1] = 'Value'
  
  OOB_order = Gini_OOB[order(Gini_OOB$Value, decreasing = decreasing), 'Variable']
  
  inbag_order = Gini_inbag[order(Gini_inbag$Value, decreasing = decreasing), 'Variable']
  to_plot = rbind(Gini_OOB, Gini_inbag)
  
  if (order_by=='Gini_OOB'){
    to_plot$Variable = factor(to_plot$Variable, levels = rev(OOB_order))
  } else {
    to_plot$Variable = factor(to_plot$Variable, levels = rev(inbag_order))
  }
  
  # Step 2 Plot 
  plot = ggplot(to_plot, aes_string(x="Variable", y="Value", fill="type"))+
    geom_bar(position='dodge', stat='identity')+
    ylab("Weighted Sum of Gini Reduction") + 
    coord_flip() +
    ggtitle("Comparison of inbag and OOB Variable Importance (Gini Reduction)") 
  print(plot)
  
  if (decreasing) {
    sep = ' > '
  } else {
    sep = ' < '
  }
  cat('inbag_VI rank: ')
  cat(inbag_order, sep=sep)
  cat('\nOOB_VI rank  : ')
  cat(OOB_order, sep=sep)
}, ex = function(){
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
  plotVI(VI_Titanic,decreasing = TRUE)
  
})


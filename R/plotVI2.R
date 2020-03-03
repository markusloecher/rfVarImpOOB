plotVI2 = structure(function# creates barplots for variable importances
### creates barplots for variable importances including permutation scores
(
  VIbench, ##<< matrix with importance scores as returned by GiniImportanceForest
  col2plot = "IG_pgOOB2", #<< which column should be plotted
  Gini_inbag_col = "IG_pgOOB4", #<< which column should be interpreted as inbag impurity
  decreasing = TRUE, ##<< which direction to sort
  with_MDA=TRUE, ##<< also visualize mean decrease in accuracy (permutation importance)
  ordered_by = 'inbag', ##<< how to order
#  score = "Gini Importance", ##<< type of importance score: Gini, MIA,..
  horizontal = TRUE, ##<< horizontal barplot instead of vertical ?
  fill="order",##<< fill style for barplots; use e.g. shQuote("blue") to pass color strings
  labelSize=10, ##<< size of axis labels 
  nrow=3 ##<< number of rows of ploztz arrangement
){
  #brain dead solution  to the CRAN note issues,
  #see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  Gini_OOB_sd=Variable=Value=Gini_inbag_sd=hor=NULL
  
  if (horizontal) hor = coord_flip()
  
  if (missing(nrow)) if (with_MDA) nrow=2
  VIbench$Variable = rownames(VIbench)
  
  Gini_OOB = VIbench[,c('Variable', col2plot, paste0(col2plot, "_sd"))]
  Gini_inbag = VIbench[,c('Variable', Gini_inbag_col, paste0(Gini_inbag_col, "_sd"))]
  MDA = VIbench[,c('Variable', 'MeanDecreaseAccuracy')]
  
  colnames(Gini_OOB)[2] = 'Value'
  colnames(Gini_inbag)[2] = 'Value'
  colnames(MDA)[2] = 'Value'
  origOrder = 1:(nrow(MDA))
  
  if (ordered_by != 'orig'){
    OOB_order = Gini_OOB[order(Gini_OOB$Value, decreasing = decreasing), 'Variable']
    inbag_order = Gini_inbag[order(Gini_inbag$Value, decreasing = decreasing), 'Variable']
    MDA_order = MDA[order(MDA$Value, decreasing = decreasing), 'Variable']
    
    Gini_OOB$Variable = factor(Gini_OOB$Variable, levels=rev(OOB_order))
    Gini_inbag$Variable = factor(Gini_inbag$Variable, levels=rev(inbag_order))
    MDA$Variable = factor(MDA$Variable, levels =rev(MDA_order))
  } else {
    Gini_OOB$Variable = factor(Gini_OOB$Variable, levels=Gini_OOB$Variable)
    Gini_inbag$Variable = factor(Gini_inbag$Variable, levels=Gini_inbag$Variable)
    MDA$Variable = factor(MDA$Variable, levels =MDA$Variable)
  }
  
  if (ordered_by=='inbag'){
    order = inbag_order
  } else if (ordered_by=='OOB'){
    order = OOB_order
  } else  if (ordered_by=='MDA') {
    order = MDA_order
  } else {
    order = MDA$Variable
  }
  
  
  #ordered_by='inbag'
  Gini_OOB$Gini_OOB_sd = Gini_OOB[,paste0(col2plot, "_sd")]
  Gini_inbag$Gini_inbag_sd = Gini_inbag[,paste0(Gini_inbag_col, "_sd")]
  
  #browser()
  OOB_plot = ggplot2::ggplot(Gini_OOB)+
    geom_bar(aes_string(x="Variable", y="Value", fill=fill), stat='identity')+
    geom_errorbar(aes(x=Variable, ymin=Value-Gini_OOB_sd, ymax=Value+Gini_OOB_sd), width=0.4) +
    hor + #coord_flip() +
    theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank(),text = element_text(size=labelSize)) + 
    guides(fill=guide_legend(title="Variables:")) +
    ggtitle(paste0(col2plot," OOB"))
  
  inbag_plot = ggplot(Gini_inbag)+
    geom_bar(aes_string(x="Variable", y="Value", fill=fill), stat='identity')+
    geom_errorbar(aes(x=Variable, ymin=Value-Gini_inbag_sd, ymax=Value+Gini_inbag_sd), width=0.4)+
    hor + #coord_flip() + 
    theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank(),text = element_text(size=labelSize)) + 
    guides(fill=guide_legend(title="Variables:")) +
    ggtitle(paste0("MDI Inbag"))
  
  
  MDA_plot = ggplot(MDA)+
    geom_bar(aes_string(x="Variable", y="Value", fill=fill), stat='identity')+
    hor + #coord_flip() +
    theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank(),text = element_text(size=labelSize)) + 
    guides(fill=guide_legend(title="Variables:")) +
    ggtitle("MDA")
  
  if (with_MDA){
    plot = ggpubr::ggarrange(inbag_plot, MDA_plot, OOB_plot , widths = 40, heights = 40,nrow=nrow,ncol=4-nrow)#,common.legend = TRUE, legend = "bottom")
  } else{
    plot = ggpubr::ggarrange(inbag_plot, OOB_plot , widths = 40, heights = 40,nrow=nrow,ncol=3-nrow)#,common.legend = TRUE, legend = "bottom")
  }
  print(plot)
  # if (decreasing) {
  #   sep = ' > '
  # } else {
  #   sep = ' < '
  # }
  # cat('inbag_VI rank: ')
  # cat(inbag_order, sep=sep)
  # cat('\nOOB_VI rank  : ')
  # cat(OOB_order, sep=sep)
  
  if (with_MDA) invisible(list(inbag_plot=inbag_plot, MDA_plot=MDA_plot, OOB_plot=OOB_plot))
  if (!with_MDA) invisible(list(inbag_plot=inbag_plot, OOB_plot=OOB_plot))
  invisible(list(inbag_plot=inbag_plot, MDA_plot=MDA_plot, OOB_plot=OOB_plot))
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
  plotVI2(VI_Titanic,decreasing = TRUE)
  
})


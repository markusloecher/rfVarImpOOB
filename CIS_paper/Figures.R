library("latex2exp")
library(rfVarImpOOB)
library(ggplot2)
baseDir = "./"
setwd("~/research/randomforest_investigation/JournalPaper/CIS")
source(paste0(baseDir, "SimulateData.R"))

Figs1_2=FALSE
Figs3_4=FALSE
Figs5_6=TRUE

if (Figs1_2){
  naRows = is.na(titanic_train$Age)
  data2=titanic_train[!naRows,]
  rf3 =randomForest(Survived ~ Age + Sex + Pclass + PassengerId, data=data2, ntree=100,importance=TRUE,mtry=2, keep.inbag=TRUE)
  
  if (is.factor(data2$Survived)) data2$Survived = as.numeric(data2$Survived)-1
  VI_PMDI21 = GiniImportanceForest(rf3, data2,score="PMDI21",Predictor=mean)
  
  plots21 = plotVI2(VI_PMDI21, score="PMDI1", decreasing = TRUE,nrow=1)
  
  pl1 = ggpubr::ggarrange(plots21$inbag_plot, plots21$MDA_plot, widths = 40, heights = 40,nrow=1,ncol=2)
  
  pdf(paste0(baseDir, "figures/TitanicVarImp1.pdf"), height=1.5, width=5)
  par(cex=1.25)
  print(pl1)
  dev.off()
  
  VI_PMDI20 = GiniImportanceForest(rf3, data2,score="PMDI00",Predictor=mean)
  VI_PMDI22 = GiniImportanceForest(rf3, data2,score="PMDI22",Predictor=mean)
  VI_PMDI23 = GiniImportanceForest(rf3, data2,score="PMDI23",Predictor=mean)
  
  plots20 = plotVI2(VI_PMDI20, score="PMDI0", decreasing = TRUE,nrow=1)
  plots22 = plotVI2(VI_PMDI22, score="PMDI2", decreasing = TRUE,nrow=1)
  plots23 = plotVI2(VI_PMDI23, score="PMDI3", decreasing = TRUE,nrow=1)
  
  pl2 = ggpubr::ggarrange(plots20$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(0)}")), 
                          plots21$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(1)}")),
                          plots22$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(2)}")),
                          plots23$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(3)}")), 
                          widths = 40, heights = 40,nrow=1,ncol=4)
  
  pdf(paste0(baseDir, "figures/TitanicVarImpOOB.pdf") , height=2, width=8)
  par(cex=1.25)
  print(pl2)
  dev.off()
  
  save(VI_PMDI20,VI_PMDI21,VI_PMDI22,VI_PMDI23, file = "Figs2-3.rda")
  #plots21$OOB_plot 
}

###########################################################
if (Figs3_4){
  set.seed(123)
  load("../data/arabidopsis.rda")
  arabidopsis$sfe = sample(arabidopsis$fe)#shuffle fe as a random feature
  RF2 <- randomForest(edit ~ ., data = arabidopsis, keep.inbag=TRUE,
                      importance = TRUE, ntree = 50, mtry = 3)
  
  rfobj <- ranger(edit ~ ., data = arabidopsis, keep.inbag=TRUE,  importance = "impurity_corrected", 
                  num.trees =50, mtry = 3)
  
  VI_AIR = rfobj$variable.importance # Nembrinin et al, Actual impurity reduction importance
  
  AIR_df = cbind.data.frame(Value=VI_AIR, Variable=factor(VI_PMDI21$Variable,levels=VI_PMDI21$Variable))
  plots4 = ggplot2::ggplot(AIR_df)+
    geom_bar(aes_string(x="Variable", y="Value", fill = "Variable"), stat='identity')+ # coord_flip() +
    theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank(),text = element_text(size=10)) + 
    guides(fill=guide_legend(title="Variables:")) +
    ggtitle(paste0("AIR"))
  
  if (is.factor(arabidopsis$edit)) {
    print("converting factor to binary 0/1 label for GiniImportanceForest")
    arabidopsis$edit = as.numeric(arabidopsis$edit)-1
  }
  
  #look up font size axis labels ggplot and horizontal bar plots
  
  
  VI_PMDI21 = GiniImportanceForest(RF2, arabidopsis,score="PMDI21",Predictor=mean, ylabel="edit")
  VI_PMDI22 = GiniImportanceForest(RF2, arabidopsis,score="PMDI22",Predictor=mean, ylabel="edit")
  VI_PMDI23 = GiniImportanceForest(RF2, arabidopsis,score="PMDI23",Predictor=mean, ylabel="edit")
  
  rownames(VI_PMDI21)= gsub("X.", "-", rownames(VI_PMDI21), fixed = TRUE);rownames(VI_PMDI21)= gsub("X", "", rownames(VI_PMDI21), fixed = TRUE) 
  rownames(VI_PMDI22)= gsub("X.", "-", rownames(VI_PMDI22), fixed = TRUE);rownames(VI_PMDI22)= gsub("X", "", rownames(VI_PMDI22), fixed = TRUE) 
  rownames(VI_PMDI23)= gsub("X.", "-", rownames(VI_PMDI23), fixed = TRUE);rownames(VI_PMDI23)= gsub("X", "", rownames(VI_PMDI23), fixed = TRUE) 
  
  
  plots21 = plotVI2(VI_PMDI21, score="PMDI1", decreasing = TRUE,nrow=1,ordered_by="orig",horizontal = FALSE)
  plots22 = plotVI2(VI_PMDI22, score="PMDI2", decreasing = TRUE,nrow=1,ordered_by="orig",horizontal = FALSE)
  plots23 = plotVI2(VI_PMDI23, score="PMDI3", decreasing = TRUE,nrow=1,ordered_by="orig",horizontal = FALSE)
  
  pl3a = ggpubr::ggarrange(plots21$inbag_plot, plots21$MDA_plot, widths = 40, heights = 40,nrow=2,ncol=2)
  
  pdf(paste0(baseDir, "figures/arabidopsisVarImp1.pdf"), height=2.5, width=8)
  par(cex=1)
  print(pl3a)
  dev.off()
  
  pl3b = ggpubr::ggarrange(plots21$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(1)}")), plots22$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(2)}")),plots23$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(3)}")), plots4, widths = 40, heights = 40,nrow=4,ncol=1)
  
  pdf(paste0(baseDir, "figures/arabidopsisVarImpOOB.pdf"), height=8, width=8)
  par(cex=1)
  print(pl3b)
  dev.off()
  
  pl3b2x2 = ggpubr::ggarrange(plots21$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(1)}")), plots22$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(2)}")),plots23$OOB_plot+ ggtitle(TeX("PG_{OOB}^{(3)}")), plots4, widths = 40, heights = 40,nrow=2,ncol=2)
  
  pdf(paste0(baseDir, "figures/arabidopsisVarImpOOB_2x2.pdf"), height=5, width=8)
  par(cex=1)
  print(pl3b2x2)
  dev.off()
  
  VI_PMDI21$Variable = rownames(VI_PMDI21)
  MDA = VI_PMDI21[,c('Variable', 'MeanDecreaseAccuracy')]
  MDA$Variable = factor(MDA$Variable)
  MDA_plot = ggplot(MDA)+
    geom_bar(aes_string(x="Variable", y="MeanDecreaseAccuracy",fill="Variable"), fill="blue", stat='identity')+ NULL+
    #coord_flip() + 
    theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank()) + 
    guides(fill=guide_legend(title="Variables:")) +
    ggtitle("MDA") + theme(text = element_text(size=5))
  
  MDA_plot

}
##Simulation data

if (Figs5_6){
  #NullSim=list(OOB_sim_1=OOB_sim_1,OOB_sim_2=OOB_sim_2,MDI_sim=MDI_sim)
  NullSim=SimulateData(relevance = 0)#, verbose=1, M=2, ntree = 5)
  
  NullSim03 =SimulateData03(relevance = 0)#, verbose=1, M=2, ntree = 5)
  NullSimTI =SimulateDataTI(relevance = 0)
  #boxplot(t(NullSimTI),main="tree.interpreter",ylab="");grid();abline(h=0,col=2,lty=2)
  #################################NullSim.pdf, Fig5
  fname=paste0(baseDir, "figures/NullSim_",Sys.Date(),".pdf")
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
  
  par(mfrow=c(1,3),cex=0.85, mar=c(3,3,4,1))
  boxplot(t(NullSim03$MDI_sim),main=TeX("MDI"),ylab="");grid()
  mtext("Importance", side = 2,line=2)
  boxplot(t(NullSim03$OOB_sim_0),main=TeX("PG_{OOB}^{(0)}"),ylab="");grid()
  boxplot(t(NullSim03$OOB_sim_3),main=TeX("PG_{OOB}^{(3)}"),ylab="");grid()
  #################################PowerSim.pdf, Fig6
  PowerSim=PowerSim03=NULL
  
  PowerSim=SimulateData(relevance = 0.15)
  PowerSim03=SimulateData03(relevance = 0.15)
  PowerSimTI =SimulateDataTI(relevance = 0.15)
  boxplot(t(PowerSimTI),main="tree.interpreter",ylab="");grid();abline(h=0,col=2,lty=2)
  
  fname=paste0(baseDir, "figures/PowerSim_",Sys.Date(),".pdf")
  plotImp(PowerSim,PowerSim03,fname)
  #PowerSim=SimulateData(M=20,relevance = 0.15, prevResults=PowerSim)
  #PowerSim03=SimulateData03(M=20,relevance = 0.15, prevResults=PowerSim03)
  
  pdf(fname, height=3, width=8)
    par(mfrow=c(1,3),cex=0.85, mar=c(3,3,4,1))
    boxplot(PowerSim$MDI_sim,main=TeX("MDI"),ylab="");grid();
    mtext("Importance", side = 2,line=2)
    boxplot(PowerSim$OOB_sim_1,main=TeX("PG_{OOB}^{(1)}"),ylab="", ylim = c(-4,2));grid()
    boxplot(PowerSim$OOB_sim_2,main=TeX("PG_{OOB}^{(2)}"),ylab="", ylim = c(-4,2));grid()
  dev.off()
  
  #VI_sim_22 = GiniImportanceForest(RF3, data,score="PMDI22",Predictor=mean, ylabel="y")
  #VI_sim_23 = GiniImportanceForest(RF3, data,score="PMDI23",Predictor=mean, ylabel="y")
}


if (Fig8){
  load("OOB_mod_sims.rda")
  fname=paste0(baseDir, "figures/OOBhat_PowerNullSim_",Sys.Date(),".pdf")
  pdf(fname, height=3, width=8)
    par(mfrow=c(1,2),cex=1, mar=c(3,3,4,1))
    boxplot(t(NullSim03$OOB_sim_0),main="Null",ylab="", ylim = c(-1,1));grid();abline(h=0,col=2,lty=2)
    mtext(TeX("\\widehat{PG}_{OOB}^{(0)}"), side = 3,line=-1.75,outer=TRUE)
    boxplot(t(PowerSim03$OOB_sim_0),main="Power",ylab="", ylim = c(-1,2));grid();abline(h=0,col=2,lty=2)
    
  dev.off()
}  
l=paste0(c("12","34","56")[c(Figs1_2,Figs3_4,Figs5_6)],collapse="_")

#save.image(paste0("Figures",l,".rda"))
save.image(paste0("Figures",l,"_",Sys.Date(),".rda"))


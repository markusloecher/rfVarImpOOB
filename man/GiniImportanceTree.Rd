\name{GiniImportanceTree}
\alias{GiniImportanceTree}
\title{computes Gini information gain for one tree from randomForest}
\description{computes importance scores for an individual tree. 
These can be based on Gini impurity or Accuracy or logloss }
\usage{GiniImportanceTree(inbag, outbag, RF, k, ylabel = "Survived", 
    returnTree = FALSE, zeroLeaf = TRUE, score = c("PMDI21", 
        "MDI", "MDA", "MIA")[1], Predictor = mean, 
    correctBias = TRUE, verbose = 0)}
\arguments{
  \item{inbag}{inbag data}
  \item{outbag}{
}
  \item{RF}{object returned by call to randomForest()}
  \item{k}{which tree}
  \item{ylabel}{name of dependent variable}
  \item{returnTree}{if TRUE returns the tree data frame otherwise the aggregated Gini importance grouped by split variables}
  \item{zeroLeaf}{if TRUE discard the information gain due to splits resulting in n=1}
  \item{score}{scoring method:PMDI=mean decrease penalized Gini impurity (note:the last digit is the exponent of the penalty!), 
MDI=mean decrease impurity (Gini), MDA=mean decrease accuracy (permutation),
MIA=mean increase accuracy}
  \item{Predictor}{function to estimate node prediction, such as Mode or mean or median. Alternatively, pass an array of numbers as replacement for the yHat column of tree}
  \item{correctBias}{multiply by n/(n-1) for sample variance correction!}
  \item{verbose}{level of verbosity}
}

\value{if returnTree==TRUE returns the tree data frame otherwise the aggregated Gini importance grouped by split variables}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{

rfTit = rfTitanic(nRows = 500,nodesize=10)
rfTit$data$Survived = as.numeric(rfTit$data$Survived)-1
k=1
tmp <- InOutBags(rfTit$RF, rfTit$data, k)
IndivTree =getTree(rfTit$RF,k)
#plot(as.party(tmp))#does not work
InTree = GiniImportanceTree(tmp$inbag,rfTit$RF,k,returnTree=TRUE)
OutTree = GiniImportanceTree(tmp$outbag,rfTit$RF,k,returnTree=TRUE)

}

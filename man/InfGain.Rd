\name{InfGain}
\alias{InfGain}
\title{computes information gain for each parent node in a tree}
\description{information gain for each parent node in a tree}
\usage{InfGain(tree, total = TRUE, zeroLeaf = TRUE, score = c("PMDI21", 
    "MDI", "MDA", "MIA")[1], verbose = 0)}
\arguments{
  \item{tree}{tree (from randomForest::getTree)}
  \item{total}{if TRUE compute the sum instead of the mean}
  \item{zeroLeaf}{if TRUE discard the information gain due to splits resulting in n=1}
  \item{score}{scoring method:MDI=mean decrease impurity (Gini),MDA=mean decrease accuracy (permutation),MIA=mean increase accuracy}
  \item{verbose}{level of verbosity}
}

\value{tree object augmented with information gain at each node}

\author{Markus Loecher <Markus.Loecher@gmail.com>}






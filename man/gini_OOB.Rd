\name{gini_OOB}
\alias{gini_OOB}
\title{compute Gini impurity for binary values only}
\description{simple function to compute simple or penalized Gini impurity
The "penalty" compares the class probabilities \code{pOut} with a reference estimate \code{pIn} 
which would typically serve as a prediction (e.g. in a tree node).}
\usage{gini_OOB(gIn, gOut, k = 2, kind = 1, w = 2, correctBias = FALSE, 
    verbose = 0)}
\arguments{
  \item{gIn}{Gini Inbag}
  \item{gOut}{Gini Outbag}
  \item{k}{exponent of penalty term: abs(pOut-pIn)^k}
  \item{kind}{kind of penalty}
  \item{w}{weights, default is 2 if you pass just a single probability instead of the vector (p,1-p),}
  \item{correctBias}{multiply by n/(n-1) for sample variance correction!}
  \item{verbose}{level of verbosity}
}

\value{simple or penalized Gini impurity}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{
#Test binary case:

gini_OOB(0.5,0.5,kind=1)
gini_OOB(0.9,0.1,kind=1)
gini_OOB(0.1,0.9,kind=1)

gini_OOB(0.5,0.5,kind=2)
gini_OOB(0.9,0.1,kind=2)
gini_OOB(0.1,0.9,kind=2)


gini_OOB(0.5,0.5,kind=3)
gini_OOB(0.9,0.1,kind=3)
gini_OOB(0.1,0.9,kind=3)

}

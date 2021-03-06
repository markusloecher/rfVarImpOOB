\name{lpnorm}
\alias{lpnorm}
\title{Compute the Lp norm of a vector.}
\description{Compute the Lp norm of a vector.}
\usage{lpnorm(x, p = 2)}
\arguments{
  \item{x}{vector to compute the Lp norm of}
  \item{p}{parameter of p norm}
}

\value{Lp norm of a vector or NA}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{
lpnorm(1:10)
lpnorm(matrix(1:25, 5, 5))
lpnorm(split(1:25, rep(1:5, each = 5)))

lpnorm(1:10, 1)
lpnorm(matrix(1:25, 5, 5), 1)
lpnorm(split(1:25, rep(1:5, each = 5)), 1)

lpnorm(rnorm(10), 0)
lpnorm(matrix(rnorm(25), 5, 5), 0)
lpnorm(split(rnorm(25), rep(1:5, each = 5)), 0)

lpnorm(-5:5, Inf)
lpnorm(matrix(-25:-1, 5, 5), Inf)
lpnorm(split(-25:-1, rep(1:5, each = 5)), Inf)
}

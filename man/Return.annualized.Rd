\name{Return.annualized}
\alias{Return.annualized}

\encoding{latin1}
\title{calculate an annualized return for comparing instruments with different
length history}
\usage{
Return.annualized(R, scale = NA, geometric = TRUE)
}

\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{scale}{number of periods in a year (daily scale = 252, monthly scale =
12, quarterly scale = 4)}

\item{geometric}{utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns, default TRUE}
}


\description{
An average annualized return is convenient for comparing returns.
}

\details{
Annualized returns are useful for comparing two assets.  To do so, you must
scale your observations to an annual scale by raising the compound return to
the number of periods in a year, and taking the root to the number of total
observations.\cr
Bacon, Carl(2008) Practical Portfolio Performance Measurement and Attribution. Wiley , p. 6.\cr
See also documents in package \code{PerformanceAnalytics} for details.

}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


\examples{

  data(assetReturns)
	R=assetReturns[, -29]

  Return.annualized(R)

}



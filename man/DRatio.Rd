\name{DRatio}
\alias{DRatio}

\encoding{latin1}

\title{d ratio of the return distribution}
\usage{
DRatio(R)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

}

\description{
The d ratio is similar to the Bernado Ledoit ratio but inverted and
taking into account the frequency of positive and negative returns.
}
\details{
It has values between zero and infinity. It can be used to rank the
performance of portfolios. The lower the d ratio the better the
performance, a value of zero indicating there are no returns less than
zero and a value of infinity indicating there are no returns greater than zero.

\deqn{DRatio(R) = \frac{n_{d}*\sum^{n}_{t=1}{max(-R_{t},0)}}{n_{u}*\sum^{n}_{t=1}
{max(R_{t},0)}}}{DRatio(R) = nd*sum
(t=1..n)(max(-R(t),0)) / nu*sum(t=1..n)(max(R(t),0))}

where \eqn{n} is the number of observations of the entire series,
      \eqn{n_{d}} is the number of observations less than zero,
      \eqn{n_{u}} is the number of observations greater than zero
}
\examples{
  data(assetReturns)
	R=assetReturns[, -29]

  DRatio(R)



}
\references{
Carl Bacon, \emph{Practical portfolio performance measurement
and attribution}, second edition 2008 p.95\cr
See also package \code{PerformanceAnalytics}.
}
\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

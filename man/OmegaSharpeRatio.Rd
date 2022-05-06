\name{OmegaSharpeRatio}
\alias{OmegaSharpeRatio}

\encoding{latin1}

\title{Omega-Sharpe ratio of the return distribution}
\usage{
OmegaSharpeRatio(R, MAR = 0)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{MAR}{Minimum Acceptable Return, in the same periodicity as your
returns}

}

\description{
The Omega-Sharpe ratio is a conversion of the omega ratio to a ranking statistic
in familiar form to the Sharpe ratio.
}

\details{
To calculate the Omega-Sharpe ration we subtract the target (or Minimum
Acceptable Returns (MAR)) return from the portfolio return and we divide
it by the opposite of the Downside Deviation. \cr
Carl Bacon(2008) Practical portfolio performance measurement and attribution, second edition , p.95.\cr
See documents in  package \code{PerformanceAnalytics} for technical details.
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	R=assetReturns[, -29]
  OmegaSharpeRatio(R)

}

\name{SkewnessKurtosisRatio}
\alias{SkewnessKurtosisRatio}
\encoding{latin1}

\title{Skewness-Kurtosis ratio of the return distribution}
\usage{
SkewnessKurtosisRatio(R)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}


}
\description{
Skewness-Kurtosis ratio is the division of Skewness by Kurtosis.
}
\details{
It is used in conjunction with the Sharpe ratio to rank portfolios.
The higher the rate the better.\cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition,p.100.\cr
See also package \code{PerformanceAnalytics}.
}



\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	R=assetReturns[, -29]
  SkewnessKurtosisRatio(R)

}

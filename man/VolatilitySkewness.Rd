\name{VolatilitySkewness}
\alias{VolatilitySkewness}
\encoding{latin1}
\title{Volatility and variability of the return distribution}
\usage{
VolatilitySkewness(R, MAR = 0, stat = c("volatility", "variability"))
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{MAR}{Minimum Acceptable Return, in the same periodicity as your
returns}

\item{stat}{one of "volatility", "variability" indicating whether
to return the volatility skewness or the variability skweness}

}
\description{
Volatility skewness is a similar measure to omega but using the second
partial moment. It's the ratio of the upside variance compared to the
downside variance. Variability skewness is the ratio of the upside risk
compared to the downside risk.
}
\details{
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.97-98.\cr
See package \code{PerformanceAnalytics} for technical details.
}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


\examples{

  data(assetReturns)
	R=assetReturns[, -29]
  VolatilitySkewness(R, MAR=0, stat="volatility")

}

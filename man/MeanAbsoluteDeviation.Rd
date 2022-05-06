\name{MeanAbsoluteDeviation}
\alias{MeanAbsoluteDeviation}

\encoding{latin1}

\title{Mean absolute deviation of the return distribution}
\usage{
MeanAbsoluteDeviation(R)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

}
\description{
To calculate Mean absolute deviation we take the sum of the absolute value of the difference between the returns and the mean of the returns and we divide it by the number of returns.
}
\details{
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.62. \cr
See documents in  package \code{PerformanceAnalytics} for technical details.
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{
  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
	R=assetReturns[, -29]

MeanAbsoluteDeviation(R)

}

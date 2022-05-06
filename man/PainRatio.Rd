\name{PainRatio}
\alias{PainRatio}
\encoding{latin1}

\title{Pain ratio of the return distribution}
\usage{
PainRatio(R, Rf = 0)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rf}{risk free rate, in same period as your returns}

}
\description{
To calculate Pain ratio we divide the difference of the portfolio return
and the risk free rate by the Pain index
}
\details{

See also documents in package \code{PerformanceAnalytics} for technical details.
}
\examples{
  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
	R=assetReturns[, -29]
  PainRatio(R)

}
\references{
Carl Bacon(2008) Practical portfolio performance measurement and attribution, second edition, p.91
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

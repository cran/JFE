\name{KellyRatio}
\alias{KellyRatio}

\encoding{latin1}


\title{calculate Kelly criterion ratio (leverage or bet size) for a strategy}
\usage{
KellyRatio(R, Rf = 0)
}
\arguments{
\item{R}{a vector of returns to perform a mean over}

\item{Rf}{risk free rate, in same period as your returns}

}
\description{
Kelly criterion ratio (leverage or bet size) for a strategy.
}
\details{
The Kelly Criterion was identified by Bell Labs scientist John Kelly, and
applied to blackjack and stock strategy sizing by Ed Thorpe.\cr
As a performance metric, the Kelly Ratio is calculated retrospectively on a
particular investment as a measure of the edge that investment has over the
risk free rate.  It may be use as a stack ranking method to compare
investments in a manner similar to the various ratios related to the Sharpe
ratio. \cr
See also documents in package \code{PerformanceAnalytics} for technical details.
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


\examples{

  data(assetReturns)
	R=assetReturns[, -29]

  KellyRatio(R, Rf=0)

}

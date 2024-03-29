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
applied to blackjack and stock strategy sizing by Ed Thorpe.

The Kelly ratio can be simply stated as: \dQuote{bet size is the ratio of
edge over odds.} Mathematically, you are maximizing log-utility.  As such,
the Kelly criterion is equal to the expected excess return of the strategy
divided by the expected variance of the excess return, or

\deqn{leverage=\frac{(\overline{R}_{s}-R_{f})}{StdDev(R)^{2}}}{leverage =
(mean(R)-Rf=0)/StdDev(R)^2}

As a performance metric, the Kelly Ratio is calculated retrospectively on a
particular investment as a measure of the edge that investment has over the
risk free rate.  It may be use as a stack ranking method to compare
investments in a manner similar to the various ratios related to the Sharpe
ratio.
}

\examples{

  data(assetReturns)
	R=assetReturns[, -29]

  KellyRatio(R, Rf=0)

}
\references{
Thorp, Edward O. (1997; revised 1998). The Kelly Criterion in
Blackjack, Sports Betting, and the Stock Market. \cr
See also package \code{PerformanceAnalytics}.
}
\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


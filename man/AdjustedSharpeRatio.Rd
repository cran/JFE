\name{AdjustedSharpeRatio}
\alias{AdjustedSharpeRatio}
\encoding{latin1}

\title{Adjusted Sharpe ratio of the return distribution}
\usage{
AdjustedSharpeRatio(R, Rf = 0, FUN = "StdDev")
}

\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rf}{the risk free rate}

\item{FUN}{one of "StdDev" or "VaR" or "ES" to use as the denominator for unadjusted Sharpe ratio, default="StdDev"}

}

\description{
Adjusted Sharpe ratio was introduced by Pezier and White (2006) to adjusts
for skewness and kurtosis by incorporating a penalty factor for negative skewness
and excess kurtosis.
}

\details{

See documents in package \code{PerformanceAnalytics} for technical details. \cr
Carl Bacon(2008) Practical portfolio performance measurement and attribution, second edition, p.99.

}
\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


\examples{
    data(assetReturns)

AdjustedSharpeRatio(assetReturns)

}

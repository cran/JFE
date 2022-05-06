\name{SharpeRatio}
\alias{SharpeRatio}
\encoding{latin1}
\title{calculate a traditional or modified Sharpe Ratio of Return over StdDev or
VaR or ES}

\usage{
SharpeRatio(R, Rf = 0, alpha = 0.05, FUN="StdDev",annualize=FALSE)

}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rf}{risk free rate, in same period as your returns}

\item{alpha}{Tail probability for VaR or ES, default alpha=.05}

\item{FUN}{one of "StdDev" or "VaR" or "ES" to use as the denominator}

\item{annualize}{if TRUE, annualize the measure, default FALSE}

}
\description{
The Sharpe ratio is simply the return per unit of risk (represented by
variability).  In the classic case, the unit of risk is the standard
deviation of the returns.
}

\details{

Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk
Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002,
v 5.\cr
Sharpe, W.F. The Sharpe Ratio (1994) Journal of Portfolio Management,Fall, 49-58.\cr
See also package \code{PerformanceAnalytics} for details.

}




\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	R=assetReturns[, -29]

SharpeRatio(R)

}

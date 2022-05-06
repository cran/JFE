\name{DownsideDeviation}
\alias{DownsideDeviation}
\encoding{latin1}
\title{downside risk (deviation, variance) of the return distribution}
\usage{
DownsideDeviation(R, MAR = 0, method = c("full", "subset"),potential = FALSE)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{MAR}{Minimum Acceptable Return, in the same periodicity as your
returns}

\item{method}{one of "full" or "subset", indicating whether to use the
length of the full series or the length of the subset of the series below
the MAR as the denominator, defaults to "full"}

\item{potential}{if TRUE, calculate downside potential instead, default
FALSE}

}
\description{
Downside deviation, semideviation, and semivariance are measures of downside
risk.
}
\details{
Downside deviation, similar to semi deviation, eliminates positive returns
when calculating risk.  Instead of using the mean return or zero, it uses
the Minimum Acceptable Return as proposed by Sharpe (which may be the mean
historical return or zero). It measures the variability of underperformance
below a minimum targer rate. The downside variance is the square of the downside
potential.

To calculate it, we take the subset of returns that are less than the target
(or Minimum Acceptable Returns (MAR)) returns and take the differences of
those to the target.  We sum the squares and divide by the total number of
returns to get a below-target semi-variance. \cr
See also documents in package \code{PerformanceAnalytics} for technical details.
Sortino, F. and Price, L.(1994) Performance Measurement in a Downside
Risk Framework. Journal of Investing. Fall, 59-65. \cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution,
second edition.\cr
Plantinga, A., van der Meer, R. and Sortino, F. (2001) The Impact of Downside Risk
on Risk-Adjusted Performance of Mutual Funds in the Euronext Markets. July
19.
}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
	R=assetReturns[, -29]
  DownsideDeviation(R, MAR = 0)

}

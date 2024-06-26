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
returns to get a below-target semi-variance.

\deqn{DownsideDeviation(R , MAR) = \delta_{MAR} = \sqrt{\sum^{n}_{t=1}\frac{min[(R_{t} - MAR), 0]^2}{n}}}{DownsideDeviation(R, MAR) = sqrt(1/n * sum(t=1..n)((min(R(t)-MAR, 0))^2))}

\deqn{ DownsideVariance(R, MAR) = \sum^{n}_{t=1}\frac{min[(R_{t} - MAR), 0]^2}{n}}{DownsideVariance(R, MAR) = 1/n * sum(t=1..n)((min(R(t)-MAR, 0))^2)}

\deqn{DownsidePotential(R, MAR) = \sum^{n}_{t=1}\frac{min[(R_{t} - MAR), 0]} {n}}{DownsidePotential(R, MAR) =  1/n * sum(t=1..n)(min(R(t)-MAR, 0))}

where \eqn{n} is either the number of observations of the entire series or
the number of observations in the subset of the series falling below the
MAR.

SemiDeviation or SemiVariance is a popular alternative downside risk measure
that may be used in place of standard deviation or variance. SemiDeviation
and SemiVariance are implemented as a wrapper of DownsideDeviation with
MAR=mean(R).

In many functions like Markowitz optimization, semideviation may be
substituted directly, and the covariance matrix may be constructed from
semideviation or the vector of returns below the mean rather than from
variance or the full vector of returns.

In semideviation, by convention, the value of \eqn{n} is set to the full
number of observations. In semivariance the the value of \eqn{n} is set to
the subset of returns below the mean.  It should be noted that while this is
the correct mathematical definition of semivariance, this result doesn't
make any sense if you are also going to be using the time series of returns
below the mean or below a MAR to construct a semi-covariance matrix for
portfolio optimization.

Sortino recommends calculating downside deviation utilizing a continuous
fitted distribution rather than the discrete distribution of observations.
This would have significant utility, especially in cases of a small number
of observations. He recommends using a lognormal distribution, or a fitted
distribution based on a relevant style index, to construct the returns below
the MAR to increase the confidence in the final result.  Hopefully, in the
future, we'll add a fitted option to this function, and would be happy to
accept a contribution of this nature.
}
\examples{

  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
	R=assetReturns[, -29]
  DownsideDeviation(R, MAR = 0)



}
\references{
Sortino, F. and Price, L. Performance Measurement in a Downside
Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65. \cr
Carl Bacon, \emph{Practical portfolio performance measurement and attribution},
second edition 2008

Plantinga, A., van der Meer, R. and Sortino, F. The Impact of Downside Risk
on Risk-Adjusted Performance of Mutual Funds in the Euronext Markets. July
19, 2001. Available at SSRN:\url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=277352} \cr
see especially end note 10 \url{https://en.wikipedia.org/wiki/Semivariance}. \cr

See also package \code{PerformanceAnalytics}.
}
\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

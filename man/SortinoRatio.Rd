\name{SortinoRatio}
\alias{SortinoRatio}
\encoding{latin1}
\title{calculate Sortino Ratio of performance over downside risk}
\usage{
SortinoRatio(R, MAR = 0)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{MAR}{Minimum Acceptable Return, in the same periodicity as your
returns}

}
\description{
Sortino proposed an improvement on the Sharpe Ratio to better account for
skill and excess performance by using only downside semivariance as the
measure of risk.
}
\details{
Sortino contends that risk should be measured in terms of not meeting the
investment goal.  This gives rise to the notion of Minimum
Acceptable Return or MAR.  All of Sortino's proposed measures include the
MAR, and are more sensitive to downside or extreme risks than measures that
use volatility(standard deviation of returns) as the measure of risk.\cr
Choosing the MAR carefully is very important, especially when comparing
disparate investment choices.  If the MAR is too low, it will not adequately
capture the risks that concern the investor, and if the MAR is too high, it
will unfavorably portray what may otherwise be a sound investment.  When
comparing multiple investments, some papers recommend using the risk free
rate as the MAR.  Practitioners may wish to choose one MAR for consistency,
several standardized MAR values for reporting a range of scenarios, or a MAR
customized to the objective of the investor.\cr
Sortino, F. and Price, L. (1994) Performance Measurement in a Downside
Risk Framework.Journal of Investing. Fall, 59-65.\cr
See also package \code{PerformanceAnalytics}.

}
\examples{

  data(assetReturns)
	R=assetReturns[, -29]

  SortinoRatio(R)

}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


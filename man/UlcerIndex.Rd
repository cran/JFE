\name{UlcerIndex}
\alias{UlcerIndex}
\encoding{latin1}

\title{calculate the Ulcer Index}
\usage{
UlcerIndex(R)
}
\arguments{
\item{R}{a vector, matrix, data frame, timeSeries or zoo object of asset
returns}

}
\description{
Developed by Peter G. Martin in 1987 (Martin and McCann, 1987) and named
for the worry caused to the portfolio manager or investor.  This is
similar to drawdown deviation except that the impact of the duration of
drawdowns is incorporated by selecting the negative return for each
period below the previous peak or high water mark.  The impact of long,
deep drawdowns will have significant impact because the underperformance
since the last peak is squared.
}
\details{
UI = sqrt(sum[i=1,2,...,n](D'_i^2/n)) where
D'_i = drawdown since previous peak in period i

DETAILS:
This approach is sensitive to the frequency of the time periods involved
and penalizes managers that take time to recover to previous highs.

REFERENCES:
Martin, P. and McCann, B. (1989) The investor's Guide to Fidelity Funds:
Winning Strategies for Mutual Fund Investors.  John Wiley & Sons, Inc.

See also package \code{PerformanceAnalytics}.
}

\examples{

  data(assetReturns)
	R=assetReturns[, -29]
  maxDrawdown(R)

}
\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

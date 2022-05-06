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

This approach is sensitive to the frequency of the time periods involved
and penalizes managers that take time to recover to previous highs.\cr

Martin, P. and McCann, B. (1989) The investor's Guide to Fidelity Funds:
Winning Strategies for Mutual Fund Investors.  John Wiley & Sons, Inc.\cr
See also package \code{PerformanceAnalytics}.
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	R=assetReturns[, -29]
  maxDrawdown(R)

}

\name{DRatio}
\alias{DRatio}

\encoding{latin1}

\title{d ratio of the return distribution}
\usage{
DRatio(R)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

}

\description{
The d ratio is similar to the Bernado Ledoit ratio but inverted and
taking into account the frequency of positive and negative returns.
}

\details{
It has values between zero and infinity. It can be used to rank the
performance of portfolios. The lower the d ratio the better the
performance, a value of zero indicating there are no returns less than
zero and a value of infinity indicating there are no returns greater than zero.\cr
See also documents in package \code{PerformanceAnalytics} for technical details.\cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.95
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


\examples{
  data(assetReturns)
	R=assetReturns[, -29]

  DRatio(R)

}

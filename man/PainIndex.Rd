\name{PainIndex}
\alias{PainIndex}

\encoding{latin1}

\title{Pain index of the return distribution}
\usage{
PainIndex(R)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

}
\description{
The pain index is the mean value of the drawdowns over the entire
analysis period. The measure is similar to the Ulcer index except that
the drawdowns are not squared.  Also, it's different than the average
drawdown, in that the numerator is the total number of observations
rather than the number of drawdowns.
}
\details{
Visually, the pain index is the area of the region that is enclosed by
the horizontal line at zero percent and the drawdown line in the
Drawdown chart.\cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.89
Becker, Thomas (2006) Zephyr Associates.\cr
See also documents in package \code{PerformanceAnalytics} for technical details.
}
\examples{
  data(assetReturns)
	R=assetReturns[, -29]
# Not run
# PainIndex(R)

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

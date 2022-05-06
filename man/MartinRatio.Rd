\name{MartinRatio}
\alias{MartinRatio}

\encoding{latin1}

\title{Martin ratio of the return distribution}
\usage{
MartinRatio(R, Rf = 0)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rf}{risk free rate, in same period as your returns}

}
\description{
To calculate Martin ratio we divide the difference of the portfolio return
and the risk free rate by the Ulcer index
}

\details{
Carl Bacon, \emph{Practical portfolio performance measurement
and attribution}, second edition 2008 p.91.\cr
See also package \code{PerformanceAnalytics}.
}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{
  data(assetReturns)
	R=assetReturns[, -29]

# Not run
# MartinRatio(R)

}

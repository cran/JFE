\name{ProspectRatio}
\alias{ProspectRatio}

\encoding{latin1}
\title{Prospect ratio of the return distribution}
\usage{
ProspectRatio(R, MAR)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{MAR}{the minimum acceptable return}

}
\description{
Prospect ratio is a ratio used to penalise loss since most people feel loss
greater than gains.
}

\details{
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.100.\cr
See also package \code{PerformanceAnalytics}
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{
  data(assetReturns)
	R=assetReturns[, -29]

  ProspectRatio(R, MAR=0)

}

\name{M2Sortino}
\alias{M2Sortino}

\encoding{latin1}

\title{M squared for Sortino of the return distribution}
\usage{
M2Sortino(Ra, Rb, MAR = 0)
}
\arguments{
\item{Ra}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset return}

\item{Rb}{return vector of the benchmark asset}

\item{MAR}{the minimum acceptable return}

}
\description{
M squared for Sortino is a M^2 calculated for Downside risk instead of Total Risk
}

\details{
See documents in package \code{PerformanceAnalytics} for technical details. \cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.102-103.
}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	Ra=assetReturns[, -29]
	Rb=assetReturns[,29] #DJI

M2Sortino(Ra, Rb, MAR=0)

}

\name{BernardoLedoitRatio}
\alias{BernardoLedoitRatio}

\encoding{latin1}

\title{Bernardo and Ledoit ratio of the return distribution}
\usage{
BernardoLedoitRatio(R)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

}
\description{
To calculate Bernardo and Ledoit ratio we take the sum of the subset of
returns that are above 0 and we divide it by the opposite of the sum of
the subset of returns that are below 0
}

\details{
See documents in package \code{PerformanceAnalytics} for technical details.\cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.95.
}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{
    data(assetReturns)
BernardoLedoitRatio(R=assetReturns)

}

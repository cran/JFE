\name{InformationRatio}
\alias{InformationRatio}

\encoding{latin1}

\title{InformationRatio = ActivePremium/TrackingError}
\usage{
InformationRatio(Ra, Rb, scale = NA)
}
\arguments{
\item{Ra}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rb}{return vector of the benchmark asset}

\item{scale}{number of periods in a year (daily scale = 252, monthly scale =
12, quarterly scale = 4)}
}
\description{
The Active Premium divided by the Tracking Error.
}
\details{
InformationRatio = ActivePremium/TrackingError

This relates the degree to which an investment has beaten the benchmark to
the consistency with which the investment has beaten the benchmark.\cr
See also documents in package \code{PerformanceAnalytics} for technical details.\cr
Sharpe, W.F. The Sharpe Ratio (1994) Journal of Portfolio Management,Fall, 49-58
William Sharpe now recommends \code{InformationRatio} preferentially
to the original \code{SharpeRatio}.
}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	Ra=assetReturns[, -29]
	Rb=assetReturns[,29] #DJI

  InformationRatio(Ra, Rb)

}

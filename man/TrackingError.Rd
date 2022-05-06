\name{TrackingError}
\alias{TrackingError}
\encoding{latin1}
\title{Calculate Tracking Error of returns against a benchmark}
\usage{
TrackingError(Ra, Rb, scale = NA)
}
\arguments{
\item{Ra}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rb}{return vector of the benchmark asset}

\item{scale}{number of periods in a year (daily scale = 252, monthly scale =
12, quarterly scale = 4)}
}
\description{
A measure of the unexplained portion of performance relative to a benchmark.
}
\details{
Tracking error is calculated by taking the square root of the average of the
squared deviations between the investment's returns and the benchmark's
returns, then multiplying the result by the square root of the scale of the
returns.\cr
Sharpe, W.F. The Sharpe Ratio(1994) Journal of Portfolio Management,Fall, 49-58.\cr
See also package \code{PerformanceAnalytics}.


}
\examples{
  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
	Ra=assetReturns[, -29]
	Rb=assetReturns[,29] #DJI

  TrackingError(Ra, Rb)

}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

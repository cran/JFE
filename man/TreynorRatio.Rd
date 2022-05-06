\name{TreynorRatio}
\alias{TreynorRatio}
\encoding{latin1}
\title{calculate Treynor Ratio or modified Treynor Ratio of excess return over CAPM beta}
\usage{
TreynorRatio(Ra, Rb, Rf = 0, scale = NA, modified = FALSE)
}
\arguments{
\item{Ra}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rb}{return vector of the benchmark asset}

\item{Rf}{risk free rate, in same period as your returns}

\item{scale}{number of periods in a year (daily scale = 252, monthly scale =
12, quarterly scale = 4)}

\item{modified}{a boolean to decide whether to return the Treynor ratio or
Modified Treynor ratio}
}

\description{
The Treynor ratio is similar to the Sharpe Ratio, except it uses beta as the
volatility measure (to divide the investment's excess return over the beta).
}

\details{
To calculate modified Treynor ratio, we divide the numerator by the systematic risk
instead of the beta.\cr
See also package \code{PerformanceAnalytics}.
}



\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
	Ra=assetReturns[, -29]
	Rb=assetReturns[,29] #DJI

  TreynorRatio(Ra, Rb)

}

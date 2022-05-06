\name{CAPM.jensenAlpha}
\alias{CAPM.jensenAlpha}

\encoding{latin1}

\title{Jensen's alpha of the return distribution}
\usage{
CAPM.jensenAlpha(Ra, Rb, Rf = 0)
}
\arguments{
\item{Ra}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rb}{return vector of the benchmark asset}

\item{Rf}{risk free rate, in same period as your returns}

}
\description{
The Jensen's alpha is the intercept of the regression equation in the Capital
Asset Pricing Model and is in effect the exess return adjusted for systematic risk.
}

\details{
See documents in package \code{PerformanceAnalytics} for technical details.\cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition 2008 p.72}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


\examples{

  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
	Ra=assetReturns[, -29]
	Rb=assetReturns[,29] #DJI

CAPM.jensenAlpha(Ra, Rb)

}

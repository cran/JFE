\name{BurkeRatio}
\alias{BurkeRatio}

\encoding{latin1}

\title{Burke ratio of the return distribution}
\usage{
BurkeRatio(R, Rf = 0, modified = FALSE)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rf}{the risk free rate}

\item{modified}{a boolean to decide which ratio to calculate between Burke ratio and modified Burke ratio.}


}
\description{
To calculate Burke ratio we take the difference between the portfolio
return and the risk free rate and we divide it by the square root of the
sum of the square of the drawdowns. To calculate the modified Burke ratio
we just multiply the Burke ratio by the square root of the number of datas.
}

\details{
See documents in package \code{PerformanceAnalytics} for technical details.\cr
Carl Bacon (2008) Practical portfolio performance measurement and attribution, second edition, p.90-91.
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


\examples{
  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
  BurkeRatio(assetReturns,Rf=0)

}

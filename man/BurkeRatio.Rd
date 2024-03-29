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
\deqn{Burke Ratio = \frac{r_P - r_F}{\sqrt{\sum^{d}_{t=1}{D_t}^2}}}{Burke Ratio = (Rp - Rf) / (sqrt(sum(t=1..n)(Dt^2)))}

\deqn{Modified Burke Ratio = \frac{r_P - r_F}{\sqrt{\sum^{d}_{t=1}\frac{{D_t}^2}{n}}}}{Modified Burke Ratio = (Rp - Rf) / (sqrt(sum(t=1..n)(Dt^2 / n)))}

where \eqn{n} is the number of observations of the entire series, \eqn{d} is number of drawdowns, \eqn{r_P} is the portfolio return, \eqn{r_F} is the risk free rate and \eqn{D_t} the \eqn{t^{th}} drawdown.
}
\examples{
  data(assetReturns)
	assetReturns=assetReturns["2011::2018"] #short sample for fast example
  BurkeRatio(assetReturns,Rf=0)

}
\references{
Carl Bacon, \emph{Practical portfolio performance measurement
and attribution}, second edition 2008 p.90-91.\cr
See aslo package \code{PerformanceAnalytics}.
}
\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

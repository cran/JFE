\name{CalmarRatio}
\alias{CalmarRatio}
\alias{SterlingRatio}
\encoding{latin1}

\title{calculate a Calmar or Sterling reward/risk ratio

Calmar and Sterling Ratios are yet another method of creating a
risk-adjusted measure for ranking investments similar to the
\code{SharpeRatio}.

}

\description{
Both the Calmar and the Sterling ratio are the ratio of annualized return
over the absolute value of the maximum drawdown of an investment. The
Sterling ratio adds an excess risk measure to the maximum drawdown,
traditionally and defaulting to 0.1.
}

\usage{
CalmarRatio(R, scale = NA)

SterlingRatio(R, scale = NA, excess = 0.1)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{scale}{number of periods in a year (daily scale = 252, monthly scale =
12, quarterly scale = 4)}

\item{excess}{for Sterling Ratio, excess amount to add to the max drawdown,
traditionally and default 0.1}
}


\details{
It is also traditional to use a three year return series for these
calculations, although the functions included here make no effort to
determine the length of your series.  If you want to use a subset of your
series, you'll need to truncate or subset the input data to the desired
length.\cr
See also package \code{PerformanceAnalytics} for technical details.\cr
Bacon, Carl (2008) Practical Portfolio Performance Measurement and Attribution. Wiley.
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}

\examples{

  data(assetReturns)
	R=assetReturns[, -29]

#    SterlingRatio(R)

}

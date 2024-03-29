\name{Return.annualized}
\alias{Return.annualized}

\encoding{latin1}
\title{calculate an annualized return for comparing instruments with different
length history}
\usage{
Return.annualized(R, scale = NA, geometric = TRUE)
}

\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{scale}{number of periods in a year (daily scale = 252, monthly scale =
12, quarterly scale = 4)}

\item{geometric}{utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns, default TRUE}
}


\description{
An average annualized return is convenient for comparing returns.
}
\details{
Annualized returns are useful for comparing two assets.  To do so, you must
scale your observations to an annual scale by raising the compound return to
the number of periods in a year, and taking the root to the number of total
observations:
\deqn{prod(1+R_{a})^{\frac{scale}{n}}-1=\sqrt[n]{prod(1+R_{a})^{scale}}-1}{prod(1
+ Ra)^(scale/n) - 1}

where scale is the number of periods in a year, and n is the total number of
periods for which you have observations.

For simple returns (geometric=FALSE), the formula is:

\deqn{\overline{R_{a}} \cdot scale}{mean(R)*scale}
}
\examples{

  data(assetReturns)
	R=assetReturns[, -29]

  Return.annualized(R)

}
\references{
Bacon, Carl. \emph{Practical Portfolio Performance Measurement
and Attribution}. Wiley. 2004. p. 6\cr
See also package \code{PerformanceAnalytics}.
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}


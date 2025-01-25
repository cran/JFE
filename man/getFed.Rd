\name{getFed}
\alias{getFed}

\encoding{latin1}

\title{Download financial and economic time series data from the Fed}

\description{
It downloads financial and economic time series data from the Fed.
}
\usage{
getFed(var.name="UNRATE", freq="Monthly", do.plot=TRUE, plot.name=NULL)
}

\arguments{
  \item{var.name}{
The name of variable as used by <"https://fred.stlouisfed.org">. For example, "UNRATE" denotes unemployment rate, "CPIAUCSL" is monthly consumer price index, "GDP" is Gross domestic product, and "ICSA" is the initial claim seasonally adjusted. User please check with the web for symbol.
}

  \item{freq}{The time series frequency, match Fed label, for example,"Monthly","Quarterly".

}

  \item{do.plot}{Whether to plot the retrieved data, the default is TRUE. Please set this value to be FALSE to avoid too many plotting, if you want to loop more downloads.}


  \item{plot.name}{if do.plot=TRUE, the main label appeared. Default is var.name.}

}


\details{

This function connects with <"https://fred.stlouisfed.org"> and downloads the specified market data. The rownames of downloaded data is embedded by the timestamp already.
}


\value{

  \item{data}{The data retrieved.}

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{
#To save time, the example below is commented.
#getFed(var.name="UNRATE", freq="Monthly",
#do.plot=TRUE,
#plot.name="Monthly US Unemployment Rate")$data

}

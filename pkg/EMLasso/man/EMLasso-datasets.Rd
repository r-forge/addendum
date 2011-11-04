\name{EMLasso-datasets}
\alias{EMLasso-datasets}
\alias{aDfr}
\alias{aDfr.MD}
\alias{eml17}
\alias{eml20}
\alias{emlcvfit}
\alias{y}
\docType{data}
\title{EMLasso demo datasets and objects}
\description{EMLasso demo datasets and objects}
\format{
	\enumerate{
		\item aDfr: \code{\link{data.frame}} of 100 observations and 100 columns: 90 3 level factors (cat1 till 
							cat90) and 10 continuous (cnt1 till cnt10)
		\item aDfr.MD: same as \code{aDfr}, but with some missing data
		\item aDfr.MD: same as \code{aDfr}, but with some missing data
		\item eml17, eml20: \code{\link{cv.1l.emlasso}} objects from fitting EMLasso to \code{aDfr.MD} and \code{y}
		\item emlcvfit: \code{\link{EMLasso.lognet.cv}} object from fitting EMLasso to \code{aDfr.MD} and \code{y}
		\item y: factor of length 100 with 2 levels, "sick" and "healthy".
	}
}
\usage{
	data(emlcvfit)
}
\keyword{datasets}




\name{GLoMo-class}
\alias{GLoMo-class}
\docType{S3class}
\title{
Structure of GLoMo object
}
\description{
Structure of GLoMo object

}
\details{
NOTE: where I write dataset, this could either be a \code{\link{data.frame}} or \code{\link{numdfr}} object.
\enumerate{
	\item \code{uid}: character vector: each item is a unique identifier of a cell. These get longer with more factor columns in the dataset
	\item \code{pihat}: probability of each cell
	\item \code{omegahat}: matrix of the continuous columns within cells (note: homoscedastic)
	\item \code{orgdatadim}: dimensions of the dataset used to create this
	\item \code{uniqueFactorCombinationsAndContinuousMeans}: for each uid, the matching factor levels + the means in that cell for the continuous columns. Note: the column order is the same as in the original dataset
	\item \code{factorCols}: named vector of column indices of the factor columns
	\item \code{guidSeparator}: character used as separator in creating the uids.
}
}

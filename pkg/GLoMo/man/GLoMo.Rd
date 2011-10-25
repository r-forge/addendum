\name{GLoMo}
\alias{GLoMo}
\alias{GLoMo-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit naive General Location Model
}
\description{
Fit a naive General Location Model, only supporting cells that are present in the data.
}
\usage{
GLoMo(dfr, weights = rep(1, dim(dfr)[1]), uniqueIdentifiersPerRow = NULL, separator = ",", pooledCov = TRUE, verbosity = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
\code{\link{data.frame}} or \code{\link{numdfr}} to fit the model to
}
  \item{weights}{
vector of weights attributed to each row in \code{dfr}
}
  \item{uniqueIdentifiersPerRow}{
List of uids (see \code{\link{categoricalUniqueIdentifiers}}) for each row of \code{dfr}. If not provided, it is calculated.
}
  \item{separator}{
Only relevant if \code{uniqueIdentifiersPerRow} was not provided. This parameter is then passed on to \code{\link{categoricalUniqueIdentifiers}}.
}
  \item{pooledCov}{
If \code{TRUE}, the pooled covariance is used. This is relevant because often some if not all cells contain only 1 observation.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Ignored for now
}
}
\details{
Finds all 'cells' defined by the combinations of categorical variables in
\code{dfr}, and finds their (weighted) probability. Then it finds the mean per
cell for all continuous variables, and a covariance matrix (which is typically
pooled, although theoretically it should be unpooled)
}
\value{
List of class "GLoMo".
(NOTE: where I write dataset, this could either be a \code{\link{data.frame}} or \code{\link{numdfr}} object):
	\item{\code{uid} }{character vector: each item is a unique identifier of a cell. These get longer with more factor columns in the dataset}
	\item{\code{pihat} }{probability of each cell (numerical vector)}
	\item{\code{omegahat} }{matrix (named) of the continuous columns within cells (note: homoscedastic)}
	\item{\code{orgdatadim} }{dimensions of the dataset used to create this}
	\item{\code{uniqueFactorCombinationsAndContinuousMeans} }{for each uid, the matching factor levels + the means in that cell for the continuous columns. Note: the column and row order is the same as the
column order in the original dataset}
	\item{\code{factorCols} }{named vector of column indices of the factor columns}
	\item{\code{guidSeparator} }{character used as separator in creating the uids.}
  \item{\code{invomega} }{inverse of omegahat --- often used for prediction}
}
\references{
"Statistical Analysis with Missing Values"
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}
\note{
The \code{dfr} passed is supposed to not contain any \code{NA} values!

Further more, in the return value, the order of the columns (in e.g.
\code{omegahat} and \code{uniqueFactorCombinationsAndContinuousMeans}) is the
same as in the original \code{dfr}.

The length of \code{uid} and \code{pihat} is the same as the number of rows in
\code{uniqueFactorCombinationsAndContinuousMeans} (i.e. the number of unique
'cells' in \code{dfr}). Their order also matches (i.e. first item of \code{uid}
matches the first row of \code{uniqueFactorCombinationsAndContinuousMeans} etc.)

The number of columns/rows in \code{omegahat} and the number of items in
\code{factorCols} is also the total number of columns in \code{dfr} and thus
also in \code{uniqueFactorCombinationsAndContinuousMeans}.
}

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{predict}}, \code{\link{GLoMo-class}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)
iris.weights<-iris.nd.rnd$weights
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fit }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line

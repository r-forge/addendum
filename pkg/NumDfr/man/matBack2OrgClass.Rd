\name{matBack2OrgClass}
\alias{matBack2OrgClass}
\alias{matBack2OrgClass.numdfr}
\alias{matBack2OrgClass.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coerce a matrix (back) to \code{class} of some other object
}
\description{
Coerce a matrix (back) to \code{class} of some other object
}
\usage{
\method{matBack2OrgClass}{numdfr}(objWithClass, mat, catCols, levelList, ord=rep(FALSE, ncol(mat)),
	colnms=NULL, verbosity=0)
\method{matBack2OrgClass}{data.frame}(objWithClass, mat, catCols, levelList, ord=rep(FALSE, ncol(mat)),
	colnms=NULL, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{objWithClass}{
object to dispatch class upon
}
  \item{mat}{
matrix to coerce (back) to format/class of \code{objWithClass}
}
  \item{catCols}{
indices of categorical columns
}
  \item{levelList}{
list of character vectors holding the labels for categorical columns
}
  \item{ord}{
logical vector indicating whether each column is an ordered factor
}
  \item{colnms}{
columnn names to use (if not the original ones - default is \code{NULL})
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
object of the same class as \code{objWithClass}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{as.matrix}}
}
\examples{
iris.nd<-numdfr(iris)
irismat<-as.nummatrix(iris)
irismat<-irismat[sample(nrow(irismat), 20, replace=TRUE),]
rownames(irismat)<-seq(20)

ccs<-which(sapply(iris, is.factor))
lvls<-allLevels(iris, onlyNonEmpty=TRUE)
matBack2OrgClass(iris, irismat, catCols=ccs, levelList=lvls, verbosity=5)
matBack2OrgClass(iris.nd, irismat, catCols=ccs, levelList=lvls, verbosity=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ numdfr }
\keyword{ matrix }
\keyword{ coerce }% __ONLY ONE__ keyword per line

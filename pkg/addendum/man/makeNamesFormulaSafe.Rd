\name{makeNamesFormulaSafe}
\alias{makeNamesFormulaSafe}
\alias{makeDatasetFormulaSafe}
\alias{makeDatasetFormulaSafe.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alter column names / factor levels so that they can be safely used in formulas
}
\description{
Lots of code automatically uses column/factor names to build up formulas. These
methods make sure that the names are safely usable in formulas.
}
\usage{
makeNamesFormulaSafe(nms)
\method{makeDatasetFormulaSafe}{data.frame}(dfr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nms}{
character vector of items to make formula-safe
}
  \item{dfr}{
\code{\link{data.frame}} to make formula-safe
}
}
\details{
As of now, this is done through 2 \code{\link{gsub}} calls, that perform the
following changes to the names:

\itemize{
	\item Following special characters are replaced with "_": ":", "^", "*", "+", "-", "(", ")", "~", "%"
	\item All space-like characters are removed.
}

Note: there is no check that the names are unique afterwards!!
}
\value{
same type/shape as main parameter.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gsub}}
}
\examples{
	iris.cpy<-iris
	levels(iris.cpy$Species)<-c("Hello peoples", "Hello+P-e(o)~ples", "H^el*l\%o peoples")
	colnames(iris.cpy)[2]<-"look it contains holes and a +"
	iris.cpy2<-makeDatasetFormulaSafe(iris.cpy)
	cat("Original column names:", colnames(iris.cpy), "\n\tLevels of Species:", levels(iris.cpy$Species), "\n")
	cat("Safe column names:", colnames(iris.cpy2), "\n\tLevels of Species:", levels(iris.cpy2$Species), "\n")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ formula }

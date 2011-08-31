\name{predict.GLoMo}
\alias{predict.GLoMo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict (sample) data from GLoMo model
}
\description{
Sample data (filling out existing data or sampling a completely new dataset). If
a dataset is provided, and it contains 'cells' that are not covered by the naive
GLoMo, a warning is issued and marginal probabilities and means are used.
}
\usage{
\method{predict}{GLoMo}(object, nobs = 1, newdata = NULL, forrows = seq(nrow(newdata)), reusabledata = NULL, returnRepeats = FALSE, returnSelectedGlomoRows = FALSE, verbosity = 0,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{\link{GLoMo}} object
}
  \item{nobs}{
number of observations to sample. Can be a single integer or a vector of the
same length as \code{forrows}.
}
  \item{newdata}{
\code{NULL} (the default) if you want to sample a completely new dataset.
Otherwise, a dataset of the correct column structure, holding \code{NA} values.
}
  \item{forrows}{
indices of the rows of \code{newdata} for which data will be sampled. Default is
all of them.
}
  \item{reusabledata}{
See \code{\link{reusableDataForGLoMoSampling}}. Some of the calculations can be 
done for a row so that repeated sampling can go faster. If not provided, this is
calculated. Optionally, pass a \code{GuidData} object or a list of uids for the
rows here.
}
  \item{returnRepeats}{
if \code{TRUE} (not the default), an additional vector of the same length as
\code{forrows} is returned, holding how many rows were sampled for each row.
}
  \item{returnSelectedGlomoRows}{
if \code{TRUE} (not the default), an additional vector of the same length as
the number of rows sampled is returned, holding which row of the \code{GLoMo}
object was used for which row.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Ignored for now
}
}
\value{
If both \code{returnRepeats} and \code{returnSelectedGlomoRows} are \code{FALSE},
a \code{\link{data.frame}} or \code{\link{numdfr}} (dependent
	on the \code{dfr} that was used in the original call to \code{\link{GLoMo}}
	that holds the sampled data.
	
Otherwise, a list with items:
  \item{predicted }{The sampled dataset described above}
  \item{glomorowsused }{(only if \code{returnSelectedGlomoRows} was \code{TRUE}) vector of the same length as
the number of rows sampled is returned, holding which row of the \code{GLoMo}
object was used for which row}
  \item{numRepPerRow }{(only if \code{returnRepeats} was \code{TRUE}) vector of the same length as
\code{forrows} is returned, holding how many rows were sampled for each row}
}
\references{
"Statistical Analysis with Missing Values"
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{predict}}, \code{\link{GLoMo}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)
iris.weights<-iris.nd.rnd$weights
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
iris.pred.cond<-predict(iris.glomo, nobs=100, verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ predict }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line

\name{basisExpansionFormula}
\alias{basisExpansionFormula}
\alias{modelText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create formula applying a template to all columns
}
\description{
\code{basisExpansionFormula}: Apply a template to all columns (except \code{outcol}) of a dataframe and create
a formula with \code{outcol} on the left hand side and all templated columns as
terms on the right hand side

\code{modelText}: simple version of the above: allows passing a vector of column names instead,
but doesn't support template (just uses the column names plain and simple).
}
\usage{
basisExpansionFormula(dfr, outCol, patternNumericCols = "X", subPatternCol = "X", fixed = TRUE, ...)

modelText(outCol, predCols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataframe whose column names are used
}
  \item{outCol}{
Name of the outcome column
}
  \item{patternNumericCols}{
What pattern should be applied to all non-\code{outcol} columns
}
  \item{subPatternCol}{
String within \code{patternNumericCols} that is replaced by the non-\code{outcol} columnname
}
  \item{fixed}{
Parameter passed on to \code{grep} family of functions
}
  \item{\dots}{
Other parameters passed on to \code{grep} family of functions
}
  \item{predCols}{
vector holding the names of predictor columns
}
}
\details{
Special cases: \code{patternNumericCols} equal to \code{"X"} or \code{"."} is simply replaced with \code{"outcol ~ ."}
}
\value{
String representation of the formula
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
require(mgcv)
n <- 100000  ## simulate n data
dat <- gamSim(1,n=n,dist="binary",scale=.33)
p <- binomial()$linkinv(dat$f-6) ## make 1's rare
dat$y <- rbinom(p,1,p)      ## re-simulate rare response

## Now sample all the 1's but only proportion S of the 0's
S <- 0.02                   ## sampling fraction of zeroes
dat <- dat[dat$y==1 | runif(n) < S,] ## sampling

## Create offset based on total sampling fraction
dat$s <- rep(log(nrow(dat)/n),nrow(dat))

useFrm<-basisExpansionFormula(dfr=dat[,2:5], outCol="y",
	patternNumericCols = "s(X, bs=\"cr\")",	subPatternCol = "X", fixed = TRUE)
useFrm<-paste(useFrm, "offset(s)", sep="+")

lr.fit <- gam(formula(useFrm),family=binomial,data=dat,method="REML")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

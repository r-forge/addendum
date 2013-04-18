\name{imputeDs2FitDs}
\alias{constButWarnFunction}
\alias{getSafeFunction}
\alias{getSafeFunction.default}
\alias{getSafeFunction.unsafefunction}
\alias{getUnsafeFunction}
\alias{getUnsafeFunction.default}
\alias{getUnsafeFunction.unsafefunction}
\alias{illegals2Null}
\alias{illegalsCalculatedConstCountered}
\alias{illegalsCountered}
\alias{illegalToSmryLegalFunction}
\alias{imputeDs2FitDs}
\alias{imputeDs2FitDs.default}
\alias{imputeDs2FitDs.dfrConversionProps}
\alias{imputeDs2FitDs.dfrConversionPropsEx}
\alias{imputeDs2FitDsProps}
\alias{imputeDs2FitDsProps.default}
\alias{imputeDs2FitDsProps.normalImputationConversion}
\alias{isIllegal}
\alias{newIllegals}
\alias{normalImputationConversion}
\alias{normalImputationConversion-class}
\alias{print.unsafefunction}
\alias{removeIllegals}
\alias{removeScaling}
\alias{removeScaling.default}
\alias{removeScaling.dfrConversionPropsEx}
\alias{specialLegalX}
\alias{typicalScaleAndCenter}
\alias{typicalTransformations}
\alias{unsafefunction}
\alias{unsafefunction-class}
\title{Reusable information to convert an imputed dataset to fitting form}
\usage{
  imputeDs2FitDs(conversionData, ds, verbosity = 0, ...)

  \method{imputeDs2FitDs}{default}(conversionData,ds,verbosity=0,...)

  \method{imputeDs2FitDs}{dfrConversionProps}(conversionData,ds,verbosity=0,...)

  \method{imputeDs2FitDs}{dfrConversionPropsEx}(conversionData,ds,verbosity=0,...)

  imputeDs2FitDsProps(object,ds,verbosity=0)

  \method{imputeDs2FitDsProps}{default}(object,ds,verbosity=0)

  \method{imputeDs2FitDsProps}{normalImputationConversion}(object,ds,verbosity=0)

  removeScaling(object,verbosity=0)

  \method{removeScaling}{default}(object,verbosity=0)

  \method{removeScaling}{dfrConversionPropsEx}(object,verbosity=0)

  normalImputationConversion(betweenColAndLevel = "",
    includeBaseLevel=FALSE, scalingParams=NULL,
    transformParams=NULL)

  typicalTransformations(nm="_AllNonFact", addGon=FALSE)

  typicalScaleAndCenter()

  newIllegals(f,x)

  isIllegal(x)

  removeIllegals(x)

  illegalsCountered(f, f2=constButWarnFunction(),
    f2OnAll=FALSE)

  illegalsCalculatedConstCountered(f, smry=min, dflt=1e-10)

  \method{print}{unsafefunction}(x,...)

  getUnsafeFunction(object)

  \method{getUnsafeFunction}{default}(object)

  \method{getUnsafeFunction}{unsafefunction}(object)

  getSafeFunction(object,x)

  \method{getSafeFunction}{default}(object,x)

  \method{getSafeFunction}{unsafefunction}(object,x)

  illegals2Null(f,x)

  constButWarnFunction(cnst=0, warn=TRUE)

  specialLegalX(f, x, smry=min, dflt=1e-10)

  illegalToSmryLegalFunction(f, smry=min, dflt=1e-10,
    warn=TRUE)
}
\arguments{
  \item{conversionData}{object that holds information to
  convert an imputed dataset like
  \code{\link{dfrConversionProps}} or a custom
  implementation.}

  \item{ds}{dataset for which the lambdas need to be found}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{\dots}{For specific implementations}

  \item{object}{typically the return value of a call to
  \code{imputeDs2FitDs}}

  \item{betweenColAndLevel}{see
  \code{\link{dfrConversionProps}}}

  \item{includeBaseLevel}{see
  \code{\link{dfrConversionProps}}}

  \item{scalingParams}{list that may contain two items:
  "scale" and "center". Each are character vectors
  indicating which columns need to be scaled/centered. You
  can also use any of the meta-columns: "_AllNonFact",
  "_AllFact", "_All", "_AllExtra"}

  \item{transformParams}{list with an item per column that
  you want to apply transformations to (or you can use
  meta-column "_AllNonFact"). Each item is itself a list.
  The names are the extension that will be appended to the
  column name, the value is the function that will be
  applied to the column.}

  \item{nm}{name of the column or "_AllNonFact"
  (symbolically representing all non-factor columns) that
  these transformations will be applied to.}

  \item{addGon}{if \code{TRUE}, extra goniometrical
  transformations are added (\code{sin}, \code{cos} and
  \code{tan})}

  \item{f}{function for which illegal results (\code{NA} or
  \code{NaN}) will be checked. You can also directly pass
  along the return values of some function}

  \item{x}{data for which the results of \code{f} will be
  checked}

  \item{f2}{function that is called for the items of
  \code{x} that give 'illegal' results}

  \item{f2OnAll}{if \code{TRUE} (not the default),
  \code{f2} is reran on all items of \code{x} instead of
  only on the ones given illegal results from \code{f}}

  \item{cnst}{constant that will be repeated as return
  value (defaults to 0)}

  \item{warn}{if \code{TRUE} (default), each time this
  function is used, it will display a message}

  \item{smry}{summary function (like \code{min}, the
  default, or similar)}

  \item{dflt}{if the calculated summary still fails, this
  value is taken}
}
\value{
  dataset or matrix that can be used for fitting - depends
  on the implementation

  In this implementation, log the fact + return the
  incoming dataset \code{ds}

  In this implementation, use
  \code{\link{factorsToDummyVariables}}

  In this implementation, use
  \code{\link{factorsToDummyVariables}}

  Dataset to be used for fitting in EMLasso

  In this (default) implementation, return whatever was
  passed in (\code{object})

  In this (default) implementation, creat a
  \code{\link{dfrConversionProps}(Ex)}

  object that will still perform the other data
  conversions, but not scaling

  In this (default) implementation, return whatever was
  passed in (\code{object})

  \code{\link{dfrConversionProps}(Ex)} object like
  \code{object} but without scaling

  object of class \code{normalImputationConversion}

  object that is ideally fit for use as parameter
  \code{transformParams} of
  \code{normalImputationConversion}

  object that is ideally fit for use as parameter
  \code{scalingParams} of \code{normalImputationConversion}

  logical vector of the same length as \code{x}.
  \code{TRUE} for elements of \code{x} that resulted in an
  'illegal' return value of \code{f}

  logical vector that holds \code{TRUE} for each 'illegal'
  element of \code{x}

  copy of \code{x} where all the 'illegal' values have been
  removed

  creates a list with two functions (\code{safe} and
  \code{unsafe}). The \code{unsafe} is simply \code{f}, the
  \code{safe} version calls \code{f}, but for items that
  become 'illegal', \code{f2} is called. The class of the
  result is "unsafefunction"

  The class of the result is "unsafefunction", but now has
  structure that wil allow to calculate the constant from
  the first set of x's passed along (see
  \code{getSafeFunction})

  nothing

  function (a unsafe version of it - see
  \code{illegalsCalculatedConstCountered} or
  \code{illegalsCountered})

  In this implementation, simply return \code{object}

  In this implementation, simply return
  \code{object$unsafe}

  function (a safe version of it - see
  \code{illegalsCalculatedConstCountered} or
  \code{illegalsCountered})

  In this implementation, simply return \code{object}

  In this implementation, simply return \code{object$safe}
  if it is present, or build one from the other properties

  wrapper function around \code{f} that will return
  \code{NULL} if any of \code{f(x)} is turned into
  \code{NA} or \code{NaN}.

  function that will return the right nr of repeats of the
  constant. Depending on the \code{warn} value, it will
  display a message that this occurred or not.

  a single value that is either the calculated summary for
  the legal \code{x} and \code{f(x)} or \code{dflt}.

  a function that is a wrapper around \code{f} which
  replaces illegal values with the return value for the
  summary value of the legal \code{x}s.
}
\description{
  Reusable information to convert an imputed dataset to
  fitting form
}
\details{
  'Illegal' means that the result became \code{NA},
  \code{NaN} or \code{infinite} where \code{x} was none of
  those.
}
\note{
  abstracts away creation of conversionData: see
  \code{\link{EMLasso}}

  There is no reason to include ellipsis as a parameter! If
  you think you need it, look at the workaround through
  \code{normalImputationConversion}.

  Warnings during the calling of \code{f} are suppressed

  Warnings during the calling of \code{f} are suppressed
}
\examples{
removeIllegals(c(1,NA,3,1/0,Inf))
}
\seealso{
  \code{\link{EMLasso}}

  \code{\link{EMLasso}}
  \code{\link{factorsToDummyVariables}}

  \code{\link{EMLasso}}
  \code{\link{factorsToDummyVariables}}

  \code{\link{EMLasso}}

  \code{\link{EMLasso}}

  \code{\link{EMLasso}}

  \code{\link{repeatedlyPredictOut}}

  \code{\link{repeatedlyPredictOut}}

  \code{\link{repeatedlyPredictOut}}

  \code{\link{EMLasso}}
}
\keyword{conversion}
\keyword{dataset}
\keyword{impute}


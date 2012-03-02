\name{validateFunction.lognet}
\alias{validateFunction.lognet}
\title{validation function for use in \code{predict.conditional.allrows.GLoMo}}
\usage{
  validateFunction.lognet(attempts, otherData, forrow,
    verbosity = 0)
}
\arguments{
  \item{attempts}{\code{\link{numdfr}} or
  \code{\link{data.frame}} holding the rows to validate
  (sampled from GLoMo)}

  \item{otherData}{extra data that will be passed in.
  Required to be of class \code{LognetValidationData}}

  \item{forrow}{to which original row of the dataset do
  these predictions belong}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  integer vector of indices of accepted rows (within
  attempts)
}
\description{
  'Validates' samples in the rejection sampling way, by
  checking the predicted probability in the logistic LASSO
}
\note{
  noticed here that you need a way to map forrow (that is a
  rowindex in the original dataset) to an item in
  otherData. This will not be an issue if forrows that is
  passed to predict.conditional.allrows.GLoMo is really all
  rows, otherwise (then otherData will likely only hold
  items only for every selected row) you need more info...

  really only used internally from
  \code{\link{EMLasso.1l.lognet}}
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{predict.conditional.allrows.GLoMo}},
  \code{\link{EMLasso.1l.lognet}}
}
\keyword{GLoMo}
\keyword{predict}
\keyword{rejection}
\keyword{sampling}


\name{EMLassoFromDir}
\alias{EMLassoFromDir}
\title{Get EMLasso from the save files in a folder}
\usage{
  EMLassoFromDir(dir,
    fileForm="EMLasso\\\\.1l_parallel_([0-9]+)\\\\.saved",
    toNrForm="\\\\1", postProcessAll = postProcessEMLasso,
    family="unidentifiable", params="unidentifiable",
    verbosity=0)
}
\arguments{
  \item{dir}{path where the files are}

  \item{fileForm}{Regular expression showing the form of
  the filenames (should also hold a group to find the index
  of that file)}

  \item{toNrForm}{replacement regular expression: combined
  with \code{fileForm} should turn the name into its index
  (see \code{\link{sub}})}

  \item{postProcessAll}{function, like
  \code{\link{postProcessEMLasso}} (its default) and of
  that signature, to do some extra work before returning
  the result of \code{\link{EMLasso}}}

  \item{family}{see \code{\link{EMLasso}}, but it is
  overridden}

  \item{params}{see \code{\link{EMLasso}} object that will
  provide the conversion from imputed dataset to one that
  is ready for fitting the predictor model}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  very similar to the return value of
  \code{\link{EMLasso}}, though some (mostly unimportant)
  variables may be impossible to reconstruct, especially
  the \code{call} and \code{params}
}
\description{
  Get EMLasso from the save files in a folder (like what is
  saved during \code{\link{EMLasso}})
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso}}
}
\keyword{EMLasso}
\keyword{GLoMo}


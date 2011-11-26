\name{cv.glmnet.stability}
\alias{cv.glmnet.stability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find out how stable a glmnet's coefficients are over crossvalidation
}
\description{
Compares, with several measures, the coefficients of the model fit to the whole dataset
and to each fold's fitting set.
}
\usage{
cv.glmnet.stability(orgx, orgy, nfolds=10, stability.type=c("MC", "MCR", "FP", "FPR", "FN", "FNR", "FDR", "DST", "DSTPV"), unbiasFullCorrectionType=c("dont", "ridge", "brglm", "none"), unbiasFoldCorrectionType=c("dont", "ridge", "brglm", "none", "full_ridge", "full_brglm", "full_none"), outname="out", ..., verbosity=0)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orgx, orgy, nfolds}{
\code{x} and \code{y} and \code{nfolds} passed to \code{\link{cv.glmnet}}
}
  \item{stability.type}{
One or more measures out of : "MC" (missclassification nr), "MCR" (missclassification rate), "FP" (false positives),
"FPR" (false positive rate), "FN" (false negatives), "FNR" (false negative rate), "FDR" (false discovery rate), 
"DST" (euclidian distance in coefficient space), "DSTPV" (same as "DST but divided by nr of coefficients").
}
  \item{unbiasFullCorrectionType}{
method of bias correction that is applied to the model on the full dataset: "dont" (no bias correction), "ridge" (logistic regression with the selected variables + very light ridge), "brglm" (\code{brglm} w selected variables), "none" (simple
logistic regression).
}
  \item{unbiasFoldCorrectionType}{
method of bias correction that is applied to the model on the fold's fitting dataset: "dont" (no bias correction), "ridge" (logistic regression with the selected variables + very light ridge), "brglm" (\code{brglm} w selected variables), "none" (simple
logistic regression). The ones prefixed with "full_" do the same, but now they use the variables selected in the model from
the full dataset: "full_ridge", "full_brglm" and "full_none".
}
  \item{outname}{
name that can does not coincide with a column name already in \code{orgx}.
}
  \item{\dots}{
passed on to \code{\link{glmnet}}.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
A matrix with 2 columns per criterion: one with that criterion as column name and one with the criterion + "sd", 
that holds the sd from crossvalidation of that criterion. It has a row for every lambda.
}
\note{
The return value is ideally suited for \code{\link{matplotsd}}
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

\seealso{
\code{\link{plot}}, \code{\link{par}}
}
\examples{
y<-as.factor(rbinom(n=nrow(iris),prob=0.5,size=1))
x<-factorsToDummyVariables(iris)
tststab<-cv.glmnet.stability(orgx=x, orgy=y, stability.type=c("MC", "MCR", "FP", "FPR", "FN", "FNR", "FDR"),
	unbiasFullCorrectionType="dont", unbiasFoldCorrectionType="dont", family="binomial", verbosity=1)
matplotsd(x=seq(nrow(tststab)), y=tststab, col=neatColorSet(), legendx="topleft", legendcex=0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ SD }
\keyword{ error }
\keyword{ bar }

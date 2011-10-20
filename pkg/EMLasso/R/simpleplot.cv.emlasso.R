#' simpleplot implementation for class cv.emlasso
#' 
#' \code{\link{simpleplot}} implementation for class \code{\link{cv.emlasso}}
#' 
#' @param object \code{\link{cv.emlasso}} object
#' @param beta.type see \code{type} parameter of \code{\link{getBeta}}
#' @param xvar see \code{\link{simpleplot}}
#' @param label see \code{\link{simpleplot}}
#' @param \dots see \code{\link{simpleplot}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return see \code{\link{simpleplot}}
#' @method simpleplot cv.emlasso
#' @note This makes sure \code{\link{plotex}} works properly
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{simpleplot}}, \code{\link{plotex}}
#' @keywords simpleplot
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlcvfit<-EMLasso.lognet.cv(aDfr.MD, y, lambda=c(0.05, 0.03), nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlcvfitred<-reduce(emlcvfit)
#' simpleplot(emlcvfitred)
#' @export
simpleplot.cv.emlasso<-function(object, beta.type=NULL, xvar = c("norm", "lambda", "dev"), label=FALSE,..., verbosity=0)
{
		x<-object$glmnet.fit
    xvar = match.arg(xvar)
    theBeta<-getBeta(object, type=beta.type)
    plotCoef(theBeta, lambda = x$lambda, df = x$df, dev = x$dev.ratio,
        label = label, xvar = xvar, ...)
}
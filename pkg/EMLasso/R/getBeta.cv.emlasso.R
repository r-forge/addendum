#' getBeta implementation for class cv.emlasso
#' 
#' \code{\link{getBeta}} implementation for class \code{\link{cv.emlasso}}
#' 
#' @param object \code{\link{cv.emlasso}} object
#' @param type if equal to "final", then the beta from the final \code{\link{glmnet}} fit are used
#' @return see \code{\link{getBeta}}
#' @method getBeta cv.emlasso
#' @note needed to make \code{\link{plotex}} etc. work seemlessly
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{plotex}}
#' @keywords plotex
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlcvfit<-EMLasso.lognet.cv(aDfr.MD, y, lambda=c(0.05, 0.03), nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlcvfitred<-reduce(emlcvfit)
#' getBeta(emlcvfitred)
#' @export
getBeta.cv.emlasso<-function(object, type=NULL)
{
	if((! is.null(type)) && (type=="final"))
	{
		getBeta.cv.glmnet(object, type=NULL)
	}
	else
	{
		object$orgcoef
	}
}
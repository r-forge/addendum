#' S3 Generic method for recalculating criteria
#' 
#' S3 Generic method for recalculating criteria
#' 
#' @param object object for which to recalculate the criteria
#' @param calculateCriteria function that does the calculations ()defaults to \code{\link{calculateCriteria.EMLasso.1l.lognet}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param \dots passed on to deeper functions
#' @return object similar to \code{object} but with newly calculated criteria
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{calculateCriteria.EMLasso.1l.lognet}}, \code{\link{EMLasso.1l.lognet.cv}}
#' @keywords criteria
#' @export
refreshCriteria<-function(object, calculateCriteria=calculateCriteria.EMLasso.1l.lognet, verbosity=0, ...) UseMethod("refreshCriteria")



#' @rdname refreshCriteria
#' 
#' @inheritParams refreshCriteria
#' @return object similar to \code{object} but with newly calculated criteria
#' @method refreshCriteria cvpart.emlasso
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords criteria
#' @export
refreshCriteria.cvpart.emlasso<-function(object, calculateCriteria=calculateCriteria.EMLasso.1l.lognet, verbosity=0, ...)
{
	crits<-calculateCriteria(object$fitinfo, object$valsample, verbosity=verbosity)
	object$criteria=crits
	return(object)
}



#' @rdname refreshCriteria
#' 
#' @inheritParams refreshCriteria
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param grouped see \code{\link{cv.glmnet}}
#' @param maxFracZeroesAllowed see \code{\link{EMLasso.1l.lognet.cv}}
#' @return object similar to \code{object} but with newly calculated criteria
#' @method refreshCriteria cv.1l.emlasso
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords criteria
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlfit<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlfit<-eml17
#' emlfit<-refreshCriteria(emlfit, verbosity=10)
#' @export
refreshCriteria.cv.1l.emlasso<-function(object,
	calculateCriteria=calculateCriteria.EMLasso.1l.lognet, verbosity=0,
	type.measure=ifelse(is.null(object$type.measure), "auc", object$type.measure),
	grouped=ifelse(is.null(object$type.grouped), TRUE, object$type.grouped),
	maxFracZeroesAllowed=0, ...)
{
	if(! is.null(calculateCriteria))
	{
		for(i in seq_along(object$actualfits))
		{
			catwif(verbosity > 1, "Fold", i, "/", length(object$actualfits))
			object$actualfits[[i]]<-refreshCriteria(object$actualfits[[i]], calculateCriteria=calculateCriteria, verbosity=verbosity-2, ...)
		}
	}

  cvstuff = mimic_cv_lognet(object$actualfits, type.measure=type.measure, 
		grouped=grouped, verbosity=verbosity-1)
  cvm = cvstuff$cvm
  cvsd = cvstuff$cvsd
  cvname = cvstuff$name
  object$cvm = cvm
  object$cvsd = cvsd
  object$cvm = cvup = cvm +cvsd
  object$cvlo = cvm - cvsd
  object$name = cvname
  object$type.measure=type.measure
	object$grouped=grouped
	
	cfs<-coef(object, foldname=NULL, itername=NULL, onlyFinal=TRUE,
		onlyEqualZeroPercent=maxFracZeroesAllowed)
	object$nzero<-sapply(cfs, mean, na.rm=TRUE)

	return(object)
}
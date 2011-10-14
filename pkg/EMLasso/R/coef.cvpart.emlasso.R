#' Get coefficients of a "cvpart.emlasso" object
#' 
#' Get coefficients of a "cvpart.emlasso" object
#' 
#' @param object "cvpart.emlasso" object
#' @param onlyFinal if \code{TRUE}, only the coefficients of the final iteration are returned
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param \dots ignored
#' @method coef cvpart.emlasso
#' @return \code{\link{data.frame}} with a row per iteration and a column per coefficient
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords coefficient
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{nrs<-nrow(aDfr.MD)
#' uniqueGroupNrs<-seq(10)
#' groupNrPerRow<-similarSizeGroups(10, nobs=nrs, rand = TRUE)
#' eml1ffit<-EMLasso.1l.lognet.onefold(1, groupNrPerRow, aDfr.MD, y, lambda=0.05, 
#' 	nrOfSamplesPerMDRow=7, maxIt=20, minIt=10, weightsName="weights", orgriName="orgri",
#' 	precalcGuidData=FALSE, reuseDebugLevel=-1, 
#' 	nrOfSamplesForCriteria=7, calculateCriteria=calculateCriteria.EMLasso.1l.lognet,verbosity=10)}
#' eml1ffit<-eml17$actualfits[[1]]
#' coef(eml1ffit)
#' @export
coef.cvpart.emlasso<-function(object, onlyFinal=FALSE, verbosity=0,...)
{
	if(onlyFinal)
	{
		catwif(verbosity > 0, "Returning only final coefficients")
		rv<-object$fitinfo$coefs[nrow(object$fitinfo$coefs),]
	}
	else
	{
		catwif(verbosity > 0, "Returning ALL coefficients")
		rv<-object$fitinfo$coefs
	}
	return(rv)
}
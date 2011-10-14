#' Crossvalidate EMLasso for 1 lambda
#' 
#' Crossvalidate EMLasso for 1 lambda
#' 
#' @param dfr dataset (\code{\link{numdfr}} or \code{\link{data.frame}}) to fit it to
#' @param resp outcome vector
#' @param lambda the single lambda
#' @param nrOfSamplesPerMDRow For rows with missing data, how many rows to sample
#' @param howManyFold how many-fold crossvalidation
#' @param groupNrPerRow as alternative to howManyFold: provide group number per row
#' @param minIt minimum number of iterations before convergence is possible
#' @param maxIt maximum number of iterations before convergence is automatically assumed
#' @param weightsName name that can be safely given to a weights column (should not conflict
#' with existing column names)
#' @param orgriName name that can be safely given to a original row index column (should not 
#' conflict with existing column names)
#' @param precalcGuidData (experimental) if \code{TRUE} an effort is made to reuse GuidData
#' @param nrOfSamplesForCriteria similar to nrOfSamplesPerMDRow, but for final calculation of
#' validation criteria
#' @param calculateCriteria function to calculate criteria
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param reuseDebugLevel (for internal use only)
#' @param type.measure one of the measures \code{\link{cv.glmnet}}
#' @param grouped see \code{\link{cv.glmnet}}
#' @param maxFracZeroesAllowed used for final set of coefficients -> only return the ones
#' that were zero in at most this percentage of the folds
#' @param dfrconvprobs dataset conversion properties (see \code{\link{dfrConversionProbs}})
#' @return An object of class \code{\link{cv.1l.emlasso}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @references [PENDING]
#' @keywords EMLasso crossvalidate
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlcvfit<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlcvfit<-eml17
#' @export
EMLasso.1l.lognet.cv<-function(dfr, resp, lambda, nrOfSamplesPerMDRow,
	howManyFold=10, groupNrPerRow=NULL, maxIt=20,
	minIt=10, weightsName="weights", orgriName="orgri", precalcGuidData=TRUE,
	nrOfSamplesForCriteria=nrOfSamplesPerMDRow,
	calculateCriteria=calculateCriteria.EMLasso.1l.lognet, verbosity=0,
	reuseDebugLevel=-1, type.measure="auc", grouped=TRUE,
	maxFracZeroesAllowed=0,
	dfrconvprobs)
{
	catwif(verbosity > 0, "starting.")
	if(missing(dfrconvprobs))
	{
		catwif(verbosity > 1, "dfrconvprobs was not passed along so calculating it now")
		dfrconvprobs<-dfrConversionProbs(dfr, betweenColAndLevel="")
	}
	calculateCriteria<-getAsFunction(calculateCriteria, verbosity=verbosity-1)
	if(is.null(calculateCriteria)) catwif(verbosity > 1, "**No valid calculateCriteria found, so criteria will not be calculated.")

	nrs<-nrow(dfr)
	if(reuseDebugLevel > 1)
	{
		catwif(verbosity > 1, "reusing group numbers")
		uniqueGroupNrs<-.getEMLProp("uniqueGroupNrs")
		howManyFold<-.getEMLProp("howManyFold")
	}
	else
	{
		if(! is.null(groupNrPerRow))
		{
			uniqueGroupNrs<-sort(unique(groupNrPerRow))
			howManyFold<-length(uniqueGroupNrs)
		}
		else
		{
			uniqueGroupNrs<-seq(howManyFold)
			groupNrPerRow<-similarSizeGroups(howManyFold, nobs=nrs, rand = TRUE)
		}
		if(reuseDebugLevel > -1)
		{
			.setEMLProp("uniqueGroupNrs", uniqueGroupNrs)
			.setEMLProp("groupNrPerRow", groupNrPerRow)
		}
	}
	results<-lapply(seq(howManyFold), function(curGroupi){
			catwif(verbosity > 1, curGroupi, "/", howManyFold)
			curGroup<-uniqueGroupNrs[curGroupi]
			EMLasso.1l.lognet.onefold(curGroup, groupNrPerRow, dfr, resp, lambda,
				nrOfSamplesPerMDRow,
				maxIt, minIt, weightsName, orgriName,
				precalcGuidData, dfrconvprobs, reuseDebugLevel, 
				nrOfSamplesForCriteria, calculateCriteria, verbosity=verbosity-1)
			})
  cvstuff<-try(mimic_cv_lognet(results, type.measure=type.measure, grouped=grouped,
		verbosity=verbosity-1))
	if(! inherits(cvstuff, "try-error"))
	{
	  cvm<-cvstuff$cvm
	  cvsd<-cvstuff$cvsd
	  cvname<-cvstuff$name
  }
  else
	{
	  cvm<-NA
	  cvsd<-NA
	  cvname<-type.measure
  }
  #note: need to nz and lambda here!!
  object<-list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm +
      cvsd, cvlo = cvm - cvsd, name = cvname, actualfits=results,
			type.measure=type.measure, grouped=grouped)
	class(object)<-"cv.1l.emlasso"
	cfs<-try(coef(object, foldname=NULL, itername=NULL, onlyFinal=TRUE,
		onlyEqualZeroPercent=maxFracZeroesAllowed))
	object$nzero<-try(sapply(cfs, mean, na.rm=TRUE))

	return(object)
}
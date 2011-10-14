#' Predict probabilities from a model with its own validation sample
#' 
#' Predict probabilities from a model with its own validation sample
#' 
#' @param object model fit
#' @param \dots for flexibility in 'derived' implementation
#' @return depends on the implementation, but typically a \code{data.frame} with columns
#' trueOut= true outcome for each row, predProb= predicted probability of level 2 for each 
#' 	row, (predProb_LR: optional: predicted probability from logistic regression
#' 	with the selected predictors), weight= weight apointed to each row
#' @note aids to generalize validation
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords probability model
#' @export
validationOutAndPredProb<-function(object,...) UseMethod("validationOutAndPredProb")



#' @rdname validationOutAndPredProb
#' 
#' @inheritParams validationOutAndPredProb
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @method validationOutAndPredProb cvpart.emlasso
#' @usage \method{validationOutAndPredProb}{cvpart.emlasso}(object, ...)
#' @seealso \code{\link{EMLasso.1l.lognet.onefold}}
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{nrs<-nrow(aDfr.MD)
#' uniqueGroupNrs<-seq(10)
#' groupNrPerRow<-similarSizeGroups(10, nobs=nrs, rand = TRUE)
#' eml1ffit<-EMLasso.1l.lognet.onefold(1, groupNrPerRow, aDfr.MD, y, lambda=0.05, 
#' 	nrOfSamplesPerMDRow=7, maxIt=20, minIt=10, weightsName="weights", orgriName="orgri",
#' 	precalcGuidData=FALSE, reuseDebugLevel=-1, 
#' 	nrOfSamplesForCriteria=7, calculateCriteria=calculateCriteria.EMLasso.1l.lognet,verbosity=10)}
#' eml1ffit<-eml17$actualfits[[1]]
#' validationOutAndPredProb(eml1ffit)
#' @export
validationOutAndPredProb.cvpart.emlasso<-function(object,...)
{
	trueOut<-object$valsample$resp
	predProb<-object$criteria$predProb
	predProb_LR<-object$criteria$predProb_logreg
	weight<-rep(1/object$valsample$numRepPerRow,object$valsample$numRepPerRow)
	data.frame(trueOut=trueOut, predProb=predProb, predProb_LR=predProb_LR, weight=weight)
}



#' @rdname validationOutAndPredProb
#' 
#' @inheritParams validationOutAndPredProb
#' @method validationOutAndPredProb cv.1l.emlasso
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{cv.1l.emlasso}}, \code{\link{EMLasso.1l.lognet}}
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlfit<-eml17
#' validationOutAndPredProb(emlfit)
#' @export
validationOutAndPredProb.cv.1l.emlasso<-function(object,...)
{
	coeflst<-lapply(seq_along(object$actualfits), function(i){
			rv<-validationOutAndPredProb(object$actualfits[[i]], ...)
			rv$fold<-i
			rv
		})
	combineSimilarDfrList(coeflst)
}

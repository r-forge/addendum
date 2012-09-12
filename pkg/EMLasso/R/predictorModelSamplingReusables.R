#' Create reusable part of prediction for repeated sampling
#' 
#' Create reusable part of prediction for repeated sampling
#' 
#' @param predictorModel predictor model (e.g. \code{\link{GLoMo}})
#' @param iterCount for the how manieth iteration is this the reusable part
#' @param previousReusables result of this function for the previous iteration
#' 	or \code{NULL} if there was none
#' @param ds dataset for which prediction needs to occur
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return for the generic method: depending on the implementation.
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords predictor model rejection sampling guid
#' @export predictorModelSamplingReusables
predictorModelSamplingReusables<-function(predictorModel, iterCount, previousReusables=NULL, ds, verbosity=0) UseMethod("predictorModelSamplingReusables")
#' @rdname predictorModelSamplingReusables
#' 
#' @aliases predictorModelSamplingReusables.GLoMo SamplingReusablesGLoMo SamplingReusablesGLoMo-class
#' @method predictorModelSamplingReusables.GLoMo
#' @usage \method{predictorModelSamplingReusables}{GLoMo}(predictorModel, iterCount, previousReusables=NULL, ds, verbosity=0)
#' @return (in the \code{\link{GLoMo}} implementation) an object of class "SamplingReusablesGLoMo", i.e. a list with items:
#' \enumerate{
#' 	\item \code{guidDataOfOriginalDfr}: return value of \code{\link{getGuidData}}
#' 	\item \code{uidsPerRowOfOriginalDfr}: list holding for each row in \code{ds} the applicable uids in the \code{\link{GLoMo}}
#' }
#' @seealso \code{\link{EMLasso.1l}}
#' @S3method predictorModelSamplingReusables GLoMo
predictorModelSamplingReusables.GLoMo<-function(predictorModel, iterCount, previousReusables=NULL, ds, verbosity=0)
{
	#note: ds should always be the dataset that needs completion (so holds missing data) - in EMLasso this
	#is simply always the same. Normally, if a SamplingReusablesGLoMo has already been made for this 
	#dataset, the uidsPerRowOfOriginalDfr can simply be reused.
	#uidsPerRowOfOriginalDfr's member possibleGlomoGuidPerObs maps rows of 'ds' to cells  of the GLoMo
	if(is.null(previousReusables))
	{
		guidDataOfOriginalDfr<-getGuidData(glomo=predictorModel, dfr=ds, guidPerObservation=NULL, verbosity=verbosity-1)
		uidsPerRowOfOriginalDfr<-guidDataOfOriginalDfr$guidPerObservation
		rv<-list(guidDataOfOriginalDfr=guidDataOfOriginalDfr, uidsPerRowOfOriginalDfr=uidsPerRowOfOriginalDfr)
		class(rv)<-"SamplingReusablesGLoMo"
		return(rv)
	}
	else
	{
		previousReusables$guidDataOfOriginalDfr <- getGuidData(glomo = predictorModel, dfr = ds, 
																													 guidPerObservation = previousReusables$uidsPerRowOfOriginalDfr, 
																													 verbosity = verbosity - 1)
		return(previousReusables)
	}
# 	if(iterCount==1)
# 	{
# 		previousReusables$guidDataOfOriginalDfr<-previousReusables$uidsPerRowOfOriginalDfr
# 	}
# 	return(previousReusables)
}
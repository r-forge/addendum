#' Complete a dataset conditional on a model and the outcomes
#' 
#' Complete a dataset conditional on a model and the outcomes
#' 
#' @aliases sampleConditional
#' @param predictorModel model binding the predictors (e.g. \code{\link{GLoMo}})
#' @param outcomeModel outcome model (linking the outcomes to the predictors) (e.g. \code{\link{glmnet}} object)
#' @param nrOfSamplesPerMDRow number times each row with missing data will be completed
#' @param ds dataset to be completed
#' @param out outcomes
#' @param rowsToUseForFit which of the rows in \code{ds} can be used for fitting the outcome model
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}}
#' @param reusableForSampling Which of the rows of dfr/out can be used for fitting the LASSO
#' @param reusableForvalidation minimum number of iterations before convergence is possible
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l}}
#' @keywords EMLasso postprocess
#' @export
sampleConditional<-function(predictorModel, outcomeModel, nrOfSamplesPerMDRow, ds, out, rowsToUseForFit, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0) UseMethod("sampleConditional")
#' @rdname sampleConditional
#' 
#' @aliases sampleConditional.GLoMo sampledConditionallyGLomo sampledConditionallyGLomo-class sampledConditionally sampledConditionally-class
#' @method sampleConditional.GLoMo
#' @usage \method{sampleConditional}{GLoMo}(predictorModel, outcomeModel, nrOfSamplesPerMDRow, ds, out, rowsToUseForFit, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0)
#' @return object of classes "sampledConditionallyGLomo" and "sampledConditionally", which is a list with items:
#' \enumerate{
#' 	\item \code{ds}: completed dataset
#' 	\item \code{out}: outcome per row of \code{ds} (the one in this list)
#' 	\item \code{weights}: weight per row in \code{ds} (the one in this list)
#' 	\item \code{orgri}: row index within \code{ds} (the parameter of the function) of each row within \code{ds} (the one in this list)
#' 	\item \code{useForFit}: boolean indicator for each row of \code{ds} (the one in this list) that is \code{TRUE} if this row is to be used for validation
#' 	\item \code{useOut}: outcomes, but only for the rows of \code{ds} that can be used for validation 
#' }
#' @S3method sampleConditional GLoMo
sampleConditional.GLoMo<-function(predictorModel, outcomeModel, nrOfSamplesPerMDRow, ds, out, rowsToUseForFit, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0)
{
	tmp<-predict.conditional.allrows.GLoMo(object=predictorModel, nobs=nrOfSamplesPerMDRow,
		dfr=ds, validateFunction=acceptOrRejectFunction(outcomeModel), 
		guidDataOfOriginalDfr=reusableForSampling$guidDataOfOriginalDfr,
		otherData=reusableForvalidation, verbosity=verbosity-2)
	
	curData<-tmp$predicted
	curWeights<-rep(1/tmp$repsperrow, tmp$repsperrow)#now holds 1 weight per observation
	curOut<-rep(out, tmp$repsperrow)
	curOrgri<-rep(seq_along(out), tmp$repsperrow)
	curUseForFit<-curOrgri %in% rowsToUseForFit
	curUseOut<-(out[curOrgri])[curUseForFit]
	retval<-list(
		ds=curData,
		out=curOut,
		weights=curWeights,
		orgri=curOrgri,
		useForFit=curUseForFit,
		useOut=curUseOut
		)
	class(retval)<-c("sampledConditionallyGLomo", "sampledConditionally")
	return(retval)
}
#' @rdname sampleConditional
#' 
#' @aliases acceptOrRejectFunction
#' @method acceptOrRejectFunction
#' @usage acceptOrRejectFunction(outcomeModel)
#' @return \code{sampleConditional} returns a function with the same signature and goal as \code{\link{validateFunction.lognet}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l}}
#' @keywords EMLasso postprocess
#' @export acceptOrRejectFunction
acceptOrRejectFunction<-function(outcomeModel) UseMethod("acceptOrRejectFunction")
#' @rdname sampleConditional
#' 
#' @aliases acceptOrRejectFunction.default
#' @method acceptOrRejectFunction.default
#' @usage \method{acceptOrRejectFunction}{default}(outcomeModel)
#' @seealso \code{\link{EMLasso.1l}}
#' @S3method acceptOrRejectFunction default
acceptOrRejectFunction.default<-function(outcomeModel)
{
	return(function(attempts, otherData, forrow, verbosity = 0){seq(nrow(attempts$predicted))})
}
#' @rdname sampleConditional
#' 
#' @aliases acceptOrRejectFunction.lognet
#' @method acceptOrRejectFunction.lognet
#' @usage \method{acceptOrRejectFunction}{lognet}(outcomeModel)
#' @seealso \code{\link{EMLasso.1l}}
#' @S3method acceptOrRejectFunction lognet
acceptOrRejectFunction.lognet<-function(outcomeModel)
{
	return(validateFunction.lognet)
}
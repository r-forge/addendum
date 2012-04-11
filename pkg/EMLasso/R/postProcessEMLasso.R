#' Postprocess EMLasso (e.g. add extra members or change class)
#' 
#' Postprocess EMLasso (e.g. add extra members or change class)
#' 
#' @aliases postProcessEMLasso
#' @param predictorModel the final predictor model will be passed in here (e.g. \code{\link{GLoMo}} object)
#' @param retval the return value of \code{\link{EMLasso.1l}} to be postprocessed
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return Typically the passed along \code{retval} object with optionally some more items/class/attributes
#' @details This function is (typically) called at the end of \code{\link{EMLasso}} and may alter the \code{class}
#' 	of the end result and/or add some more items to the list.
#' In the case of the \code{\link{GLoMo}} implementation, it adds the classnames extended with "GLoMo" and
#' 	provides the \code{combinedGLoMo} item (see \code{\link{combineGLoMos}})
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l}}
#' @keywords EMLasso postprocess
#' @export
postProcessEMLasso<-function(predictorModel, retval, verbosity=0) UseMethod("postProcessEMLasso")
#' @rdname postProcessEMLasso
#' 
#' @aliases postProcessEMLasso.GLoMo EMLassoGLoMo-class EMLassoGLoMo
#' @method postProcessEMLasso.GLoMo
#' @usage \method{postProcessEMLasso}{GLoMo}(predictorModel, retval, verbosity=0)
#' @S3method postProcessEMLasso GLoMo
postProcessEMLasso.GLoMo<-function(predictorModel, retval, verbosity=0)
{
	if(any(sapply(retval$result, class) == "try-error"))
	{
		catw("At least one run failed, so will not attempt to complete postprocessing")
	}
	else
	{
		try({
			glomolist<-lapply(retval$result, "[[", "predictorModel")
			retval$combinedGLoMo<-combineGLoMos(listOfGLoMos=glomolist, verbosity=verbosity-5)
		})
	}
	class(retval)<-c(class(retval), paste(class(retval), "GLoMo", sep="") )
	
	return(retval)
}
#' @rdname postProcessEMLasso
#' 
#' @aliases postProcessEMLasso.default
#' @method postProcessEMLasso.default
#' @usage \method{postProcessEMLasso}{default}(predictorModel, retval, verbosity=0)
#' @S3method postProcessEMLasso default
postProcessEMLasso.default<-function(predictorModel, retval, verbosity=0)
{
	return(retval)
}

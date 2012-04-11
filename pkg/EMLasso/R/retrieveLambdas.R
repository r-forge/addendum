#' Retrieve lambdas
#' 
#' Retrieve lambdas
#' 
#' @param lambdas simple set of lambda values for the default, or an instance of a class
#' 	like \code{smartLambdaRetriever} for which a specific version of the method is 
#' 	implemented.
#' @param ds dataset for which the lambdas need to be found
#' @param out outcome variable
#' @param \dots For specific implementations, e.g. passed on to 
#' 	\code{\link{findReasonableLambdaHelper}} in \code{retrieveLambdas.smartLambdaRetriever}
#' @note abstracts away selection of lambdas: see \code{\link{EMLasso}}
#' @keywords lambda selection
#' @export
retrieveLambdas<-function(lambdas,ds,out,...) UseMethod("retrieveLambdas")
#' @rdname retrieveLambdas
#' 
#' @aliases retrieveLambdas retrieveLambdas.default
#' @method retrieveLambdas.default
#' @usage \method{retrieveLambdas}{default}(lambdas,ds,out,...)
#' @return vector of lambda values - in fact, \code{lambdas}
#' @seealso \code{\link{EMLasso}}
#' @S3method retrieveLambdas default
retrieveLambdas.default<-function(lambdas,ds,out,...)
{
	return(lambdas)
}
#' @rdname retrieveLambdas
#' 
#' @aliases retrieveLambdas.smartLambdaRetriever smartLambdaRetriever-class
#' @method retrieveLambdas.smartLambdaRetriever
#' @usage \method{retrieveLambdas}{default}(lambdas,ds,out,...)
#' @return vector of lambda values through \code{\link{findReasonableLambdaHelper}}
#' @seealso \code{\link{EMLasso}}
#' @S3method retrieveLambdas smartLambdaRetriever
retrieveLambdas.smartLambdaRetriever<-function(lambdas,ds,out,...)
{
	extraparms<-list(...)
	parms<-lambdas
	for(nm in names(extraparms))
	{
		parms[[nm]]<-extraparms[[nm]]
	}
	parms[["ds"]]<-ds
	parms[["out"]]<-out
	parms[["showPlot"]]<-FALSE #let's avoid this plot here...
	
	rlh<-do.call(findReasonableLambdaHelper, parms)
	return(getLambdas(rlh))
}
#' @rdname retrieveLambdas
#' 
#' @aliases smartLambdaRetriever smartLambdaRetriever-class
#' @method smartLambdaRetriever
#' @usage smartLambdaRetriever(...)
#' @note Constructor for \code{smartLambdaRetriever} objects. See 
#' 	\code{\link{findReasonableLambdaHelper}} for what could be passed through \dots
#' @seealso \code{\link{EMLasso}}, \code{\link{findReasonableLambdaHelper}}
#' @export smartLambdaRetriever
smartLambdaRetriever<-function(...)
{
	retval<-list(...)
	class(retval)<-"smartLambdaRetriever"
	return(retval)
}

#' Wrapper to complete a dataset based on (univariate) marginal distributions
#' 
#' Wrapper to complete a dataset based on (univariate) marginal distributions
#' 
#' @param weightsName object that holds information to convert an imputed dataset
#' 	like \code{\link{dfrConversionProps}} or a custom implementation.
#' @param orgriName dataset for which the lambdas need to be found
#' @param maxFullNACatCols see \code{\link{rCatsAndCntInDfr}}
#' @param howManyIfTooMany see \code{\link{rCatsAndCntInDfr}}
#' @return function that can actually perform the conversion -- used in \code{\link{EMLasso.1l}}
#' @note abstracts away creation of marginal completion: see \code{\link{EMLasso.1l}}
#' @keywords impute dataset marginal
#' @export
marginalCompleter<-function(weightsName="weights", orgriName="orgri", maxFullNACatCols = 6, howManyIfTooMany = 1000)
{
	force(weightsName)
	force(orgriName)
	force(maxFullNACatCols)
	force(howManyIfTooMany)
	retval<-function(ds, out, rowsToUse, verbosity=0)
	{
		completedData<-rCatsAndCntInDfr(dfr=ds, weightsName=weightsName,
			orgriName=orgriName, reweightPerRow=TRUE, verbosity=verbosity-1,
			maxFullNACatCols=maxFullNACatCols, howManyIfTooMany=howManyIfTooMany)
		completedWeights<-completedData[[weightsName]]
		completedOrgri<-completedData[[orgriName]]
		rmColsPos<-match(c(weightsName, orgriName), colnames(completedData))
		completedData<-completedData[,-rmColsPos]
		completedUse<-completedOrgri %in% rowsToUse
		completedOut<-out[completedOrgri]
		rv<-list(ds=completedData, out=completedOut, weights=completedWeights, 
						 completedUse=completedUse)
		class(rv)<-"marginalCompleted"
		return(rv)
	}
	return(retval)
}
#' @rdname marginalCompleter
#' 
#' @aliases completeMarginal marginalCompleted marginalCompleted-class
#' @method completeMarginal
#' @usage completeMarginal(object,ds, out, rowsToUse, verbosity=0)
#' @param object object/function that will perform the actual imputation
#' @param ds dataset that needs completion
#' @param out outcomes
#' @param rowsToUse rowindices of the rows that can actually be used
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return object of class \code{marginalCompleted}, holding items:
#' \enumerate{
#' 	\item \code{ds}: completed dataset
#' 	\item \code{out}: outcome variable
#' 	\item \code{weights}: weights to apply to each row of \code{ds}
#' 	\item \code{completedUse}: rowindices within \code{ds} to be actually used
#' }
#' @seealso \code{\link{EMLasso.1l}}
#' @export completeMarginal
completeMarginal<-function(object,ds, out, rowsToUse, verbosity=0) UseMethod("completeMarginal")
#' @rdname marginalCompleter
#' 
#' @aliases completeMarginal.function
#' @method completeMarginal.function
#' @usage \method{completeMarginal}{function}(object,ds, out, rowsToUse, verbosity=0)
#' @seealso \code{\link{EMLasso.1l}}
#' @S3method completeMarginal function
completeMarginal.function<-function(object,ds, out, rowsToUse, verbosity=0)
{
	return(object(ds=ds, out=out, rowsToUse=rowsToUse, verbosity=verbosity))
}
#' @rdname marginalCompleter
#' 
#' @aliases completeMarginal.default
#' @method completeMarginal.default
#' @usage \method{completeMarginal}{default}(object,ds, out, rowsToUse, verbosity=0)
#' @seealso \code{\link{EMLasso.1l}}
#' @S3method completeMarginal default
completeMarginal.default<-function(object,ds, out, rowsToUse, verbosity=0)
{
	rv<-list(ds=ds, out=out, weights=rep(1, nrow(ds)), completedUse=rowsToUse)
	class(rv)<-"marginalCompleted"
	return(rv)
}


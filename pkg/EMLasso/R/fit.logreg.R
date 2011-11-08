#' Fit logistic regression by using logistic LASSO with lambda=0
#' 
#' Fit logistic regression by using logistic LASSO with lambda=0
#' 
#' @param dfr dataset (\code{\link{numdfr}} or \code{\link{data.frame}})
#' @param resp outcome vector
#' @param wts weight vector per observation (does not have to sum to 1, and defaults to equal weights)
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param useCols restrict the regression to only include these columns as predictors
#' @param outName name that can be used safely for the outcome (i.e. a column name not present in \code{dfr})
#' @param dfrConvData premade return value of \code{\link{dfrConversionProbs}} for that \code{glmnet}
#' and dataset
#' @return \code{\link{glmnet}} object
#' @note The warning in the old function pointed me to the fact that the weights are _not_
#' probability weights in a binomial glm!!
#' So, as an alternative, we use glmnet with lambda=0 !!!
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords fit logistic regression
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' lreg<-fit.logreg(iris, y, wts=runif(nrow(iris)), verbosity=1)
#' @export
fit.logreg<-function(dfr, resp, wts=rep(1, nrow(dfr)), verbosity=0, useCols=NULL,
	outName="out", dfrConvData)
{
	catwif(verbosity > 0, "dim dfr:", dim(dfr))
	if(missing(dfrConvData))
	{
		dfrConvData<-dfrConversionProbs(dfr, "")
	}
	dfr.mat<-factorsToDummyVariables(dfr, betweenColAndLevel = "", dfrConvData=dfrConvData, verbosity=verbosity-1) #Some factors are only partially there!
	catwif(verbosity > 0, "after factorsToDummyVariables dim dfr.mat:", dim(dfr.mat))
	if(!is.null(useCols))
	{
    if (is.character(useCols)) {
        useCols <- useCols[useCols %in% colnames(dfr.mat)]
        catwif(verbosity > 0, "useCols:", useCols)
    }
    else {
        catwif(verbosity > 0, "useCols in positions:", useCols)
        catwif(verbosity > 0, "useCols:", colnames(dfr.mat)[useCols])
    }
		dfr.mat<-dfr.mat[,useCols, drop=FALSE]
	}
	catwif(verbosity > 0, "dim dfr.mat:", dim(dfr.mat), ", l(resp):", length(resp), ", l(wts):", length(wts), "\n")
	if(ncol(dfr.mat) == 0)
	{
		catwif(verbosity > 0, "No predictors, so returning NULL")
		return(NULL)
	}
	fit<-glmnet(dfr.mat, resp, family="binomial", weights=wts, lambda=0, standardize=FALSE)
	catwif(verbosity > 0, "glm fit succeeded.")
	return(fit)
}

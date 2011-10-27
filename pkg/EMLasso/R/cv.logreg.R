#' Crossvalidate logistic regression by using logistic LASSO with lambda=0
#' 
#' Crossvalidate logistic regression by using logistic LASSO with lambda=0
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
#' @param nfolds number of folds
#' @param type.measure see \code{\link{cv.glmnet}}
#' @return \code{\link{cv.glmnet}} object
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords fit logistic regression
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' cvlreg<-cv.logreg(iris, y, wts=runif(nrow(iris)), verbosity=1)
#' @export
cv.logreg<-function(dfr, resp, wts=rep(1, nrow(dfr)), verbosity=0, useCols=NULL,
	outName="out", dfrConvData, nfolds=10, type.measure="auc")
{
	catwif(verbosity > 0, "dim dfr:", dim(dfr))
	if(missing(dfrConvData))
	{
		dfrConvData<-dfrConversionProbs(dfr, "")
	}
	dfr.mat<-factorsToDummyVariables(dfr, betweenColAndLevel = "", dfrConvData=dfrConvData, verbosity=verbosity-1) #Some factors are only partially there!
	firstcolname<-colnames(dfr.mat)[1]
	catwif(verbosity > 0, "after factorsToDummyVariables dim dfr.mat:", dim(dfr.mat))
	if(!is.null(useCols))
	{
		if(is.character(useCols))
		{
			useCols<-useCols[useCols %in% colnames(dfr.mat)]
			catwif(verbosity > 0, "useCols:", useCols)
		}
		else
		{
			catwif(verbosity > 0, "useCols in positions:", useCols)
			catwif(verbosity > 0, "useCols:", colnames(dfr.mat)[useCols])
		}
		dfr.mat<-dfr.mat[,useCols, drop=FALSE]
	}
	catwif(verbosity > 0, "dim dfr.mat:", dim(dfr.mat), ", l(resp):", length(resp), ", l(wts):", length(wts), "\n")
	if(ncol(dfr.mat) == 0)
	{
		catwif(verbosity > 0, "No predictors, so we will do some faking...")
		dfr.mat<-matrix(runif(nrow(dfr)), ncol=1) #make dfr.mat one random column
		colnames(dfr.mat)<-firstcolname #just to make sure it is a 'known' column name
		catwif(verbosity > 5, "dfr.mat structure")
		if(verbosity > 5) str(dfr.mat)
		tmpln<-glmnet(dfr.mat, resp, family="binomial", weights=wts, standardize=FALSE)
		#get the biggest lambda there, and just multiply it by 10
		useLambda<-10*max(tmpln$lambda) #get huge value for lambda and hope it works to set coef to 0
	}
	else
	{
		#debug.tmp<<-list(dfr.mat=dfr.mat, resp=resp, wts=wts)
		catwif(verbosity > 0, "Checking for NAs in the data")
		naperrow<-apply(dfr.mat, 1, function(currow){sum(is.na(currow))})
		whichrowsna<-which(naperrow > 0)
		if(length(whichrowsna) > 0)
		{
			if(verbosity > 0)
			{
				catw("The following rows have NAs (names are rownumbers, values are nr of NAs):")
				tmp<-naperrow[whichrowsna]
				names(tmp)<-whichrowsna
				print(tmp)
				catw("These rows will now be removed from the dataset before fitting.")
			}
			dfr.mat<-dfr.mat[-whichrowsna,]
			resp<-resp[-whichrowsna]
			wts<-wts[-whichrowsna]
		}
		useLambda<-0
	}
	fit<-cv.glmnet(dfr.mat, resp, family="binomial", weights=wts, lambda=useLambda, 
		standardize=FALSE, nfolds=nfolds, type.measure=type.measure)
	catwif(verbosity > 0, "glm fit succeeded.")
	return(fit)
}

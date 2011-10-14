#' Get coefficients of a "cv.1l.emlasso" object
#' 
#' Get coefficients of a \code{\link{cv.1l.emlasso}} object
#' 
#' @param object \code{\link{cv.1l.emlasso}} object
#' @param foldname name that can be safely given to a column holding the fold number. 
#' 	If \code{NULL}, no such column is added
#' @param itername name that can be safely given to a column holding the iteration number
#' 	If \code{NULL}, no such column is added
#' @param onlyFinal if \code{TRUE}, then only the coefficients for the last iteration
#' 	(normally convergence) are returned
#' @param onlyEqualZeroPercent return only coefficients that are zero at most this fraction
#' 	of the repetitions/folds
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param form return the results as either a "data.frame", "matrix" or "sparse".
#' @param \dots passed on to \code{\link{coef.cvpart.emlasso}}
#' @return depending mostly on the parameter \code{form}, but typically: per fold/iteration
#' 	a row, per coefficient (that is nonzero often enough) a column
#' @method coef cv.1l.emlasso
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords coefficient
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlfit<-eml17$actualfits[[1]]$fitinfo
#' coef(emlfit, onlyFinal=TRUE)
#' @export
coef.cv.1l.emlasso<-function(object, foldname="fold", itername="iter",
	onlyFinal=FALSE, onlyEqualZeroPercent=0, verbosity=0, form=c("data.frame", "matrix", "sparse"),...)
{
	catwif(verbosity > 0, "Getting coefficient list")
	coeflst<-lapply(seq_along(object$actualfits), function(i){
			rv<-coef(object$actualfits[[i]], onlyFinal=onlyFinal, verbosity=verbosity-1, ...)
			if((! is.null(foldname)) && (nchar(foldname) > 0))
			{
				rv[[foldname]]<-i
			}
			if((! is.null(itername)) && (nchar(itername) > 0))
			{
				rv[[itername]]<-seq(nrow(rv))-1
			}
			rv
		})
	catwif(verbosity > 0, "Combining coefficients")
	rv<-combineSimilarDfrList(coeflst)
	remCols<- -match(c(foldname, itername), colnames(rv), nomatch=NULL)
	useCols<-seq_along(rv)
	if(length(remCols) > 0) useCols<-useCols[remCols]
	#can I do this faster with colMeans ??
	catwif(verbosity > 0, "colsZeroPercent")
	colsZeroPercent<-sapply(useCols, function(curcol){mean(rv[,curcol]==0, na.rm=TRUE)})
	catwif(verbosity > 0, colsZeroPercent)
	remCols<-useCols[colsZeroPercent > onlyEqualZeroPercent]
	if(length(remCols) > 0)
	{
		rv<-rv[,-remCols]
	}
	form <- match.arg(form)
	rv<-switch(form,
		data.frame = rv,
		matrix= as.matrix(rv),
		sparse= Matrix(data=as.matrix(rv), sparse=TRUE))
	return(rv)
}
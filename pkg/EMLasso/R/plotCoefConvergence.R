#' Plot coefficient evolution of EMLasso.lognet objects
#' 
#' Plot coefficient evolution of \code{\link{EMLasso.lognet}} objects over iterations
#' 
#' @param cofs matrix (or (sparse) \code{\link{Matrix}}) of coefficients: rows=iterations, cols=variables
#' @param skipForNZ when checking if a coefficient is ever nonzero, the first \code{skipForNZ} (run-in) iterations are skipped
#' @param zeroThres how high does a value have to be (in absolute value) to be nonzero.
#' @param colors colors used for the \code{\link{matplot}}
#' @param maxleg show legend for (at most) the first \code{maxleg} variables
#' @param leg.cex \code{cex} passed to \code{\link{legend}}
#' @param \dots passed on to \code{\link{matplot}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return nothing
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords crossvalidate model
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' require(addendum)
#' require(NumDfr)
#' require(GLoMo)
#' require(snowfall)
#' require(EMLasso)
#' sfInit(parallel = FALSE, cpus = 1)
#' sfLibrary(addendum)
#' sfLibrary(NumDfr)
#' sfLibrary(GLoMo)
#' sfLibrary(EMLasso)
#' iris.cpy<-randomNA(iris, n=0.1)
#' iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,  
#' 	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
#' 	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
#' sfStop()
#' plotCoefConvergence(iris.emlognet$result[[1]]$coefs, lty=1, xlab="iteration", ylab="coefficient")
#' @export
plotCoefConvergence<-function(cofs, skipForNZ=5, zeroThres=0.0001, colors=neatColorSet(), maxleg=10, 
															leg.cex=0.5, verbosity=0, ...)
{
	fmat<-as.matrix(cofs)
	skipForNZ<-seq(1, skipForNZ)
	notAllZero<-sapply(seq(ncol(fmat)), function(rn)
	{
		catwif(verbosity > 1, "checking all zeroes for variable", rn, "/", ncol(fmat))
		curcoefvals<-fmat[-skipForNZ,rn]
		cntNonZero<-sum(abs(curcoefvals)>zeroThres)
		catwif(verbosity > 1, "->Nonzero count:", cntNonZero)
		return(cntNonZero>0)
	})
	fmat<-fmat[,notAllZero, drop=FALSE]
	matplot(x=seq(nrow(cofs)), y=fmat, type="l", col=colors, ...)
	if(maxleg > 0)
	{
		nms<-colnames(fmat)
		if(length(nms) > maxleg) nms<-nms[seq(maxleg)]
		if(length(colors) < length(nms)) colors<-rep(colors, length.out=length(nms))
		legend("bottomleft", legend=nms, text.col=colors, cex=leg.cex)
	}
	abline(h=0, lty="dotted")
	invisible()
}

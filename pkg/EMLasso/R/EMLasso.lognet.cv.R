#' Run EMLasso.1l.lognet.cv for several lambda values through snowFall
#' 
#' Run \code{\link{EMLasso.1l.lognet.cv}} for several lambda values through snowFall
#' making use of \code{\link{run.parallel}}
#' 
#' @param \dots passed on to \code{\link{EMLasso.1l.lognet.cv.param}}
#' @param logdir path to folder where logfiles (and results) of each repeat are stored
#' @param saveTempResults if \code{TRUE}, save the results of each (parallelized) 
#' repetition, so they can be reobtained if something goes wrong with the others.
#' @return a list holding
#' \item{result }{list of \code{\link{cv.1l.emlasso.reduced}} objects per lambda} 
#' \item{params }{result of \code{\link{EMLasso.1l.lognet.cv.param}} call}
#' \item{logdir }{as passed in}
#' @note watch out! always reduced objects returned!
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso code{\link{cv.1l.emlasso.reduced}}, \code{\link{EMLasso.1l.lognet.cv.param}}, \code{\link{EMLasso.1l.lognet.cv}}
#' @references [PENDING]
#' @keywords EMLasso logistic LASSO EM
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlcvfit<-EMLasso.lognet.cv(aDfr.MD, y, lambda=c(0.05, 0.03), nrOfSamplesPerMDRow=7, verbosity=10)}
#' @export
EMLasso.lognet.cv<-function(...,
	logdir="./",
# 	logdir="/home/nsabbe/Dropbox/Doctoraat/Bayesian Lasso/EMLRun/",
	saveTempResults=TRUE)
{
	params<-do.call(EMLasso.1l.lognet.cv.param, list(...)) #in fact I should be able to get this from e1lc_param
	if(saveTempResults) savedir<-logdir else savedir<-NULL
	result<-run.parallel(..., paramcreationname="EMLasso.1l.lognet.cv.param",
		functionname="EMLasso.1l.lognet.cv", paramname="e1lc_param", logdir=logdir,
		savedir=savedir, postprocessname="postprocess.reduce.1l.emlasso", 
		loadLibsIfSfNotRunning=c("Matrix", "glmnet", "addendum", "NumDfr", "GLoMo", "EMLasso"))
	retval<-list(result=result, params=params, logdir=logdir)
	class(retval)<-"EMLasso.lognet.cv"
	return(retval)
}
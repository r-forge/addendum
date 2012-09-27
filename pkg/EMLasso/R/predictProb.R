#' predict probabilities
#' 
#' predict probabilities
#' 
#' @param object object that is used to predict the probabilities
#' @param newdata dataset for which the probabilities must be predicted
#' @param \dots provided for extensibility (S3)
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return vector of probabilities of the same length as \code{nrow(newdata)}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords predict probability
#' @export
predictProb<-function(object, newdata, ..., verbosity=0) UseMethod("predictProb")
#' @method predictProb lognetProbabilityReusable
#' @rdname predictProb
#' @inheritParams predictProb
#' @return vector of probabilities of the same length as \code{nrow(newdata)}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords predict probability
#' @examples data(iris)
#' 	iris.nd2<-numdfr(iris)
#' 	y2<-rbinom(nrow(iris), 1, 0.5)
#' 	iris.nic2<-normalImputationConversion(
#'		scalingParams=typicalScaleAndCenter(), 
#'		transformParams=typicalTransformations())
#' 	iris.cp2<-imputeDs2FitDsProps(iris.nic2,iris.nd2,verbosity=1)
#' 
#' 	iris.cvtd2<-imputeDs2FitDs(iris.cp2,ds=iris.nd2,verbosity=3)
#'
#' 	lnet<-glmnet(iris.cvtd2, y2, family="binomial")
#' 	lpw<-lognetProbabilityReusable(lnet, imputeDs2FitDsProperties=iris.cp2, iris.nd2, usecol=5, verbosity=1)
#' predictProb(lpw, iris.nd2[seq(20),], verbosity=10)
#' @export
predictProb.lognetProbabilityReusable<-function(object, newdata, ..., verbosity=0)
{
	catwif(verbosity > 10, "The betas are:")
	printif(verbosity > 10, object$betas)
	mat<-imputeDs2FitDs(object$imputeDs2FitDsProperties,newdata,verbosity=verbosity-1)
	mat<-mat[,names(object$betas),drop=FALSE]
	catwif(verbosity > 5, "Structure of converted and column-reduced matrix:")
	strif(verbosity > 5, mat)
	
	tmpres<-as.vector(mat %*% object$betas)
	catwif(verbosity > 5, "Result after matrix multiplication:")
	printif(verbosity > 5, tmpres)
	linval<-as.vector(tmpres) + object$a0 - object$penalty
# 	rvnopen<-expit(linval)
# 	catwif(verbosity > 5, "Linear values without penalty:")
# 	printif(verbosity > 5, linval)
	linval<- linval - object$penalty
	catwif(verbosity > 5, "Linear values:")
	printif(verbosity > 5, linval)
	rv<-expit(linval)
	
# 	catwif(verbosity > 5, "Unpenalized probability:")
# 	printif(verbosity > 5, rvnopen)
	catwif(verbosity > 5, "Penalized probability:")
	printif(verbosity > 5, rv)
	return(rv)
}
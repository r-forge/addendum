#' predict probabilities
#' 
#' predict probabilities
#' 
#' @param object object that is used to predict the probabilities
#' @param newdata dataset for which the probabilities must be predicted
#' @param \dots provided for extensibility (S3)
#' @return vector of probabilities of the same length as \code{nrow(newdata)}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords predict probability
#' @export
predictProb<-function(object, newdata, ...) UseMethod("predictProb")

#' @method predictProb lognetProbWrapper
#' @rdname predictProb
#' @inheritParams predictProb
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return vector of probabilities of the same length as \code{nrow(newdata)}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords predict probability
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' irisdummy<-factorsToDummyVariables(iris, verbosity=10)
#' lnet<-glmnet(irisdummy, y, family="binomial")
#' lpw<-lognetProbWrapper(lnet, iris, betweenColAndLevel="", usecol=5, verbosity=1)
#' predictProb(lpw, iris[seq(20),], verbosity=10)
#' @export
predictProb.lognetProbWrapper<-function(object, newdata, ..., verbosity=0)
{
	mat<-newdata[,object$conversionProps$fromCols, drop=TRUE]
	mat<-as.nummatrix(mat)
	nr<-nrow(mat)
	equalInd<-object$conversionProps$mustEqual != 1
	if(sum(equalInd) > 0)
	{
		catwif(verbosity > 0, "Categorical conversion needed for columns:", object$conversionProps$newNames[equalInd])
		tocomp<-matrix(rep.int(object$conversionProps$mustEqual[equalInd],nr),nr,byrow=TRUE)
		mat[,equalInd]<-as.integer(mat[,equalInd]==tocomp)
	}
	tmpres<-mat %*% object$conversionProps$useBeta
	linval<-as.vector(tmpres) + object$originalLognet$a0[object$usedcol]
	pp<-exp(-linval)
	return(1/(1 + pp))
}

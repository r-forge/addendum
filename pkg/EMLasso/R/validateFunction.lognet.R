#' validation function for use in \code{predict.conditional.allrows.GLoMo}
#' 
#' 'Validates' samples in the rejection sampling way, by checking the predicted
#' probability in the logistic LASSO
#' 
#' @param attempts \code{\link{numdfr}} or \code{\link{data.frame}} holding the
#' rows to validate (sampled from GLoMo)
#' @param otherData extra data that will be passed in. Required to be of class
#' \code{LognetValidationData}
#' @param forrow to which original row of the dataset do these predictions belong
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return integer vector of indices of accepted rows (within attempts)
#' @note noticed here that you need a way to map forrow (that is a rowindex in the
#' original dataset) to an item in otherData. This will not be an issue if forrows
#' that is passed to predict.conditional.allrows.GLoMo is really all rows,
#' otherwise (then otherData will likely only hold items only for every selected
#' row) you need more info...
#'
#' really only used internally from \code{\link{EMLasso.1l}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{predict.conditional.allrows.GLoMo}}, \code{\link{EMLasso.1l}}
#' @keywords GLoMo predict rejection sampling
#' @export
validateFunction.lognet<-function(attempts, otherData, forrow, verbosity=0)
{
	#ripped mostly from my old sample.conditional.predictor.fit.oneAttempt
	stopifnot(inherits(otherData, "SamplingReusablesLognet"))
	#for now, assume otherData$outcomes has one item for each potental forrow
	curOutcome<-otherData$outcomes[forrow]
	catwif(verbosity > 0, "original outcome: ", curOutcome)
	if(is.factor(curOutcome)) curOutcome<-as.integer(curOutcome)-1 #makes the first level the reference
	probYPos<-predictProb(otherData$valModelWrapper, newdata=attempts$predicted,
		verbosity=verbosity-1)
	if(curOutcome)
	{
		probYPos<-1-probYPos
	}
	#rejection sampling:
	return(which(runif(nrow(attempts$predicted)) <= probYPos))
}
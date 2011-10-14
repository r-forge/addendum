#' Sorts a list of \code{\link{cv.1l.emlasso.reduced}} by lambda
#' 
#' Sorts a list of \code{\link{cv.1l.emlasso.reduced}} by lambda
#' 
#' @param reducedResultList list of \code{\link{cv.1l.emlasso.reduced}}
#' @return similar list, but now sorted by lambda, and of class "reducedResultList"
#' @note used internally by \code{\link{getSortedReducedResultList}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords reduce list sort
#' @export
sortReducedResultList<-function(reducedResultList)
{
	lambdas<-sapply(reducedResultList, "[[", "lambda")
	reducedResultList<-reducedResultList[order(lambdas, decreasing=TRUE)] #sorted from big lambda to small
	class(reducedResultList)<-"reducedResultList"
	return(reducedResultList)
}
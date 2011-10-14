#' Postprocessing function for reduction - after save of object, before return
#' 
#' Postprocessing function for reduction - after save of object, before return
#' 
#' @param eml \code{\link{cv.1l.emlasso}} object
#' @param cparam \code{\link{EMLasso.1l.lognet.cv.param}} object
#' @param i indexer for the \code{i}th lambda
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return see \code{\link{reduce.cv.1l.emlasso}}
#' @note simply calls \code{\link{reduce.cv.1l.emlasso}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords reduce
#' @export
postprocess.reduce.1l.emlasso<-function(eml, cparam, i, verbosity)
{
	reduce.cv.1l.emlasso(eml, cparam$dfr, verbosity=verbosity-1)
}

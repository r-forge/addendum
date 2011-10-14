#' 
#' Reduce several cv.1l.emlasso objects (saved or not), sort and return as a new list
#' 
#' @param object list of \code{\link{cv.1l.emlasso}} objects, or path containing
#' saved such objects (per file), or path + filename pattern
#' @param orgdfr original dataset used to fit the EMLassos
#' @param \dots passed on to \code{\link{reduce.cv.1l.emlasso}} for each object
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param splitPatternLastPart which character(s) can be used to distinguish the
#' path from the filename pattern in \code{object} (if it is a character)
#' @return list, sorted by lambda, of \code{\link{cv.1l.emlasso.reduced}} objects,
#' one for every original object. The list is of class "reducedResultList"
#' @note These kinds of objects are saved during \code{\link{EMLasso.lognet.cv}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords reduce list
#' @examples data(emlcvfit, package="EMLasso")
#' \dontrun{eml17<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
#' eml20<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.03, nrOfSamplesPerMDRow=7, verbosity=10)}
#' redlist<-getSortedReducedResultList(list(eml17, eml20), verbosity=10)
#' @export
getSortedReducedResultList<-function(object, orgdfr, ..., verbosity=0, splitPatternLastPart="/")
{
	if(is.character(object))
	{
		parts<-strsplit(object, splitPatternLastPart)
		if(length(parts[[1]])==1)
		{
			catwif(verbosity>0, "object was character without directory")
			ptrn<-parts[[1]]
			dr<-"."
		}
		else
		{
			catwif(verbosity>0, "object was character with directory")
			ptrn<-parts[[1]][length(parts[[1]])]
			rmchrs<-nchar(ptrn)
			dr<-substr(object, 1, nchar(object)-rmchrs)
			catwif(verbosity>0, "dr=", dr)
			catwif(verbosity>0, "ptrn=", ptrn)
		}
		object<-list.files(dr, full.names=TRUE, pattern=ptrn)
		catwif(verbosity > 5, "resulting files:")
		printif(verbosity > 5, object)
	}
	else if(inherits(object, "EMLasso.lognet.cv"))
	{
		catwif(verbosity>0, "object was EMLasso.lognet.cv")
		#result=result, params=params, logdir=logdir
		object<-object$result
	}
	missingOrgDfr<-missing(orgdfr)
	reducedResultList<-lapply(seq_along(object), function(i){
			catwif(verbosity>1, "item", i, "/", length(object))
			if(! inherits(object[[i]], "cv.1l.emlasso.reduced"))
			{
				if(! inherits(object[[i]], "cv.1l.emlasso"))
				{
					catwif(verbosity>1, "Needed cv.1l.emlasso, got: ", class(object[[i]]))
				}
				if(missingOrgDfr) #missing doesn't work in nested functions!!
				{
					reduce(object[[i]], ..., verbosity=verbosity-2)
				}
				else
				{
					reduce(object[[i]], orgdfr, ..., verbosity=verbosity-2)
				}
			}
			else
			{
				catwif(verbosity >2, "using reduced detail as is.")
				object[[i]]
			}
		})
	return(sortReducedResultList(reducedResultList))
}
numdfr<-function(dfr)
{
	lvls<-lapply(dfr, function(cc){if(is.factor(cc)) return(levels(cc)) else return(character(0))})
	mat<-matrix(unlist(dfr), ncol=ncol(dfr))
	colnames(mat)<-colnames(dfr)
	rownames(mat)<-rownames(dfr)
	retval<-list(mat=mat, lvls=lvls)
	class(retval)<-"numdfr"
	return(retval)
}

#returnAsMatrix TRUE will just return the matrix with the original dimensions
#If drop _AND_ reduceIfPossible are both TRUE, then the 'typical' dimension
#   reduction is applied to the resulting matrix
"[.numdfr"<-function (x, i, j, returnAsMatrix = drop, drop = FALSE)
{
	.debugtxt("[.numdfr")
	.debugtxt(ifelse(missing(i), "no i", paste("i:", i)))
	.debugtxt(ifelse(missing(j), "no j", paste("j:", j)))
	.debugtxt("returnAsMatrix:", returnAsMatrix)
	.debugtxt("drop:", drop)
	if(returnAsMatrix)
	{
		return(x$mat[i,j, drop=drop])
	}
	newlvls<-if (missing(j)) x$lvls else x$lvls[j]
	newmat<-x$mat[i,j, drop=FALSE]
	retval<-list(mat=newmat, lvls=newlvls)
	class(retval)<-"numdfr"
	return(retval)
}

"[<-.numdfr"<-function (x, i, j, value)
{
	#.debugtxt("[<-.numdfr")
	if(is.numdfr(value)) value<-value$mat
	x$mat[i,j]<-value
	return(x)
}

length.numdfr<-function(x){
	.debugtxt("length.numdfr")
	ncol(x$mat)
}

dimnames.numdfr<-function(x){
	.debugtxt("dimnames.numdfr")
	dimnames(x$mat)
}

"dimnames<-.numdfr"<-function(x, value){
	.debugtxt("dimnames<-.numdfr")
	dimnames(x$mat)<-value
	names(x$lvls)<-value[[2]]
	return(x)
}

dim.numdfr<-function(x){
	.debugtxt("dim.numdfr")
	dim(x$mat)
}
#"dim<-.numdfr" #similar as data.frame: not directly supported!!

names.numdfr<-function(x){
	.debugtxt("names.numdfr")
	colnames(x$mat)
}

"names<-.numdfr"<-function(x, value){
	.debugtxt("names<-.numdfr")
	colnames(x$mat)<-value
	names(x$lvls)<-value
	return(x)
}

is.numdfr<-function(x){
	.debugtxt("is.numdfr")
	inherits(x, "numdfr")
}

as.double.numdfr<-function(x,...){
	.debugtxt("as.double.numdfr")
	x$mat
}

is.na.numdfr<-function(x){is.na(x$mat)}

str.numdfr<-function(object,...){
	cat("numdfr object with dimensions:", dim(object), "\n")
	cat("->Rownames: ", rownames(object), "\n")
	cat("->Colnames: ", colnames(object), "\n")
	cat("\nThe following variables are factor-like:\n")
	for(i in findCatColNums(object))
	{
		cat("\t", names(object$lvls)[i], ":", object$lvls[[i]], "\n")
	}
	invisible()
}

#This one will make sure that lapply/sapply works similarly on numdfr as it does 
#		on data.frame,though I suspect quite a performance penalty (???)
as.list.numdfr<-function(x, returnFactors=TRUE,...){
	.debugtxt("as.list.numdfr")
	rv<-lapply(seq(ncol(x$mat)), function(cc){
			lev<-x$lvls[[cc]]
		  if((length(lev) > 0) & (returnFactors==TRUE))
		  {
			  quickFactor(x$mat[,cc], labels=lev)#really fast
			}
			else
			{
				x$mat[,cc]
			}
		})
	names(rv)<-colnames(x$mat)
	return(rv)
}
#IMPORTANT NOTE: need to check ?InternalMethods and ?methods
#It appears I can write custom versions of a lot more interesting functions (amongst which dim)
#Done at least part of it now: see above with dim and length and the likes

as.data.frame.numdfr<-function(x, row.names = NULL, optional = FALSE, ...)
{
	value<-as.list(x, returnFactors=TRUE,...)
	if(is.null(row.names))
	{
		attr(value, "row.names") <- postfixToMakeUnique(rownames(x$mat))
	}
	else
	{
		attr(value, "row.names") <- row.names
	}
	class(value) <- "data.frame"
	value
}

findCatColNums.numdfr<-function(dfr){
	.debugtxt("findCatColNums.numdfr")
	which(sapply(dfr$lvls, length) > 0)
}

#note: _assumes_ all parameters are numdfr of the same structure!!
#Will probably not fail if this is not the case, but results are unpredictable
rbind.numdfr<-function(..., ensure.unique.rownames=FALSE, separator=".", postfixcol=NULL, allowemptypostfix=TRUE, deparse.level = 1)
{
	allparams<-list(...)
	allmats<-lapply(allparams, "[[", "mat")
	newmat<-do.call(rbind, allmats)
	if((ensure.unique.rownames) & (!is.null(rownames(newmat))))
	{
		rownames(newmat)<-postfixToMakeUnique(rownames(newmat), separator=separator,
			postfixcol=postfixcol, allowemptypostfix=allowemptypostfix)
	}
	retval<-list(mat=newmat, lvls=(allparams[[1]])$lvls)
	class(retval)<-"numdfr"
	return(retval)
}

print.numdfr<-function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
    row.names = TRUE)
{
	#note: code mostly ripped from print.data.frame :-)
	#mostly with aim of easily reusing format.data.frame
    n <- length(row.names(x))
    if (length(x) == 0L) {
        cat(gettextf("numdfr with 0 columns and %d rows\n",
            n))
    }
    else if (n == 0L) {
        print.default(names(x), quote = FALSE)
        cat(gettext("<0 rows> (or 0-length row.names)\n"))
    }
    else {
				x2<-as.data.frame.numdfr(x)
        m <- as.matrix(format.data.frame(x2, digits = digits,
            na.encode = FALSE))
        if (!isTRUE(row.names))
            dimnames(m)[[1L]] <- if (identical(row.names, FALSE))
                rep.int("", n)
            else row.names
        print(m, ..., quote = quote, right = right)
    }
    invisible(x)
}
#if(FALSE)
#{
#	#do either to turn on/off debug text
#	.numdfr.debugmode<-TRUE
#	.numdfr.debugmode<-FALSE
#}

.numdfr_debugmode <- function() {
  .debugging <- FALSE

  list(
    get = function() .debugging,
    set = function(value) .debugging <<- value
  )
}
.actual_debugmode <- .numdfr_debugmode()
.isNumdfrDebugging<-function(){.actual_debugmode$get()}

setDebugmode<-function(doDebug=TRUE){
	oldDebug<-.actual_debugmode$get()
	.actual_debugmode$set(doDebug)
	invisible(oldDebug)
}
.debugtxt<-function(...){if(.isNumdfrDebugging()) cat("**D:", ..., "\n")}


factorsToDummyVariables.numdfr<-function(dfr, betweenColAndLevel = "",...)
{
	.debugtxt("factorsToDummyVariables.numdfr")
	nc<-dim(dfr$mat)[2]
	nr<-dim(dfr$mat)[1]
	coln<-colnames(dfr$mat)
	retval<-do.call(cbind, lapply(seq(nc), function(ci){
			lvls<-dfr$lvls[[ci]]
			if(length(lvls)>0)
			{
				lvls<-lvls[-1]
				stretchedcols<-dfr$mat[,rep(ci, length(lvls))]
				comparelvls<-matrix(rep(seq_along(lvls)+1, each=nr), nrow=nr)
				stretchedcols<-stretchedcols==comparelvls
				mode(stretchedcols)<-"integer"
				if(!is.matrix(stretchedcols)){
					warning("In factorsToDummyVariables.numdfr: stretchedcols was not a matrix?")
					stretchedcols<-matrix(stretchedcols, nrow=1)}
				colnames(stretchedcols)<-paste(coln[ci], lvls, sep=betweenColAndLevel)
				return(stretchedcols)
			}
			else
			{
				curcol<-dfr$mat[,ci, drop=FALSE]
				return(curcol)
			}
		}))
	rownames(retval)<-rownames(dfr)
	return(retval)
}


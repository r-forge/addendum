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
#	.debugtxt(ifelse(missing(i), "no i", paste("i:", i)))
#	.debugtxt(ifelse(missing(j), "no j", paste("j:", j)))
#	.debugtxt("returnAsMatrix:", returnAsMatrix)
#	.debugtxt("drop:", drop)
	if(returnAsMatrix)
	{
		return(.getMatrix(x)[i,j, drop=drop])
	}
	newlvls<-if (missing(j)) .getLevels(x) else .getLevels(x)[j]
	newmat<-.getMatrix(x)[i,j, drop=FALSE]
	retval<-list(mat=newmat, lvls=newlvls)
	class(retval)<-"numdfr"
	return(retval)
}

"[<-.numdfr"<-function (x, i, j, value)
{
	.debugtxt("[<-.numdfr")
	if(is.numdfr(value)) value<-.getMatrix(value)
	if(any(is.character(value)))
	{
		#probably means the user is assigning like a factor
		#For now, we'll make some assumptions on how this assignment occurs
		#find out which columns that are being assigned are categorical:
		if(missing(i)) tmpi<-seq(nrow(x)) else tmpi<-i
		if(missing(j)) tmpj<-seq(ncol(x)) else tmpj<-j
		tmplvls<-.getLevels(x)[tmpj]
		if(is.array(value)) tmpvalue<-value else tmpvalue<-array(value, dim=c(length(tmpi), length(tmpj)))
		for(curcoli in which(sapply(tmplvls, length)>0))
		{
			newvals<-match(tmpvalue[,curcoli], tmplvls[[curcoli]], nomatch=-1)
			unmatched<-which(newvals==-1)
			if(length(unmatched) > 0)
			{
				stop(paste("Tried to assign invalid 'factor' values: ", tmpvalue[unmatched,curcoli], "to column", tmpj[curcoli]))
			}
			tmpvalue[,curcoli]<-newvals
		}
		value<-as.numeric(tmpvalue) #if this gives a warning, there were probably some errors...
	}
	oldclass<-class(x)
	x<-unclass(x)
	x$mat[i,j]<-value
	class(x)<-oldclass
	return(x)
}

length.numdfr<-function(x){
	.debugtxt("length.numdfr")
	ncol(.getMatrix(x))
}

dimnames.numdfr<-function(x){
	.debugtxt("dimnames.numdfr")
	dimnames(.getMatrix(x))
}

"dimnames<-.numdfr"<-function(x, value){
	.debugtxt("dimnames<-.numdfr")
	oldclass<-class(x)
	x<-unclass(x)
	dimnames(x$mat)<-value
	names(x$lvls)<-value[[2]]
	class(x)<-oldclass
	return(x)
}

dim.numdfr<-function(x){
	.debugtxt("dim.numdfr")
	dim(.getMatrix(x))
}
#"dim<-.numdfr" #similar as data.frame: not directly supported!!

names.numdfr<-function(x){
	.debugtxt("names.numdfr")
	colnames(.getMatrix(x))
}

"names<-.numdfr"<-function(x, value){
	.debugtxt("names<-.numdfr")
	oldclass<-class(x)
	x<-unclass(x)
	colnames(x$mat)<-value
	names(x$lvls)<-value
	class(x)<-oldclass
	return(x)
}

is.numdfr<-function(x){
	.debugtxt("is.numdfr")
	inherits(x, "numdfr")
}

as.double.numdfr<-function(x,...){
	.debugtxt("as.double.numdfr")
	.getMatrix(x)
}

is.na.numdfr<-function(x){
	.debugtxt("is.na.numdfr")
	is.na(.getMatrix(x))
}

str.numdfr<-function(object,...){
	cat("numdfr object with dimensions:", dim(object), "\n")
	cat("->Rownames: ", rownames(object), "\n")
	cat("->Colnames: ", colnames(object), "\n")
	cat("\nThe following variables are factor-like:\n")
	lvls<-.getLevels(object)
	for(i in findCatColNums(object))
	{
		cat("\t", names(lvls)[i], ":", lvls[[i]], "\n")
	}
	invisible()
}

#This one will make sure that lapply/sapply works similarly on numdfr as it does 
#		on data.frame,though I suspect quite a performance penalty (???)
as.list.numdfr<-function(x, returnFactors=TRUE,...){
	.debugtxt("as.list.numdfr")
	x<-unclass(x)
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
	.debugtxt("as.data.frame.numdfr")
	value<-as.list(x, returnFactors=TRUE,...)
	if(is.null(row.names))
	{
		attr(value, "row.names") <- postfixToMakeUnique(rownames(.getMatrix(x)))
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
	which(sapply(.getLevels(dfr), length) > 0)
}

#note: _assumes_ all parameters are numdfr of the same structure!!
#Will probably not fail if this is not the case, but results are unpredictable
rbind.numdfr<-function(..., ensure.unique.rownames=FALSE, separator=".", postfixcol=NULL, allowemptypostfix=TRUE, deparse.level = 1)
{
	.debugtxt("rbind.numdfr")
	allparams<-list(...)
	allmats<-lapply(allparams, .getMatrix)
	newmat<-do.call(rbind, allmats)
	if((ensure.unique.rownames) & (!is.null(rownames(newmat))))
	{
		rownames(newmat)<-postfixToMakeUnique(rownames(newmat), separator=separator,
			postfixcol=postfixcol, allowemptypostfix=allowemptypostfix)
	}
	retval<-list(mat=newmat, lvls=.getLevels((allparams[[1]])))
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

display.numdfr<-function(dfr)
{
	.debugtxt("display.numdfr")
	display(as.data.frame(dfr))
}

as.matrix.numdfr<-function(x, ...)
{
	return(.getMatrix(x))
}

allLevels<-function(x, onlyNonEmpty=FALSE) UseMethod("allLevels")
allLevels.numdfr<-function(x, onlyNonEmpty=FALSE){
	.debugtxt("allLevels.numdfr")
	lvls<-.getLevels(x)
	if(! onlyNonEmpty)
	{
		return(lvls)
	}
	else
	{
		keep<-sapply(lvls, length) > 0
		return(lvls[keep])
	}
}
allLevels.data.frame<-function(x, onlyNonEmpty=FALSE){
	retval<-lapply(x, function(curcol){
			tmp<-levels(curcol)
			if(is.null(tmp)) tmp<-character(0)
			return(tmp)
		})
	if(onlyNonEmpty)
	{
		keep<-sapply(retval, length) > 0
		retval<-retval[keep]
	}
	return(retval)
}

.findIndexOfColumnName<-function(x, name, exact=TRUE)
{
	.debugtxt(".findIndexOfColumnName")
	if(exact)
	{
		match(name, colnames(x))
	}
	else
	{
		pmatch(name, colnames(x), duplicates.ok=TRUE)
	}
}

"[[.numdfr"<-function(x, ..., exact=TRUE)
{
	.debugtxt("[[.numdfr")
	thecol<-unlist(as.list(...))
	if(length(thecol) != 1) stop("Unsupported operation: passing more than one paramter to [[.numdfr")
	thelvls<-.getLevels(x)[[thecol]]
	thecol<-.getMatrix(x)[,thecol]
	if(length(thelvls) > 0)
	{
		return(quickFactor(thecol, labels=thelvls))
	}
	return(thecol)
}

"$.numdfr"<-function(x, name)
{
	.debugtxt("$.numdfr")
	return("[[.numdfr"(x, name, exact=TRUE))
}

.getMatrix<-function(x)
{
	#.subset2 is like "[[" but without method dispatch!
	return(.subset2(x, "mat", exact=TRUE))
}

.getLevels<-function(x)
{
	#.subset2 is like "[[" but without method dispatch!
	return(.subset2(x, "lvls", exact=TRUE))
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
	mat<-.getMatrix(dfr)
	lvls<-.getLevels(dfr)
	nc<-dim(mat)[2]
	nr<-dim(mat)[1]
	coln<-colnames(mat)
	retval<-do.call(cbind, lapply(seq(nc), function(ci){
			clvls<-lvls[[ci]]
			if(length(clvls)>0)
			{
				clvls<-clvls[-1]
				stretchedcols<-mat[,rep(ci, length(clvls))]
				comparelvls<-matrix(rep(seq_along(clvls)+1, each=nr), nrow=nr)
				stretchedcols<-stretchedcols==comparelvls
				mode(stretchedcols)<-"integer"
				if(!is.matrix(stretchedcols)){
					warning("In factorsToDummyVariables.numdfr: stretchedcols was not a matrix?")
					stretchedcols<-matrix(stretchedcols, nrow=1)}
				colnames(stretchedcols)<-paste(coln[ci], clvls, sep=betweenColAndLevel)
				return(stretchedcols)
			}
			else
			{
				curcol<-mat[,ci, drop=FALSE]
				return(curcol)
			}
		}))
	rownames(retval)<-rownames(mat)
	return(retval)
}


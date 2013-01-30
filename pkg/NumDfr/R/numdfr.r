numdfr<-function(dfr)
{
	lvls<-lapply(dfr, function(cc){if(is.factor(cc)) return(levels(cc)) else return(character(0))})
	ords<-allOrderedFacts(dfr, onlyNonEmpty=FALSE)
	mat<-matrix(unlist(dfr), ncol=ncol(dfr))
	colnames(mat)<-colnames(dfr)
	rownames(mat)<-rownames(dfr)
	retval<-list(mat=mat, lvls=lvls, ord=ords)
	class(retval)<-"numdfr"
	return(retval)
}

#returnAsMatrix TRUE will just return the matrix with the original dimensions
#If drop _AND_ reduceIfPossible are both TRUE, then the 'typical' dimension
#   reduction is applied to the resulting matrix
"[.numdfr"<-function (x, i, j, returnAsMatrix = drop, drop = FALSE)
{
	.debugtxt()
#	.debugtxt(ifelse(missing(i), "no i", paste("i:", i)))
#	.debugtxt(ifelse(missing(j), "no j", paste("j:", j)))
#	.debugtxt("returnAsMatrix:", returnAsMatrix)
#	.debugtxt("drop:", drop)
	if(returnAsMatrix)
	{
		return(.getMatrix(x)[i,j, drop=drop])
	}
	if(missing(j) && is.matrix(i) && (length(dim(i))==2)  )
	{
		#NS 20120612: I think there are some errors in this case.
		#Why would this function ever return a list ??
		
		rv<-.getMatrix(x)[i]#don't specify drop=FALSE or the wrong kind of getter is used...
		usedcols<-unique(i[,2])
		newlvls<-.getLevels(x)[usedcols]
		neword<-.getOrdered(x)[usedcols]
		factors<-!(sapply(newlvls, is.null))
		if(sum(factors) > 0)
		{
			rv<-as.list(rv)
			names(rv)<-rownames(i)
			for(lvli in seq_along(newlvls))
			{
				if(factors[lvli])
				{
					wrv<-which(i[,2]==usedcols[lvli])
					for(j in wrv)
					{
						rv[[j]]<-quickFactor(x=rv[[j]], labels=newlvls[[lvli]], ordered=neword[lvli])
					}
				}
			}
		}
		return(rv)
	}
	else
	{
		newlvls<-if (missing(j)) .getLevels(x) else .getLevels(x)[j]
		newords<-if (missing(j)) .getOrdered(x) else .getOrdered(x)[j]
		newmat<-.getMatrix(x)[i,j, drop=FALSE]
	}
	retval<-list(mat=newmat, lvls=newlvls, ord=newords)
	class(retval)<-"numdfr"
	return(retval)
}

"[<-.numdfr"<-function (x, i, j, value)
{
	.debugtxt()
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
	.debugtxt()
	ncol(.getMatrix(x))
}

dimnames.numdfr<-function(x){
	.debugtxt()
	dimnames(.getMatrix(x))
}

"dimnames<-.numdfr"<-function(x, value){
	.debugtxt()
	oldclass<-class(x)
	x<-unclass(x)
	dimnames(x$mat)<-value
	names(x$lvls)<-value[[2]]
	names(x$ord)<-value[[2]]
	class(x)<-oldclass
	return(x)
}

dim.numdfr<-function(x){
	.debugtxt()
	dim(.getMatrix(x))
}
#"dim<-.numdfr" #similar as data.frame: not directly supported!!

names.numdfr<-function(x){
	.debugtxt()
	colnames(.getMatrix(x))
}

"names<-.numdfr"<-function(x, value){
	.debugtxt()
	oldclass<-class(x)
	x<-unclass(x)
	colnames(x$mat)<-value
	names(x$lvls)<-value
	names(x$ord)<-value
	class(x)<-oldclass
	return(x)
}

is.numdfr<-function(x){
	.debugtxt()
	inherits(x, "numdfr")
}

as.double.numdfr<-function(x,...){
	.debugtxt()
	.getMatrix(x)
}

is.na.numdfr<-function(x){
	.debugtxt()
	is.na(.getMatrix(x))
}

str.numdfr<-function(object, short=TRUE,...){
	cat("numdfr object with dimensions:", dim(object), "\n")
	rn<-rownames(object)
	nrn<-length(rn)
	cn<-colnames(object)
	ncn<-length(cn)
	shortMaxRN<-10
	shortMaxCN<-10
	rntxt<-"->Rownames: "
	cntxt<-"->Colnames: "
	if(short)
	{
		if(nrn > shortMaxRN)
		{
			rntxt<-"->Some rownames: "
			rn<-c(rn[1:2], rn[sort(2+sample.int(nrn-4, shortMaxRN-4))], rn[nrn-c(1,0)])
		}
		if(ncn > shortMaxCN)
		{
			cntxt<-"->Some colnames: "
			cn<-c(cn[1:2], cn[sort(2+sample.int(ncn-4, shortMaxRN-4))], cn[ncn-c(1,0)])
		}
	}
	cat(rntxt, rn, "\n", fill=TRUE)
	cat(cntxt, cn, "\n", fill=TRUE)

	shortMaxFN<-10
	lvls<-.getLevels(object)
	ords<-.getOrdered(object)
	ccns<-findCatColNums(object)
	lccns<-length(ccns)
	if(short && (lccns > shortMaxFN))
	{
		ccns<-c(ccns[1:2], ccns[sort(2+sample.int(lccns-4, shortMaxFN-4))], ccns[lccns-c(1,0)])
		cat("\nThe following variables are some of the factor-like (* is ordered):\n")
	}
	else
	{
		cat("\nThe following variables are factor-like (* is ordered):\n")
	}
	
	lvltxts<-sapply(ccns, function(ccn){paste(lvls[[ccn]], collapse=" ")})
	
	ccoltxt<-paste("\t", colnames(object)[ccns], ifelse(ords[ccns], "*", ""), ":", lvltxts, " ; ")
	
	cat(ccoltxt, "\n", fill=TRUE)
	invisible()
}

#This one will make sure that lapply/sapply works similarly on numdfr as it does 
#		on data.frame,though I suspect quite a performance penalty (???)
as.list.numdfr<-function(x, returnFactors=TRUE,...){
	.debugtxt()
	x<-unclass(x)
	rv<-lapply(seq(ncol(x$mat)), function(cc){
			lev<-x$lvls[[cc]]
			ord<-x$ord[cc]
			if((length(lev) > 0) & (returnFactors==TRUE))
		  {
			  quickFactor(x$mat[,cc], labels=lev, ordered=ord)#really fast
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
	.debugtxt()
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
	.debugtxt()
	which(sapply(.getLevels(dfr), length) > 0)
}

findOrderedColNums.numdfr<-function(dfr){
	.debugtxt()
	which(.getOrdered(dfr))
}

#note: _assumes_ all parameters are numdfr of the same structure!!
#Will probably not fail if this is not the case, but results are unpredictable
rbind.numdfr<-function(..., ensure.unique.rownames=FALSE, separator=".", postfixcol=NULL, allowemptypostfix=TRUE, deparse.level = 1)
{
	.debugtxt()
	allparams<-list(...)
	if(length(allparams)==0) return(NULL)
	if(length(allparams)==1) return(allparams[[1]])
	allmats<-lapply(allparams, .getMatrix)
	newmat<-do.call(rbind, allmats)
	if((ensure.unique.rownames) & (!is.null(rownames(newmat))))
	{
		rownames(newmat)<-postfixToMakeUnique(rownames(newmat), separator=separator,
			postfixcol=postfixcol, allowemptypostfix=allowemptypostfix)
	}
	retval<-list(mat=newmat, lvls=.getLevels(allparams[[1]]), ord=.getOrdered(allparams[[1]]))
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
	.debugtxt()
	display(as.data.frame(dfr))
}

as.matrix.numdfr<-function(x, ...)
{
	return(.getMatrix(x))
}

allLevels.numdfr<-function(x, onlyNonEmpty=FALSE){
	.debugtxt()
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

allOrderedFacts.numdfr<-function(x, onlyNonEmpty=FALSE){
	.debugtxt()
	ord<-.getOrdered(x)
	if(! onlyNonEmpty)
	{
		return(ord)
	}
	else
	{
		keep<-sapply(.getLevels(x), length) > 0
		return(ord[keep])
	}
}

.findIndexOfColumnName<-function(x, name, exact=TRUE)
{
	.debugtxt()
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
	.debugtxt()
	thecol<-unlist(as.list(...))
	if(length(thecol) != 1) stop("Unsupported operation: passing more than one paramter to [[.numdfr")
	thelvls<-.getLevels(x)[[thecol]]
	theord<-.getOrdered(x)[thecol]
	thecol<-.getMatrix(x)[,thecol]
	if(length(thelvls) > 0)
	{
		return(quickFactor(thecol, labels=thelvls, ordered=theord))
	}
	return(thecol)
}

"[[<-.numdfr"<-function(x, i, value)
{
	.debugtxt()
	lvls<-.getLevels(x)
	ord<-.getOrdered(x)
	mat<-.getMatrix(x)
	if(length(value) > nrow(mat)) stop("Cannot add more items than there are observations")
	thecol<-i
	therows<-nrow(mat)

	if(is.character(thecol))
	{
		nm<-thecol
		ci<-match(thecol, colnames(mat))
		if(is.na(ci))
		{
			ci<-ncol(mat)+1
			mat<-cbind(mat, NA)
			lvls<-c(lvls, list(character()))
			ord<-c(ord, FALSE)
		}
	}
	else
	{
		if(thecol > ncol(mat))
		{
			nfn<-findNextFreeNr(colnames(mat))
			newnms<-paste(".", seq.int(nfn+1, length.out=thecol - ncol(mat)), sep="")
			newcols<-length(newnms)
			nm<-newnms[newcols]
			newpos<-seq.int(ncol(mat)+1, length.out=newcols)
			mat<-cbind(mat, matrix(NA, nrow=nrow(mat), ncol=newcols))
			colnames(mat)[newpos]<-newnms
			lvls<-c(lvls, lapply(newnms, function(nm){character()}))
			ord<-c(ord, rep(FALSE, newcols))
			names(lvls)[newpos]<-newnms
			names(ord)[newpos]<-newnms
			ci<-ncol(mat)
		}
		else
		{
			ci<-thecol
			nm<-colnames(mat)[ci]
		}
	}
	if(is.character(value)) value<-as.factor(value)
	if(is.factor(value))
	{
		mat[,ci]<-as.numeric(as.integer(value))
		lvls[[ci]]<-levels(value)
		ord[[ci]]<-is.ordered(value)
	}
	else
	{
		mat[,ci]<-value
	}
	colnames(mat)[ci]<-nm
	names(lvls)[ci]<-nm
	names(ord)[ci]<-nm
	
	retval<-list(mat=mat, lvls=lvls, ord=ord)
	class(retval)<-"numdfr"
	return(retval)
}
	
"$.numdfr"<-function(x, name)
{
	.debugtxt()
	return("[[.numdfr"(x, name, exact=TRUE))
}

"$<-.numdfr"<-function(x, name, value)
{
	.debugtxt()
	return("[[<-.numdfr"(x=x, i=name, value=value))
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

.getOrdered<-function(x)
{
	#.subset2 is like "[[" but without method dispatch!
	#support for old versions: if ord is not present
	rv<-.subset2(x, "ord", exact=TRUE)
	if(is.null(rv))
	{
		warning("Ordered property for old instance of numdfr requested (no ord present). May render unexpected results.")
		rv<-.getLevels(x)
		return(rep(FALSE, length(rv)))
	}
	return(rv)
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

setDebugmodeNumDfr<-function(doDebug=TRUE){
	oldDebug<-.actual_debugmode$get()
	.actual_debugmode$set(doDebug)
	invisible(oldDebug)
}
.debugtxt<-function(...)
{
	if(.isNumdfrDebugging())
	{
		if(length(list(...))==0)
		{
			cat("**D:", curfnfinder(skipframes=2, extraPrefPerLevel=""), "\n")
		}
		else
		{
			cat("**D:", ..., "\n")
		}
	}
}


#therow<-runif(300)
#nrofrows<-500
#
#> system.time(replicate(1000, {matrix(rep(therow, each=nrofrows), ncol=length(therow));1}))
#   user  system elapsed
#   4.08    1.62    5.85
#
#> system.time(replicate(1000, {t(matrix(rep(therow, nrofrows), nrow=length(therow)));1}))
#   user  system elapsed
#   5.72    1.92    7.83
#
#> system.time(replicate(1000, {matrix(rep(therow, nrofrows), ncol=length(therow), byrow=TRUE);1}))
#   user  system elapsed
#   4.80    1.69    6.64
#> system.time(replicate(1000, {matrix(rep.int(therow,nrofrows),nrofrows,byrow=TRUE);1}))
#   user  system elapsed
#   2.73    1.02    3.78

#note:
#tmp<-matrix(rep.int(therow,nrofrows),nrofrows,byrow=TRUE)
#
#system.time(replicate(1000, {tmp<-t(tmp);1}))

#.factorsToDummyVariables.simpleloop.numdfr<-function(dfr, betweenColAndLevel = "",...)
#{#19 secs
#	.debugtxt()
#	mat<-.getMatrix(dfr)
#	lvls<-.getLevels(dfr)
#	nc<-dim(mat)[2]
#	nr<-dim(mat)[1]
#	reps<-sapply(lvls, length)-1
#	reps[reps<1]<-1
#
#	repcols<-rep(seq_along(reps), reps)
#	retval<-mat[, repcols, drop=FALSE] #already the right size!
#	#note: this retained the original column names!!
#
#	curnewcol<-1
#	newcolnames<-colnames(retval)
#	for(i in seq_along(reps))
#	{
#		curlvls<-lvls[[i]]
#		l<-length(curlvls)
#		if(l > 1)
#		{
#			for(j in seq(2,l))
#			{
#				newcolnames[curnewcol]<-paste(newcolnames[curnewcol], curlvls[j], sep=betweenColAndLevel)
#				retval[,curnewcol]<-as.integer(retval[,curnewcol]==j)
#				curnewcol<-curnewcol+1
#			}
#		}
#		else
#		{
#			curnewcol<-curnewcol+1
#		}
#	}
#
#	colnames(retval)<-newcolnames
#	return(retval)
#}

#note: this is now exactly the same as factorsToDummyVariables.default
#apart from the .debugtxt() and some comments
factorsToDummyVariables.numdfr<-function(dfr, betweenColAndLevel = "", dfrConvData, verbosity=0,...)
{
	.debugtxt()
	factorsToDummyVariables.default(dfr=dfr, betweenColAndLevel = betweenColAndLevel, dfrConvData = dfrConvData, verbosity=verbosity,...)
}
#
#	orgfactcols<-which(reps>1)
#	startoforgcolinnewmat<-cumsum(c(1, reps))[seq_along(reps)]
#	mustmatchforfactcols<-do.call(c, lapply(reps[orgfactcols], seq))+1
#	newcolsfromfact<-rep(startoforgcolinnewmat[orgfactcols], reps[orgfactcols]) + mustmatchforfactcols -2
#
#	#tocomp<-matrix(rep(mustmatchforfactcols, each=nr), ncol=length(mustmatchforfactcols))
#	tocomp<-matrix(rep.int(mustmatchforfactcols,nr),nr,byrow=TRUE)
#	retval[,newcolsfromfact]<-ifelse(retval[,newcolsfromfact]==tocomp, 1, 0)
#
#	colexs<-do.call(c, lapply(lvls[orgfactcols], function(curlvls){if(length(curlvls) > 1) curlvls[-1] else ""}))
#	coln<-rep(colnames(mat), reps)
#	coln[newcolsfromfact]<-paste(coln[newcolsfromfact], colexs, sep=betweenColAndLevel)
#	colnames(retval)<-coln
#	return(retval)
#}

#	nendcols<-ncol(retval)
#
#	lcoln<-character(nendcols)
#	lcolex<-character(nendcols)
#	lvlmustbe<-integer(nendcols)
#	posinend<-1
#	for(i in seq_along(reps)) #i is index of original column name
#	{
#
#	}
#	extcoln<-lapply(lvls, function(curlvls){})
#	retval<-do.call(cbind, lapply(seq(nc), function(ci){
#			clvls<-lvls[[ci]]
#			if(length(clvls)>0)
#			{
#				clvls<-clvls[-1]
#				stretchedcols<-mat[,rep(ci, length(clvls))]
#				comparelvls<-matrix(rep(seq_along(clvls)+1, each=nr), nrow=nr)
#				stretchedcols<-stretchedcols==comparelvls
#				mode(stretchedcols)<-"integer"
#				if(!is.matrix(stretchedcols)){
#					warning("In factorsToDummyVariables.numdfr: stretchedcols was not a matrix?")
#					stretchedcols<-matrix(stretchedcols, nrow=1)}
#				colnames(stretchedcols)<-paste(coln[ci], clvls, sep=betweenColAndLevel)
#				return(stretchedcols)
#			}
#			else
#			{
#				curcol<-mat[,ci, drop=FALSE]
#				return(curcol)
#			}
#		}))
#	rownames(retval)<-rownames(mat)
#	return(retval)
#}


.factorsToDummyVariables_old.numdfr<-function(dfr, betweenColAndLevel = "",...)
{
	.debugtxt()
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















#numdfr.rep
reduce.numdfr<-function(object, orgdfr, repsperrow=NULL, keeponlyusedrows=FALSE, ...)
{
	.debugtxt()
	reduce.data.frame(object, orgdfr, repsperrow, keeponlyusedrows, ...)
}
unreduce.numdfr.rep<-function(object, ...)
{
	.debugtxt()
	repdata<-.getRepData.rep(object)
	orgdata<-.getOrgData.rep(object)
	parts<-lapply(seq_along(repdata), function(i){
			#catw("repdata", i, "/", length(repdata))
			currepdata<-repdata[[i]]
			#catw("->org row was:", currepdata$orgrow)
			reporgrows<-rep(currepdata$orgrow,length(currepdata$names))
			#catw("->so, reporgrows is:", reporgrows)
			rv<-orgdata[reporgrows,]
			if(length(currepdata$nacols) > 0)
			{
				rv[,currepdata$nacols]<-currepdata$repdata
			}
			#catw("->Going to get rownames. Dim of cur rv:", dim(rv))
			#catw("->Names to be used (", length(currepdata$names), "):", currepdata$names)
			rownames(rv)<-currepdata$names
			return(rv)
		})
	combineSimilarDfrList(parts)
}
as.numdfr<-function(object, ...) UseMethod("as.numdfr")
as.numdfr.default<-function(object, ...) stop("as.numdfr not provided for this class of object")
as.numdfr.data.frame<-function(object, ...)
{
	.debugtxt()
	return(numdfr(object))
}
as.numdfr.numdfr<-function(object, ...)
{
	.debugtxt()
	return(object)
}
as.numdfr.numdfr.rep<-function(object, ...)
{
	.debugtxt()
	unreduce.numdfr.rep(object, ...)
}
as.numdfr.data.frame.rep<-function(object, ...)
{
	.debugtxt()
	numdfr(unreduce(object), ...)
}

as.numdfr.rep<-function(object, orgdfr, ...) UseMethod("as.numdfr.rep")
as.numdfr.rep.default<-function(object, orgdfr, ...) stop("as.numdfr.rep not provided for this class of object")
as.numdfr.rep.numdfr<-function(object, orgdfr, ...)
{
	.debugtxt()
	reduce.numdfr(object, orgdfr, ...)
}


.getRepData.rep<-function(x)
{
	return(.subset2(x, "repdata", exact=TRUE))
}
.getOrgData.rep<-function(x, long=FALSE)
{
	orgdata<-.subset2(x, "orgdata", exact=TRUE)
	if(long)
	{
		mp<-.getMap.rep(x)
		orgdata<-orgdata[mp, ]
	}
	return(orgdata)
}
.getKeptOnlyUsedRows.rep<-function(x)
{
	return(.subset2(x, "keptonlyusedrows", exact=TRUE))
}
.getMap.rep<-function(x)
{
	return(.subset2(x, "map", exact=TRUE))
}

.getLevels.rep<-function(x)
{
	dfr<-.getOrgData.rep(x)
	return(.getLevels(dfr))
}

.getOrdered.rep<-function(x)
{
	dfr<-.getOrgData.rep(x)
	return(.getOrdered(dfr))
}

.torepsperrow<-function(vals)
{
	matchespernewrow<-rle(vals)
	repsperrow<-matchespernewrow$lengths
	names(repsperrow)<-matchespernewrow$values
	return(repsperrow)
}

.translateRowIndices<-function(x, i)
{
	#should never be called with missing i
	mappart<-.getMap.rep(x)[i]
	return(.torepsperrow(mappart))
}

originalDataset.numdfr.rep<-function(x) .getOrgData.rep(x)

#for now: very unefficient implementation of these!
"[.numdfr.rep"<-function (x, i, j, returnAsMatrix = drop, drop = FALSE)
{
	.debugtxt()
	orgdata<-.getOrgData.rep(x)
	kept<-.getKeptOnlyUsedRows.rep(x)
	rv<-as.numdfr(x)[i, j, returnAsMatrix, drop]
	#catw("structure of rv so far:")
	#str(rv)
	trri<-.translateRowIndices(x,i)
	#catw("trri:", trri)
	return(reduce(rv, orgdata, trri, kept))
}

"[<-.numdfr.rep"<-function (x, i, j, value) stop("Assignment in numdfr.rep not allowed yet")

length.numdfr.rep<-function(x)
{
	.debugtxt()
	return(ncol(.getOrgData.rep(x)))
}
dimnames.numdfr.rep<-function(x)
{
	.debugtxt()
	dn<-dimnames(.getOrgData.rep(x))
	dn[[1]]<-do.call(c, lapply(.getRepData.rep(x), "[[", "names"))
	return(dn)
}
"dimnames<-.numdfr.rep"<-function(x, value) stop("Assignment in numdfr.rep not allowed yet")
dim.numdfr.rep<-function(x)
{
	.debugtxt()
	c(length(.getMap.rep(x)), length.numdfr.rep(x))
}
names.numdfr.rep<-function(x)
{
	.debugtxt()
	names(.getOrgData.rep(x))
}

"names<-.numdfr.rep"<-function(x, value) stop("Assignment in numdfr.rep not allowed yet")
is.numdfr.rep<-function(x)
{
	.debugtxt()
	inherits(x, "numdfr.rep")
}

as.double.numdfr.rep<-function(x,...)
{
	.debugtxt()
	as.double.numdfr(as.numdfr(x))
}

is.na.numdfr.rep<-function(x)
{
	.debugtxt()
	rv<-is.na.numdfr(.getOrgData.rep(x, long=TRUE))

	rd<-.getRepData.rep(x)
	curspos<-1
	for(i in seq_along(rd))
	{
		currd<-rd[[i]]
		nr<-length(currd$names)
		if(nr > 0)
		{
			curepos<-curspos + nr - 1
			nc<-length(currd$nacols)
			if(nc > 0)
			{
				rv[seq(curspos, curepos),currd$nacols]<-is.na(currd$repdata)
			}
			curspos<-curepos+1
		}
	}
	return(rv)
}

str.numdfr.rep<-function(object,...)
{
	cat("numdfr.rep object with dimensions:", dim(object), "\n")
	cat("->Repetitions of interior rows:\n\t")
	reprle<-rle(.getMap.rep(object))
	repres<-paste(reprle$values, " (", reprle$lengths, ")", sep="")
	cat(repres, "\n", fill=TRUE)
	cat("->Rownames: ", rownames(object), "\n", fill=TRUE)
	cat("->Colnames: ", colnames(object), "\n", fill=TRUE)
	cat("\nThe following variables are factor-like:\n")
	lvls<-.getLevels.rep(object)
	ccns<-findCatColNums.numdfr.rep(object)
	lvltxts<-sapply(ccns, function(ccn){paste(lvls[[ccn]], collapse=" ")})
	ccoltxt<-paste("\t", names(lvls)[ccns], ":", lvltxts)
	cat(ccoltxt, "\n", fill=TRUE)
	invisible()
}

as.list.numdfr.rep<-function(x, returnFactors=TRUE,...)
{
	.debugtxt()
	as.list.numdfr(as.numdfr(x), returnFactors, ...)
}

as.data.frame.numdfr.rep<-function(x, row.names = NULL, optional = FALSE, ...)
{
	.debugtxt()
	as.data.frame.numdfr(as.numdfr(x), row.names, optional, ...)
}

findCatColNums.numdfr.rep<-function(dfr)
{
	.debugtxt()
	which(sapply(.getLevels.rep(dfr), length) > 0)
}

findOrderedColNums.numdfr.rep<-function(dfr)
{
	.debugtxt()
	which(.getOrdered.rep(dfr))
}

#note: assumes (original) rows with the same rowname are also exactly the same!
rbind.numdfr.rep<-function(..., original.data, ensure.unique.rownames=FALSE, separator=".", postfixcol=NULL, allowemptypostfix=TRUE, deparse.level = 1)
{
	#this is the hardest one!!
	.debugtxt()
	allparams<-list(...)
	allrnames<-lapply(allparams, function(curnumdfrrep){
			curorgdata<-.getOrgData.rep(curnumdfrrep)
			rownames(curorgdata)
		})
	#We act like we build a big dataset of all the orgdatas rbound
	rnameposindex<-vapply(allrnames, length, 0)
	#rnameposindex now holds how many original rows there are in each item of ...
	
	rnamesubindex<-do.call(c, lapply(rnameposindex, seq))
	#rnamesubindex now holds the rowindex of each row in the 'big dataset', in the
	#orgdata that it comes from
	
	rnameposindex<-rep(seq_along(rnameposindex), rnameposindex)
	#rnameposindex now holds for each row in the 'big dataset' the number of the
	#item in allparams (or ...) it originates from
	allrnames<-do.call(c, allrnames)
	#allrnames now contains the rownames in the 'big dataset'
	
	#next, we consider a 'reduced dataset' (based on rowname) where every rowname
	#is supposed to occur only once
	
	if(missing(original.data))
	{
		#for now: always 'reduce' the set of original data
		uniqueorgnames<-unique(allrnames)
		rnameindexinbig<-match(uniqueorgnames, allrnames)
		#for each row in the 'reduced dataset', find the first rowindex in the
		#'big dataset' that matches it -> stored in rnameindexinbig
		rnamenewindex<-match(allrnames, uniqueorgnames)
		#for each row in the 'big dataset', find the rowindex in the
		#'reduced dataset' that matches it -> stored in rnamenewindex
		tmpretlst<-lapply(rnameindexinbig, function(riio){
				whichlistitem<-rnameposindex[riio] #the how manieth item in ...
				curorgdata<-.getOrgData.rep(allparams[[whichlistitem]]) #get its orgdata
				whichrowinlistitem<-rnamesubindex[riio] #which row in that item's orgdata does it match
				curorgdata[whichrowinlistitem,] #get that row
			})
		#catw("Got list of original data(s):")
		#print(tmpretlst)
		neworgdata<-combineSimilarDfrList(tmpretlst)
		#catw("Combined them.")
	}
	else
	{
		neworgdata<-original.data
		uniqueorgnames<-rownames(neworgdata)
		rnameindexinbig<-match(uniqueorgnames, allrnames)
		rnamenewindex<-match(allrnames, uniqueorgnames)
	}
	#debugtmp<<-data.frame(allrnames=allrnames, rnameposindex=rnameposindex,
	#	rnamesubindex=rnamesubindex, rnamenewindex=rnamenewindex)
	newrepdata<-do.call(c, lapply(seq_along(allparams), function(i){
			#catw("Getting newrepdata", i, "/", length(allparams))
			curnumdfrrep<-allparams[[i]]
			currepdata<-.getRepData.rep(curnumdfrrep)
			for(j in seq_along(currepdata))
			{
				#catw("->current subitem i:", i, ", j:", j, "\n")
				curorgrow<-currepdata[[j]]$orgrow #what row in its original orgdata does it refer to
				#catw("->curorgrow: ", curorgrow, "\n")
				posofthisrowinindex<-which((rnameposindex==i) & (rnamesubindex==curorgrow))
				#catw("->posofthisrowinindex: ", posofthisrowinindex, "\n")
				if(length(posofthisrowinindex) != 1) stop("Something went terribly wrong!")
				neworgrow<-rnamenewindex[posofthisrowinindex]
				#catw("->Original row index", curorgrow, "got translated to new row index", neworgrow)
				currepdata[[j]]$orgrow<-neworgrow
			}
			return(currepdata)
		}))
	if(missing(original.data))
	{
		newcurkept=FALSE
	}
	else
	{
		#as soon as 1 is keptonlyusedrows, you can never be certain that the total is
		newcurkept<-any(vapply(allparams, .getKeptOnlyUsedRows.rep, TRUE))
	}
	newmap<-do.call(c, lapply(seq_along(allparams), function(i){
			curnumdfrrep<-allparams[[i]]
			curmap<-.getMap.rep(curnumdfrrep)
			currnamesubindex<-rnamesubindex[rnameposindex==i]
			currnamenewindex<-rnamenewindex[rnameposindex==i]
			curpos<-match(curmap, currnamesubindex)
			curmap<-currnamenewindex[curpos]
			return(curmap)
		}))
	rv<-list(repdata=newrepdata, orgdata=neworgdata, keptonlyusedrows=newcurkept,
		map=newmap)
	class(rv)<-"numdfr.rep"
	return(rv)
}

print.numdfr.rep<-function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
    row.names = TRUE)
{
	#note: code mostly ripped from print.data.frame :-)
	#mostly with aim of easily reusing format.data.frame
    n <- length(row.names(x))
    if (length(x) == 0L) {
        cat(gettextf("numdfr.rep with 0 columns and %d rows\n",
            n))
    }
    else if (n == 0L) {
        print.default(names(x), quote = FALSE)
        cat(gettext("<0 rows> (or 0-length row.names)\n"))
    }
    else {
				x2<-as.data.frame.numdfr.rep(x)
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

display.numdfr.rep<-function(dfr)
{
	.debugtxt()
	display(as.data.frame.numdfr.rep(dfr))
}
as.matrix.numdfr.rep<-function(x, ...)
{
	.debugtxt()
	as.matrix.numdfr(as.numdfr(x), ...)
}

allLevels.numdfr.rep<-function(x, onlyNonEmpty=FALSE)
{
	.debugtxt()
	lvls<-.getLevels.rep(x)
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

allOrderedFacts.numdfr.rep<-function(x, onlyNonEmpty=FALSE)
{
	.debugtxt()
	ord<-.getOrdered.rep(x)
	if(! onlyNonEmpty)
	{
		return(ord)
	}
	else
	{
		keep<-sapply(.getLevels.rep(x), length) > 0
		return(ord[keep])
	}
}
"[[.numdfr.rep"<-function(x, ..., exact=TRUE)
{
	.debugtxt()
	thecol<-unlist(as.list(...))
	if(length(thecol) != 1) stop("Unsupported operation: passing more than one parameter to [[.numdfr.rep")
	
	mp<-.getMap.rep(x)
	orgdata<-.getOrgData.rep(x)
	if(is.character(thecol)) colindex<-.findIndexOfColumnName(orgdata, thecol, exact=exact)
	if(is.na(thecol)) stop(paste("Column '", thecol, "' could not be found.", sep=""))
	thecol<-orgdata[[colindex, exact=exact]]

	rd<-.getRepData.rep(x)
	curspos<-1
	for(i in seq_along(rd))
	{
		currd<-rd[[i]]
		nr<-length(currd$names)
		if(nr > 0)
		{
			curepos<-curspos + nr - 1
			nc<-length(currd$nacols)
			if(nc > 0)
			{
				subcoli<-match(colindex, currd$nacols)
				if(!is.na(subcoli))
				{
					thecol[seq(curspos, curepos)]<-currd$repdata[,subcoli, drop=TRUE]
				}
			}
			curspos<-curepos+1
		}
	}

	thelvls<-.getLevels.rep(x)[[colindex]]
	theord<-.getOrdered.rep(x)[[colindex]]
	if(length(thelvls) > 0)
	{
		return(quickFactor(thecol, labels=thelvls, ordered=theord))
	}
	return(thecol)
}

"$.numdfr.rep"<-function(x, name)
{
	.debugtxt()
	return("[[.numdfr.rep"(x, name, exact=TRUE))
}

factorsToDummyVariables.numdfr.rep<-function(dfr, ...)
{
	factorsToDummyVariables(as.numdfr(dfr), ...)
}

as.nummatrix<-function(object) UseMethod("as.nummatrix")
as.nummatrix.numdfr<-function(object) .getMatrix(object)
as.nummatrix.numdfr.rep<-function(object) as.matrix(object)
as.nummatrix.matrix<-function(object) object
as.nummatrix.default<-function(object) as.matrix(object)
as.nummatrix.data.frame<-function(object){
	retval<-matrix(unlist(object), ncol=ncol(object))
	dimnames(retval)<-dimnames(object)
	return(retval)
}

#catCols holds the integer indices of the columns that are really factors
#levelList holds (in order) an item with the levels of each factor column
#colnms holds _all_ the column names
.mat2dfr<-function(mat, catCols, levelList, ord=rep(FALSE, ncol(mat)), colnms=NULL, verbosity=0)
{
	result<-as.data.frame(mat)
	catwif(verbosity>0, "resetting factors")
	for(i in seq_along(catCols)) {
		catwif(verbosity>1, "resetting factor", i, "/", length(catCols))
	  lev <- levelList[[i]]
	  cl<-catCols[i]
	  curfact<-quickFactor(result[[cl]], labels=lev, ordered=ord[i])#really fast
	  result[[cl]] <- curfact #this takes a while -> room for improvement?
#	  cat(ttxt(system.time(curfact<-quickFactor(result[[cl]], labels=lev))), "\n")
#				->typically:user: 0.00, system: 0.00, elapsed: 0.00
#	  cat(ttxt(system.time(result[[cl]] <- curfact)), "\n")
#				->typically:user: 0.16, system: 0.06, elapsed: 0.22 !!!
#note: I tried the alternative of running the quickFactor code immediately, but
#that was even more slow.
	}
	if(!is.null(colnms)) colnames(result)<-colnms
	return(result)
}

matBack2OrgClass<-function(objWithClass, mat, catCols, levelList, ord=rep(FALSE, ncol(mat)), colnms=NULL,
	verbosity=0) UseMethod("matBack2OrgClass")
	
matBack2OrgClass.data.frame<-function(objWithClass, mat, catCols, levelList, ord=rep(FALSE, ncol(mat)),
	colnms=NULL, verbosity=0)
{
	.mat2dfr(mat=mat, catCols=catCols, levelList=levelList, ord=ord, colnms=colnms,
		verbosity=verbosity)
}

matBack2OrgClass.numdfr<-function(objWithClass, mat, catCols, levelList, ord=rep(FALSE, ncol(mat)),
	colnms=NULL, verbosity=0)
{
	posInCatCols<-match(seq(ncol(mat)), catCols, nomatch=0)
	allLevels<-lapply(posInCatCols, function(ccn){
			if(ccn > 0) return(levelList[[ccn]]) else return(character(0))
		})
	if(is.null(colnms)) colnms<-colnames(mat)
	colnames(mat)<-colnms
	names(allLevels)<-colnms
	names(ord)<-colnms
	retval<-list(mat=mat, lvls=allLevels, ord=ord)
	class(retval)<-"numdfr"
	return(retval)
}

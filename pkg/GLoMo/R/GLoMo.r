#rcats will randomly (or if it is feasible, definedly) draw combinations
#of categories for the categorical variables, and note their relevance for
#each of the original rows.
#assumes dfr is wellformed, i.e.: only factors and numeric columns.
#also, all factors should be at the start of the columns (this may not be necessary anymore)

#note: see use of old version in full.glmnet.EM.fit
#note: see similar code in firstProcessingOfDfrForInitialFit.GLoMo

#known issues:
#need to check if there are no categorical columns
#if there are too many categorical columns, a subset of the possible combinations
#   is sampled, but the probabilities returned are the original ones
#a set of categorical values could be in the data.frame twice, if it can be 'attained'
#   from two original rows: note that this will then have different probabilities (!)
#Is it relevant to return the probability based on categoricals alone (probably yes)

#param onlyCategorical: if TRUE, only the categorical columns are returned
#param reweightPerRow: when more than maxFullNACatCols categorical missing vals, the
#   weights are not guaranteed to sum to 1. If this param is TRUE, they are rescaled
#   so they sum to 1 again


#rCatsInDfr<-function(dfr, maxFullNACatCols=6, howManyIfTooMany=1000, onlyCategorical=FALSE,
#	weightsName="weights", orgriName="orgri", reweightPerRow=FALSE, verbosity=0,...) UseMethod("rCatsInDfr")




#catCols holds the integer indices of the columns that are really factors
#levelList holds (in order) an item with the levels of each factor column
#colnms holds _all_ the column names
.mat2dfr<-function(mat, catCols, levelList, colnms=NULL, verbosity=0)
{
	result<-as.data.frame(mat)
	cattif(verbosity>0, ".mat2dfr: resetting factors")
	for(i in seq_along(catCols)) {
		cattif(verbosity>1, "\t.mat2dfr: resetting factor", i, "/", length(catCols))
	  lev <- levelList[[i]]
	  cl<-catCols[i]
	  curfact<-quickFactor(result[[cl]], labels=lev)#really fast
	  result[[cl]] <- curfact #this takes a while -> room for improvement?
#	  cat(ttxt(system.time(curfact<-quickFactor(result[[cl]], labels=lev))), "\n"):typically:user: 0.00, system: 0.00, elapsed: 0.00
#	  cat(ttxt(system.time(result[[cl]] <- curfact)), "\n"):typically:user: 0.16, system: 0.06, elapsed: 0.22 !!!
#note: I tried the alternative of running the quickFactor code immediately, but that was even more slow.
	}
	if(!is.null(colnms)) colnames(result)<-colnms
	return(result)
}


#.findLevels<-function(dfr) {allLevels}
#.findLevels.default<-function(dfr) {return(lapply(dfr, levels))}
#.findLevels.numdfr<-function(dfr) {return(dfr$lvls)}

.quickNumMatrix<-function(dfr) UseMethod(".quickNumMatrix")
#need to check whether this is still any different from as.matrix.data.frame in practice
.quickNumMatrix.data.frame<-function(dfr){
	retval<-matrix(unlist(dfr), ncol=ncol(dfr))
	dimnames(retval)<-dimnames(dfr)
	return(retval)
}
.quickNumMatrix.numdfr<-function(dfr){return(as.matrix(dfr))}

.matBack2OrgClass<-function(objWithClass, mat, catCols, levelList, colnms=NULL, verbosity=0) UseMethod(".matBack2OrgClass")
.matBack2OrgClass.data.frame<-function(objWithClass, mat, catCols, levelList, colnms=NULL, verbosity=0){
	.mat2dfr(mat=mat, catCols=catCols, levelList=levelList, colnms=colnms, verbosity=verbosity)
}
.matBack2OrgClass.numdfr<-function(objWithClass, mat, catCols, levelList, colnms=NULL, verbosity=0){
	posInCatCols<-match(seq(ncol(mat)), catCols, nomatch=0)
	allLevels<-lapply(posInCatCols, function(ccn){
			if(ccn > 0) return(levelList[[ccn]]) else return(character(0))
		})
	colnames(mat)<-colnms
	names(allLevels)<-colnms
	retval<-list(mat=mat, lvls=allLevels)
	class(retval)<-"numdfr"
	return(retval)
}

#optimization: best option is to look at mat2dfr... see comments there.
#   This is only relevant for data.frame...
rCatsInDfr<-function(dfr, maxFullNACatCols=6, howManyIfTooMany=1000, onlyCategorical=FALSE,
	weightsName="weights", orgriName="orgri", reweightPerRow=FALSE, verbosity=0,...)
{
	catCols<-findCatColNums(dfr)
	dfrl<-dfr[,catCols, drop=FALSE]
	if(onlyCategorical)
	{
		dfr<-dfrl
		catCols<-seq(length(catCols))
	}
	orgnames<-colnames(dfr)
	cattif(verbosity>0, "rCatsInDfr: find marginal probabilities")
	probs<-marginalProbPerCat(dfrl)#one item for every categorical column

	toAddCols<-NULL
	if((!is.null(weightsName)) && (nchar(weightsName)>0))
	{
		toAddCols<-c(toAddCols, weightsName)
	}
	if((!is.null(orgriName)) && (nchar(orgriName)>0))
	{
		toAddCols<-c(toAddCols, orgriName)
	}

	naLevels<-allLevels(dfrl)
	dfr<-.quickNumMatrix(dfr)
	cattif(verbosity>0, "rCatsInDfr: dfr is now a matrix of dimension:", dim(dfr), "and class", class(dfr))
	cattif(verbosity>0, "rCatsInDfr: while dfrl now has class", class(dfrl), "and dimension:", dim(dfrl))
	#so, from here on, dfr is a matrix!!! while dfrl only holds the categorical cols
	#   but still has the same class as dfr originally had!
	
	naLevelNums<-lapply(naLevels, function(curlvls){seq(length(curlvls))})
	cattif(verbosity>0, "rCatsInDfr: start producing new rows")
	newrows<-lapply(seq(nrow(dfrl)), function(ri)
	{
		cattif(verbosity>1, "rCatsInDfr: row", ri, "/", nrow(dfrl))
		currow<-dfrl[ri,,drop=TRUE]
		curnas<-which(is.na(currow))
		if(length(curnas) > maxFullNACatCols)
		{
			cattif(verbosity>1, "\trCatsInDfr: too many categoricals missing")
			#if these are 3-level categories, this means already >= 2187 possible combinations
			#in this case, we draw howManyIfTooMany random ones from the marginal multinomials
			stopifnot(howManyIfTooMany > 1)
			if(is.null(weightsName) || (nchar(weightsName)==0))
			{
				cattif(verbosity>1, "\t\trCatsInDfr: no need to calculate weights")
				catvals<-sapply(curnas, function(ci){
						sample.int(length(naLevels[[ci]]), howManyIfTooMany, replace=TRUE, prob=probs[[ci]])
					})
			}
			else
			{
				cattif(verbosity>1, "\t\trCatsInDfr: needed to calculate weights")
				catAll<-lapply(curnas, function(ci){
						rv<-sample.int(length(naLevels[[ci]]), howManyIfTooMany, replace=TRUE, prob=probs[[ci]])
						relvpr<-probs[[ci]][rv]
						return(list(val=rv, pr=relvpr))
					})
				catvals<-sapply(catAll, '[[', "val")
				catMargprobs<-sapply(catAll, '[[', "pr")
				catCombProbs<-unlist(apply(catMargprobs, 1, prod))
				if(reweightPerRow) catCombProbs<-catCombProbs/sum(catCombProbs) #make the weights sum to 1
			}
		}
		else if(length(curnas) > 0)
		{
			cattif(verbosity>1, "\trCatsInDfr: few categoricals missing")
			catvals<-dfr2mat(expand.grid(naLevelNums[curnas]))
			if((!is.null(weightsName)) && (nchar(weightsName)>0))
			{
				cattif(verbosity>1, "\t\trCatsInDfr: needed to calculate weights")
				catMargprobs<-expand.grid(probs[curnas])
				catCombProbs<-apply(catMargprobs, 1, prod)
			}
		}
		if(length(curnas) > 0)
		{
			retval<-dfr[rep(ri, length(catCombProbs)),, drop=FALSE]
			retval[,catCols[curnas]]<-catvals
		}
		else
		{
			cattif(verbosity>1, "\trCatsInDfr: no categoricals missing")
			catCombProbs<-1
			retval<-dfr[ri,, drop=FALSE]
		}
		toAdd<-rep(ri, length(catCombProbs))

		if(((!is.null(weightsName)) && (nchar(weightsName)>0)) || ((!is.null(orgriName)) && (nchar(orgriName)>0)))
		{
			cattif(verbosity>1, "\trCatsInDfr: need to add either weight or orgri")
			if((!is.null(weightsName)) && (nchar(weightsName)>0))
			{
				cattif(verbosity>1, "\trCatsInDfr: need to add weight")
				toAdd<-catCombProbs
				if((!is.null(orgriName)) && (nchar(orgriName)>0))
				{
					cattif(verbosity>1, "\trCatsInDfr: need to add orgri too")
					toAdd<-cbind(toAdd, ri)
				}
			}
			else
			{
				#now we know ((!is.null(orgriName)) && (nchar(orgriName)>0))
				cattif(verbosity>1, "\trCatsInDfr: need to add orgri")
				toAdd<-ri
			}
			retval<-cbind(retval, toAdd)
			colnames(retval)<-c(orgnames, toAddCols)
		}
		return(retval)
	})
	cattif(verbosity>1, "rCatsInDfr: combine the rows")
	resmat<-do.call(rbind, newrows)
	
	cattif(verbosity>1, "rCatsInDfr: turn resulting matrix into ", class(dfrl), " again")
	result<-.matBack2OrgClass(dfrl, mat=resmat, catCols=catCols, levelList=naLevels,
		colnms=c(orgnames, toAddCols), verbosity=verbosity-1)
	return(result)
}

rCatsAndCntInDfr<-function(dfr, maxFullNACatCols=6, howManyIfTooMany=1000,
	weightsName="weights", orgriName="orgri", reweightPerRow=FALSE, verbosity=0,...)
{
	catCols<-findCatColNums(dfr)
	numCols<-ncol(dfr)
	contCols<-(seq(numCols))[-catCols]

	#we expect that the original dfr will be smaller, so we fill the continuous columns first,
	#since they are filled with the mean anyway
	cln<-0
	for(i in contCols)
	{
		cln<-cln+1
		cattif(verbosity>0, "rCatsAndCntInDfr: continuous column (", i, "):", cln, "/", length(contCols))
		curcol<-dfr[,i,drop=TRUE]
		wherenas<-which(is.na(curcol))
		if(length(wherenas) > 0)
		{
			colm<-mean(curcol, na.rm=TRUE)
			dfr[wherenas,i]<-colm
		}
	}

	cattif(verbosity>0, "rCatsAndCntInDfr: categorical columns")
	retval<-rCatsInDfr(dfr=dfr, maxFullNACatCols=maxFullNACatCols,
		howManyIfTooMany=howManyIfTooMany, onlyCategorical=FALSE,
		weightsName=weightsName, orgriName=orgriName, reweightPerRow=reweightPerRow,
		verbosity=verbosity-1,...)
		
	return(retval)
}

if(FALSE)
{
	require(addendum)
	aDfr<-generateTypicalIndependentDfr(100,100,150,catProbs=randomProbabilities, minn=2, maxn=4)
	aDfr.MD<-randomNA(aDfr, 0.05)

	aNDfr.MD<-numdfr(aDfr.MD)
	
	cat("Conversion one way time used: ", ttxt(system.time(  tst<-numdfr(aDfr.MD)   )), "\n")
	#Conversion one way time used:  user: 0.05, system: 0.00, elapsed: 0.04 (hardly worth the mention!)
	
	cat("Full time used: ", ttxt(system.time(  aNDfr.RF<-rCatsInDfr(aNDfr.MD, verbosity=0)   )), "\n")
	#Current implementation:
	#Full time used:  user: 2.58, system: 0.03, elapsed: 2.62 (a lot better than it was!!)
	
	cat("Full time used: ", ttxt(system.time(  aDfr.RF<-rCatsInDfr(aDfr.MD, verbosity=0)   )), "\n")
	#This is my best implementation so far based on data.frame:
	#Full time used:  user: 22.22, system:  2.64, elapsed: 24.98

	cat("just here to avoid long jump ahead\n")
}

.hasNA<-function(dfr){return(any(is.na(dfr)))}

#This is very close to addendum:findCatColNums
.contDataAsMat<-function(dfr){
	catcols<-findCatColNums(dfr)
	return(.quickNumMatrix(dfr[,-catcols]))
}



#Altered this from fitPredictorModel.GLoMo. Turned loop code into matrix operations
#known issue: what if there are no/all factor columns??
#
#IMPORTANT CHANGE: we foresee that most of the times, there will be only one
#observation per cell (i.e.: per mid).
#If this is the case, use the unpooled (co)variance matrix. This will always
#be an overestimate of the true pooled(co)variance matrix! Because this will
#only create more variance in the generated predictors, this should not affect
#the results negatively.
#A possible result is that the algorithm may converge somewhat slower (as more
#'different' data is generated. For relatively few missing values, this should
#not be an issue (especially for few missing continuous values)
#
#Because it is rather hard to check (especially for all covariances) whether
#the above is the case, we will (for now?) always use the global covmat.
# -> arranged by parameter pooledCov
#
#Food for thought: does it still make sense to use the weights then??

GLoMo<-function(dfr, weights=rep(1,dim(dfr)[1]), uniqueIdentifiersPerRow=NULL,
	separator=",", pooledCov=TRUE, verbosity=0,...)
{
	stopifnot(!.hasNA(dfr))
	numCols<-ncol(dfr)
	factorCols<-findCatColNums(dfr)
	numCat<-length(factorCols)
	stopifnot(numCols!=numCat)
	stopifnot(numCat>0)
	dfrDim<-dim(dfr)
	cattif(verbosity > 0, "GLoMo: ", class(dfr), "(", dfrDim, "), factorCols=", factorCols)
	nobs<-dfrDim[1]
	numCont<-dfrDim[2] - numCat

	if(is.null(uniqueIdentifiersPerRow))
	{
		cattif(verbosity > 0, "GLoMo: uniqueIdentifiersPerRow were not provided so recalculating.")
		require(addendum)
		uniqueIdentifiersPerRow<-categoricalUniqueIdentifiers(dfr, separator=separator, na.becomes=NA)
	}
	cattif(verbosity > 0, "GLoMo: uniqueIdentifiersPerRow str: ", str(uniqueIdentifiersPerRow))

	weights<-weights/sum(weights) #make them sum to 1

	cattif(verbosity > 0, "GLoMo: Unique uniqueIdentifiersPerRow.")
	uids<-uniqueCharID(uniqueIdentifiersPerRow, needSort=TRUE, includeOccurrence=FALSE, impNr=1)
	cattif(verbosity > 0, length(uids), "found.")
	firstOccurrenceOfEachUMidInDfr<-match(uids, uniqueIdentifiersPerRow)
	
	uniqueFactorCombinationsAndContinuousMeans<-dfr[firstOccurrenceOfEachUMidInDfr, ]
	
#	factuid<-dfr[firstOccurrenceOfEachUMidInDfr, factorCols] #holds the factor variables for each unique mid
#	colnames(factuid)<-colnames(dfr)[factorCols]

	cattif(verbosity > 0, "GLoMo: Find the pihat + means")
	#2011/07/14: changed to matrix operations!
	#2011/08/10: changed to sparse matrix operations!
	matw<-sparseMatrix(i=match(uniqueIdentifiersPerRow, uids), j=seq_along(uniqueIdentifiersPerRow), x=weights)
	contDataAsMat<-.contDataAsMat(dfr)
	pihat<-rowSums(matw)
	themeans<-as.matrix(matw %*% contDataAsMat)
	
	contCols<-seq(ncol(dfr))[-factorCols]
	contnames<-colnames(dfr)[contCols]
	uniqueFactorCombinationsAndContinuousMeans[,contCols]<-themeans

#	umeans<-cbind(pihat=pihat, themeans)
#	#umeans should now hold 1 row for every unique mid
#	#it should hold the pihats as the first col, and the means for every continuous var
#	colnames(umeans)<-c("pihat", contnames)

	if(! pooledCov) stop("GLoMo for non-pooled covariance matrix has not been implemented yet.")
	if(verbosity > 0) catt("Getting centralized covariances")
	omegahat<-cov.wt(contDataAsMat, wt=weights, method="ML")$cov
	dimnames(omegahat)<-list(rowvar=contnames, colvar=contnames)

	#the conversion to data.frame is perhaps unnecessary and may be a performance
	#hog in the case of numdfr... -> check to improve on this
	#2011/08/24: changed the function so that this conversion no longer takes place.
	#now, uniqueFactorCombinationsAndContinuousMeans has the same class as dfr had.
	retval<-list(uid=uids, pihat=pihat, omegahat=omegahat, orgdatadim=dim(dfr),
		uniqueFactorCombinationsAndContinuousMeans=uniqueFactorCombinationsAndContinuousMeans,
		factorCols=factorCols, guidSeparator=separator)
	class(retval)<-"GLoMo"
	return(retval)
}

if(FALSE)
{
	require(addendum)
	require(NumDfr)
	#aDfr<-generateTypicalIndependentDfr(100,100,150,catProbs=randomProbabilities, minn=2, maxn=4)
	#aDfr.MD<-randomNA(aDfr, 0.05)
	#aNDfr.MD<-numdfr(aDfr.MD)
	
	aDfr.RFC<-rCatsAndCntInDfr(aDfr.MD, verbosity=1)
	colnames(aDfr.RFC)
	.hasNA(aDfr.RFC[,seq(ncol(aDfr.RFC)-2)])
	aNDfr.RFC<-rCatsAndCntInDfr(aNDfr.MD, verbosity=1)
	colnames(aNDfr.RFC)
	.hasNA(aNDfr.RFC[,seq(ncol(aNDfr.RFC)-2)])
}

if(FALSE)
{
	require(addendum)
	require(NumDfr)
	setwd("C:/users/nisabbe/Documents/My Dropbox/Doctoraat/Bayesian Lasso")
	load("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
	source("GLoMo.R")
	save.image("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
}

if(FALSE)
{
	aGLoMo.RF<-GLoMo(aDfr.RFC[,seq(ncol(aDfr.RFC)-2)], weights=aDfr.RFC[,"weights", drop=TRUE],
		verbosity=10) 
	aNDfr.RFC2<-numdfr(aDfr.RFC)
	aGLoMo.RF.N2<-GLoMo(aNDfr.RFC2[,seq(ncol(aNDfr.RFC2)-2), returnAsMatrix = FALSE], weights=aNDfr.RFC2[,"weights", drop=TRUE],
		verbosity=10)

	aGLoMo.RF.N<-GLoMo(aNDfr.RFC[,seq(ncol(aNDfr.RFC)-2), returnAsMatrix = FALSE], weights=aNDfr.RFC[,"weights", drop=TRUE],
		verbosity=10)

}


getGuidData<-function(glomo, dfr, guidPerObservation=NULL)
{
	if(is.null(guidPerObservation))
	{
		require(addendum)
		guidPerObservation<-categoricalUniqueIdentifiers(dfr, separator=glomo$guidSeparator,
			na.becomes="\\d+")
	}
	else
	{
		stopifnot(length(guidPerObservation)==nrow(dfr))
	}
	possibleGlomoGuidPerObs<-lapply(guidPerObservation, function(curfindmid){
		grep(curfindmid, glomo$uid)
	})
	retval<-list(guidPerObservation=guidPerObservation,
		possibleGlomoGuidPerObs=possibleGlomoGuidPerObs,
		separator=glomo$guidSeparator)
	class(retval)<-"GuidData"
	return(retval)
}


#is only called from within predict.GLoMo to handle the unlikely case
#of a row that has no matching row(s) in the GLoMo uids
#Note: this really only is unlikely in the setting that I plan to use it in,
#i.e.: impute in the original data from a GLoMo fit to it.
randomFillAndRepeatDataRow<-function(currow, obsneeded, levelslist, newdata)
{
	fcols<-which(sapply(levelslist, length) > 0)
	curnacols<-which(is.na(currow))
	retval<-currow[rep(1, obsneeded),]
	for(curnacol in curnacols)
	{
		if(curnacol %in% fcols)
		{
			#pick a completely random level
			retval[,curnacol]<-sample.int(length(levelslist[[curnacol]]), size=obsneeded, replace=TRUE)
		}
		else
		{
			#whatever: unweighted mean and sd
			coldata<-newdata[,curnacol, drop=TRUE]
			uwmean<-mean(coldata, na.rm=TRUE)
			uwsd<-sd(coldata, na.rm=TRUE)
			retval[,curnacol]<-rnorm(obsneeded, mean=uwmean, sd=uwsd)
		}
	}
	return(retval)
}

reusableDataForGLoMoSampling<-function(glomo, dfr, forrows=seq(nrow(dfr)), guiddata=NULL, verbosity=0)
{
	if(is.null(guiddata) | (!is(guiddata, "GuidData")))
	{
		cattif(verbosity > 0, "reusableDataForGLoMoSampling: guiddata were not (completely) provided so recalculating.")
		guiddata<-getGuidData(glomo, dfr, guidPerObservation=guiddata)
#		cattif(verbosity > 10, "predict.GLoMo: guiddata str: ")
#		if(verbosity > 10) str(guiddata)
	}
	catcols<-glomo$factorCols
	numCat<-length(catcols)
	cntcols<-seq(ncol(dfr))[-catcols]
	numCont<-length(cntcols)
	perrow<-lapply(forrows, function(currowi){
			#note: improvement possible: if no missing values in a row then these
			#calculations are not needed, just return an empty list
			if((length(forrows) > 1) & (verbosity > 1))
			{
				catt("reusableDataForGLoMoSampling: working on row", match(currowi, forrows), "/", length(forrows))
			}
			currow<-dfr[currowi,]
			glomorowsforcurrow<-guiddata$possibleGlomoGuidPerObs[[currowi]]
			whichCntColNotNA<-which(!is.na(currow[, cntcols, drop=TRUE]))
			whichCntColNA<-(1:numCont)[-whichCntColNotNA]
			presentCntColsInDfr<-cntcols[whichCntColNotNA]
			missingCntColsInDfr<-cntcols[whichCntColNA]
			if(length(whichCntColNotNA) > 0)
			{
				omega<-glomo$omegahat[whichCntColNotNA,whichCntColNotNA, drop=FALSE]

				#note in the 1,2 notation, 1 refers to missing data (NA), while 2 refers to present data (notNA)
				invSig22<-invertSymmetric(omega, careful=FALSE)
				aanwezigeXs<-matrix(currow[, presentCntColsInDfr, drop=TRUE], nrow=1)
				if(length(whichCntColNA) != numCont)
				{
					sig11<-glomo$omegahat[whichCntColNA,whichCntColNA, drop=FALSE]
					sig12<-glomo$omegahat[whichCntColNA,whichCntColNotNA, drop=FALSE]
					a<-currow[,presentCntColsInDfr, drop=TRUE]
					useSigma<-sig11 - sig12 %*% invSig22 %*% t(sig12)
					sigLeft<-sig12 %*% invSig22
				}
				else
				{
					a<-NULL
					useSigma<-glomo$omegahat
					sigLeft<-NULL
				}
			}

			if(length(glomorowsforcurrow) > 1)
			{
				if(length(whichCntColNotNA) == 0)
				{
					#if all continuous values are missing, no need to go conditional on them
					deltas<-glomo$pihat[glomorowsforcurrow]
				}
				else
				{
					deltas<-sapply(glomorowsforcurrow, function(curcatrow){
							pi.c<-glomo$pihat[curcatrow]
							relvMus<-matrix(glomo$uniqueFactorCombinationsAndContinuousMeans[curcatrow,presentCntColsInDfr,drop=TRUE], ncol=1)
							partr<-invSig22 %*% relvMus         #notation refers to near page
							part1<-aanwezigeXs %*% partr        #349 in Analysis of Incomplete
							part2<- -1/2*t(relvMus) %*% partr   #Multivariate Data
							delta.c<-part1+part2+log(pi.c)
							return(delta.c)
						})
					#apparantly, occasionally these deltas are _really_ big, which
					#prevents the exp from working.
					#solution: subtract a common term from all of them (= amounts to dividing
					#   them by a common factor after exponentiation)
					maxd<-max(deltas)
					if(maxd > 100)
					{
						subtr<-maxd-100
						deltas<-deltas - subtr
					}
					deltas<-exp(deltas) #p349 in Analysis of Incomplete Multivariate Data
				}
				deltasum<-sum(deltas)
				probs<-deltas/deltasum
			}
			else
			{
				probs<-NULL
			}

			retval<-list(
				a=a,                   #notation refers to near page 349 in
				useSigma=useSigma,     #Analysis of Incomplete Multivariate Data
				sigLeft=sigLeft,       #
				probs=probs,
				whichCntColNotNA=whichCntColNotNA,
				whichCntColNA=whichCntColNA,
				presentCntColsInDfr=presentCntColsInDfr,
				missingCntColsInDfr=missingCntColsInDfr
			)
			class(retval)<-"ReusableDataForGLoMoSamplingForOneRow"
			return(retval)
		})
	retval<-list(guiddata=guiddata, forrows=forrows, perrow=perrow)
	class(retval)<-"ReusableDataForGLoMoSampling"
	return(retval)
}

#If newdata is NULL, nobs means the number of observations to predict (generate)
#if not, it holds the number of observations to generate per original observation
#that holds missing data
predict.GLoMo<-function(object, nobs=1, newdata=NULL, forrows=seq(nrow(newdata)),
	reusabledata=NULL, returnRepeats=FALSE, returnSelectedGlomoRows=FALSE, verbosity=0,...)
{
	#of interest: the combinations of categorical values in GLoMo are always unique!
	glomo<-object #to make it more recognizable in the following code
	if(is.null(newdata) & (!is.null(reusabledata)))
	{
		warning("You should not provide reusabledata if newdata is NULL. It will be ignored.")
		reusabledata<-NULL #just in case
	}
	cntcols<-seq(ncol(newdata))[-glomo$factorCols]
	if(is.null(newdata))
	{
		cattif(verbosity > 1, "predict.GLoMo: _fully_ predicting dataset.")
		probs<-glomo$pihat
		howofteniseachglomorowsampled<-as.vector(rmultinom(1, nobs, prob=probs))
		glomorowsforcurrow<-which(howofteniseachglomorowsampled > 0)
		howofteniseachglomorowsampled<-howofteniseachglomorowsampled[glomorowsforcurrow]
		glomorowschosen<-rep(glomorowsforcurrow, howofteniseachglomorowsampled)
		retval<-glomo$uniqueFactorCombinationsAndContinuousMeans[glomorowschosen,]
		firstposofeachglomorowinresult<-cumsum(c(1, howofteniseachglomorowsampled))[-(length(howofteniseachglomorowsampled)+1)]
		lastposofeachglomorowinresult<-cumsum(howofteniseachglomorowsampled)
		for( i in seq_along(glomorowsforcurrow))
		{
			cattif(verbosity > 2, "	predict.GLoMo: working on glomorow", i, "/", length(glomorowsforcurrow))
			curglomorowi<-glomorowsforcurrow[i]
			howmanysamplesforcurglomorow<-howofteniseachglomorowsampled[i]
			useMu<-unlist(glomo$uniqueFactorCombinationsAndContinuousMeans[curglomorowi,cntcols, drop=TRUE])
			gen<-qrmvnorm(howmanysamplesforcurglomorow, mean=useMu,sigma=glomo$omegahat)
			allposinresforcurglomorow<-seq(from=firstposofeachglomorowinresult[i], to=lastposofeachglomorowinresult[i])
			retval[allposinresforcurglomorow, cntcols]<-gen
		}
		if(returnSelectedGlomoRows)
		{
			return(list(predicted=retval, glomorowsused=glomorowschosen))
		}
		else
		{
			return(retval)
		}
	}
	#now we also check that the class of the newdata matches the glomo!!
	#if it doesn't, transform it.
	if(any(class(newdata) != class(glomo$uniqueFactorCombinationsAndContinuousMeans)))
	{
		warning("Unmatching classes found between glomo and dfr. Will try to coerce dfr.")
		if(inherits(glomo$uniqueFactorCombinationsAndContinuousMeans, "data.frame"))
		{
			cattif(verbosity > 1, "predict.GLoMo: coercing newdata to data.frame.")
			newdata<-as.data.frame(newdata)
		}
		else if(inherits(glomo$uniqueFactorCombinationsAndContinuousMeans, "numdfr"))
		{
			cattif(verbosity > 1, "predict.GLoMo: coercing newdata to numdfr.")
			newdata<-numdfr(newdata)
		}
	}
	if(length(nobs) != length(forrows))
	{
		if(length(nobs) != 1) stop("Unsupported nobs passed along.")
		cattif(verbosity > 0, "predict.GLoMo: readjusting nobs.")
		nobs<-rep(nobs, length(forrows))
	}
	if(is.null(reusabledata) | (!is(reusabledata, "ReusableDataForGLoMoSampling")))
	{
		cattif(verbosity > 0, "predict.GLoMo: reusabledata were not (completely) provided so recalculating.")
		reusabledata<-reusableDataForGLoMoSampling(glomo=glomo, dfr=newdata, forrows=forrows, guiddata=reusabledata, verbosity=verbosity-1)
	}
	levelslist<-allLevels(newdata)
	predPerRow<-lapply(forrows, function(currowi){
			howManiethRow<-match(currowi, forrows)
			if((length(forrows) > 1) & (verbosity > 1))
			{
				catt("predict.GLoMo: working on row", howManiethRow, "/", length(forrows))
			}
			currow<-newdata[currowi, ]
			indexInReusableData<-match(currowi, reusabledata$forrows)
			curreusabledata<-reusabledata$perrow[[indexInReusableData]]
			curguiddata<-reusabledata$guiddata$possibleGlomoGuidPerObs[[indexInReusableData]]
			if(sum(is.na(currow)) == 0)
			{
				cattif(verbosity > 1, "predict.GLoMo: no data was missing in the original row (", currowi, ").")
				retval<-newdata[currowi, ]
				if(returnSelectedGlomoRows)
				{
					cattif(verbosity > 5, "predict.GLoMo: will use as glomorowsused:", curguiddata)
					return(list(predicted=retval, glomorowsused=curguiddata))#note, in this case, curreusabledata should hold only one value!
				}
				else
				{
					return(retval)
				}
			}
			#which of the rows in the pimeanhat can be chosen is stored in catrowsallowed
			glomorowsforcurrow<-reusabledata$guiddata$possibleGlomoGuidPerObs[[currowi]]
			howmanysamplesforcurrow<-nobs[howManiethRow]
			if(length(glomorowsforcurrow) == 0)
			{
				#in fact this should never happen
				cattif(verbosity > 1, "predict.GLoMo: row has no matching glomorows.")
				warning(paste("predict.GLoMo: Row passed along for which there are no valid predictions. There is no matching combination of categories in the GLoMo object. The rownumber was", currowi, ". Will simply pick random values."))
				retval<-randomFillAndRepeatDataRow(currow=currow, obsneeded=howmanysamplesforcurrow,
					levelslist=levelslist, newdata=newdata)
				if(returnSelectedGlomoRows)
				{
					cattif(verbosity > 5, "predict.GLoMo: will use as glomorowsused:", character(0))
					return(list(predicted=retval, glomorowsused=character(0)))
				}
				else
				{
					return(retval)
				}
			}
			else if(length(glomorowsforcurrow) == 1)
			{
				cattif(verbosity > 1, "predict.GLoMo: row has 1 matching glomorow.")
				howofteniseachglomorowsampled<-howmanysamplesforcurrow
			}
			else
			{
				cattif(verbosity > 1, "predict.GLoMo: row has multiple matching glomorow.")
				probs<-curreusabledata$probs #these probabilities are conditional on the
					#data that is present in currow (categorical AND continuous)
				cattif(verbosity > 5, "predict.GLoMo: their probabilities are: ", probs)
				cattif(verbosity > 5, "predict.GLoMo: and we need: ", howmanysamplesforcurrow, "samples.")
				howofteniseachglomorowsampled<-as.vector(rmultinom(1, howmanysamplesforcurrow, prob=probs))
			}
			#by now, howofteniseachglomorowsampled holds how many times each of the rows indicated by
			#glomorowsforcurrow are selected
			#we remove unselected rows from both:
			glomorowsforcurrow<-glomorowsforcurrow[howofteniseachglomorowsampled>0]
			howofteniseachglomorowsampled<-howofteniseachglomorowsampled[howofteniseachglomorowsampled>0]
			#now we get a vector of all the (possibly repeated) row numbers in pimeanhat
			glomorowschosen<-rep(glomorowsforcurrow, howofteniseachglomorowsampled)
#			cattif(verbosity > 5, "	predict.GLoMo: glomorowschosen:", glomorowschosen)
			#And a starting point for the return values
			retval<-glomo$uniqueFactorCombinationsAndContinuousMeans[glomorowschosen,]
#			cattif(verbosity > 5, "	predict.GLoMo: retval structure for now:")
#			if(verbosity > 5) str(retval)
			#if no continuous data missing, we always use the values in the original row:
			cntcols<-seq(ncol(newdata))[-glomo$factorCols]
#			cattif(verbosity > 5, "	predict.GLoMo: continuous column indexes in the dfr are:", cntcols)
			if(sum(is.na(currow[,cntcols, drop=TRUE])) == 0)
			{
				cattif(verbosity > 1, "	predict.GLoMo: no missing continuous data!")
				orgcont<-currow[rep(1, howmanysamplesforcurrow),cntcols]
#				if(verbosity > 5)
#				{
#					catt("orgcont str:")
#					str(orgcont)
#					catt("retval str:")
#					str(retval)
#					catt("cntcols: ", cntcols)
#				}
				retval[,cntcols]<-orgcont #replace all continuous values with the original ones
#				cattif(verbosity > 1, "	predict.GLoMo: passed no missing continuous data!")
			}
			else
			{
				#We also need to sample continuous values (conditional on the categoricals) if we get here.
				#First, fill out the values that will not change (i.e. that were present in the original row)
				retval[,curreusabledata$presentCntColsInDfr]<-currow[rep(1, howmanysamplesforcurrow),curreusabledata$presentCntColsInDfr]
				firstposofeachglomorowinresult<-cumsum(c(1, howofteniseachglomorowsampled))[seq_along(howofteniseachglomorowsampled)]
				lastposofeachglomorowinresult<-cumsum(howofteniseachglomorowsampled)
				for( i in seq_along(glomorowsforcurrow))
				{
					cattif(verbosity > 2, "	predict.GLoMo: working on glomorow", i, "/", length(glomorowsforcurrow))
					curglomorowi<-glomorowsforcurrow[i]
					howmanysamplesforcurglomorow<-howofteniseachglomorowsampled[i]
					useMu<-unlist(glomo$uniqueFactorCombinationsAndContinuousMeans[curglomorowi,cntcols, drop=TRUE])
					if(length(curreusabledata$whichCntColNA) != length(cntcols))
					{
						#so part of the continuous data is already present (and reused in each sample)
						mu1<-useMu[curreusabledata$whichCntColNA]
						mu2<-useMu[curreusabledata$whichCntColNotNA]
						useMu<-unlist(mu1 + as.vector(curreusabledata$sigLeft %*% matrix(unlist(curreusabledata$a - mu2), ncol=1)))
					}
					if(verbosity > 2) catt("predict.GLoMo: Past parameter calculation. Will now generate conditional normal.")
					gen<-qrmvnorm(howmanysamplesforcurglomorow, mean=useMu,sigma=curreusabledata$useSigma) #normal, each row contains one simulation
					allposinresforcurglomorow<-seq(from=firstposofeachglomorowinresult[i], to=lastposofeachglomorowinresult[i])
					retval[allposinresforcurglomorow, curreusabledata$missingCntColsInDfr]<-gen
				}
			}
			if(returnSelectedGlomoRows)
			{
				cattif(verbosity > 5, "predict.GLoMo: will use as glomorowsused:", glomorowschosen)
				return(list(predicted=retval, glomorowsused=glomorowschosen))
			}
			else
			{
				return(retval)
			}
		})
	#debug.predPerRow<<-predPerRow
	#now recombine the 'subdatasets' in predPerRow, just like it's done at
	#the end of rCatsInDfr
	if(returnSelectedGlomoRows)
	{
		result<-combineSimilarDfrList(lapply(predPerRow, "[[", "predicted")) #this should be OK for numdfr, but probably slow for data.frame
		glomorowsused<-do.call(c, lapply(predPerRow, "[[", "glomorowsused"))
	}
	else
	{
		result<-combineSimilarDfrList(predPerRow) #this should be OK for numdfr, but probably slow for data.frame
	}
	if(!is.null(rownames(predPerRow)))
	{
		rownames(predPerRow)<-postfixToMakeUnique(rownames(predPerRow))
	}
	if(returnRepeats)
	{
		numRepPerRow<-sapply(seq_along(forrows), function(curi){
				currow<-newdata[forrows[curi], ]
				if(sum(is.na(currow)) == 0) return(1) else return(nobs[curi])
			})
		names(numRepPerRow)<-as.character(forrows)
		retval<-list(predicted=result, numRepPerRow=numRepPerRow)
		if(returnSelectedGlomoRows) retval$glomorowsused<-glomorowsused
		return(retval)
	}
	if(returnSelectedGlomoRows)
	{
		return(list(predicted=result, glomorowsused=glomorowsused))
	}
	return(result)
}


#GLoMo:
#uid, pihat, omegahat, orgdatadim, uniqueFactorCombinationsAndContinuousMeans,
#		factorCols, guidSeparator

if(FALSE)
{
#	aReusableNData.1to5<-reusableDataForGLoMoSampling(glomo=aGLoMo.RF.N, dfr=aNDfr.MD, forrows=1:5, guiddata=NULL, verbosity=10)
#	predict(aGLoMo.RF.N, nobs=1, newdata=aNDfr.MD, forrows=1:5, reusabledata=aReusableNData.1to5, verbosity=10)
#	.hasNA(aNDfr.MD[1:5,])
	
	aNDfr.MD.guids<-getGuidData(glomo=aGLoMo.RF.N, dfr=aNDfr.MD, guidPerObservation=NULL)
	a.nsamplesperrow<-sample.int(10, size=nrow(aNDfr.MD), replace=TRUE)
	aNDfr.MD.pred<-predict(aGLoMo.RF.N, nobs=a.nsamplesperrow, newdata=aNDfr.MD,
		reusabledata=aNDfr.MD.guids, returnRepeats=TRUE, returnSelectedGlomoRows=TRUE,
		verbosity=10)
}

if(FALSE)
{
	require(addendum)
	require(NumDfr)
	setwd("C:/users/nisabbe/Documents/My Dropbox/Doctoraat/Bayesian Lasso")
	load("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
	source("GLoMo.R")
	save.image("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
}

#will have to see whether this really provides a speedup
#maybe doing the grep again with the reduced uids will be faster than this
updateGuidData<-function(oldglomo, newglomo, oldrowsused=seq(nrow(oldglomo$uid)), oldguiddata)
{
	oldrowsused<-unique(oldrowsused)
	oldtonew<-sapply(oldglomo$uid[oldrowsused], function(curolduid){
			match(curolduid, newglomo$uid)
		})

	oldguiddata$possibleGlomoGuidPerObs<-lapply(oldguiddata$possibleGlomoGuidPerObs, function(oldrows){
			oldtonew[match(oldrows, oldrowsused)]
		})
	return(oldguiddata)
}

validateFunction.acceptall<-function(attempts, otherData, forrow, verbosity=0)
{
	return(seq(nrow(attempts$predicted)))
}

validateFunction.useprob<-function(attempts, otherData, forrow, verbosity=0)
{
	theprob<-otherData[forrow]
	if(is.null(theprob) || (theprob < 0) || (theprob > 1))
	{
		cattif(verbosity > 0, "invalid probability for row", forrow, "in validateFunction.useprob. Using 50%.")
		theprob<-0.5
	}
	res<-sample.int(2, size=nrow(attempts$predicted), replace=TRUE, prob=c(1-theprob, theprob))
	return(which(res==2))
}

validateFunction.default<-function(attempts, otherData, forrow, verbosity=0)
{
	if(is.null(otherData)) otherData<-rep(0.5, max(forrow))
	return(validateFunction.useprob(attempts, otherData, forrow, verbosity=verbosity))
}

#validateFunction, like the examples above, is a function that must return the
#		indices (rownumbers) of rows that are accepted
predict.conditional.GLoMo<-function(object, nobs=1, dfr, forrow, #only supported for 1 row at a time
	validateFunction=validateFunction.default, guiddata=NULL,
	otherData=NULL, initialSuccessRateGuess=0.5, verbosity=0,...)
{
	glomo<-object #to make it more recognizable in the following code
	cattif(verbosity > 0, "predict.conditional.GLoMo for row", forrow)
	if(! .hasNA(dfr[forrow,]))
	{
		if(is.null(guiddata))
		{
			guiddata<-getGuidData(glomo, dfr[forrow,], guidPerObservation=NULL)
			guidToFind<-guiddata$guidPerObservation[1]
		}
		else
		{
			guidToFind<-guiddata$guidPerObservation[forrow]
		}
		return(list(predicted=dfr[forrow, ], glomorowsused=match(guidToFind, glomo$uid)))
	}
	successes<-0
	attempts<-0
	successRateSoFar<-initialSuccessRateGuess
	tryAtATime<-as.integer((nobs-successes) / successRateSoFar)
	if(tryAtATime < (nobs-successes)) tryAtATime<-(nobs-successes)
	if(tryAtATime < 1) tryAtATime<-1
	reusabledata<-reusableDataForGLoMoSampling(glomo=glomo, dfr=dfr,
		forrows=forrow, guiddata=guiddata, verbosity=verbosity-1)
	acceptedRows<-NULL
	acceptedGLoMoRowsRows<-NULL
	howManyLoops<-0
	while(successes < nobs)
	{
		howManyLoops<-howManyLoops+1
		cattif(verbosity > 0, "predict.conditional.GLoMo start of loop", howManyLoops)
		cattif(verbosity > 5, "predict.conditional.GLoMo will try", tryAtATime, "unconditional predictions")
		cattif(verbosity > 5, "predict.conditional.GLoMo successes:", successes, "/", attempts, "->", successRateSoFar)
		newAttempts<-predict(glomo, nobs=tryAtATime, newdata=dfr, forrows=forrow,
			reusabledata=reusabledata, returnRepeats=FALSE, returnSelectedGlomoRows=TRUE,
			verbosity=verbosity-1)
		newAttemptValidity<-validateFunction(newAttempts, otherData, forrow, verbosity=verbosity-1)
		newlyAccepted<-length(newAttemptValidity)
		if(newlyAccepted > 0)
		{
			if(newlyAccepted > nobs - successes)
			{
				newAttemptValidity<-newAttemptValidity[seq(nobs - successes)]
				newlyAccepted<-nobs - successes
			}
			successes<-successes+newlyAccepted
			if(is.null(acceptedRows))
			{
				acceptedRows<-newAttempts$predicted[newAttemptValidity,]
				acceptedGLoMoRowsRows<-newAttempts$glomorowsused[newAttemptValidity]
			}
			else
			{
				acceptedRows<-rbind(acceptedRows, newAttempts$predicted[newAttemptValidity,])
				acceptedGLoMoRowsRows<-c(acceptedGLoMoRowsRows, newAttempts$glomorowsused[newAttemptValidity])
			}
		}
		#cattif(verbosity > 5, "Before: attempts", attempts, ", tryAtATime", tryAtATime)
		attempts<-attempts+tryAtATime
		#cattif(verbosity > 5, "Next: attempts", attempts, ", successes", successes, ", successRateSoFar", successRateSoFar)
		if(successes == 0) successRateSoFar<-successRateSoFar/2 else successRateSoFar<-successes/attempts
		#cattif(verbosity > 5, "Next: nobs", nobs, ", successes", successes, ", successRateSoFar", successRateSoFar)
		tryAtATime<-as.integer((nobs-successes) / successRateSoFar)
		#cattif(verbosity > 5, "Finally: tryAtATime", tryAtATime)
		if(tryAtATime < (nobs-successes)) tryAtATime<-(nobs-successes)
		if(tryAtATime < 1) tryAtATime<-1
	}
	return(list(predicted=acceptedRows, glomorowsused=acceptedGLoMoRowsRows))
}

predict.conditional.allrows.GLoMo<-function(object, nobs=1, dfr, forrows=seq(nrow(dfr)),
	validateFunction=validateFunction.default, guiddata=NULL,
	otherData=NULL, initialSuccessRateGuess=0.5, verbosity=0,...)
{
	glomo<-object #to make it more recognizable in the following code
	if(length(nobs) != length(forrows))
	{
		if(length(nobs) != 1) stop("Unsupported nobs passed along.")
		cattif(verbosity > 0, "predict.conditional.allrows.GLoMo: readjusting nobs.")
		nobs<-rep(nobs, length(forrows))
	}
	if(is.null(guiddata) | (!is(guiddata, "GuidData")))
	{
		cattif(verbosity > 0, "predict.conditional.allrows.GLoMo: guiddata were not (completely) provided so recalculating.")
		guiddata<-getGuidData(glomo, dfr, guidPerObservation=guiddata)
	}
	predPerRow<-lapply(seq_along(forrows), function(currowi){
			predict.conditional.GLoMo(object=glomo, nobs=nobs[currowi],
				dfr=dfr, forrow=forrows[currowi], validateFunction=validateFunction, 
				guiddata=guiddata,otherData=otherData,
				initialSuccessRateGuess=initialSuccessRateGuess, verbosity=verbosity-1)
		})
	result<-combineSimilarDfrList(lapply(predPerRow, "[[", "predicted")) #this should be OK for numdfr, but probably slow for data.frame
	repsPerRow<-sapply(predPerRow, function(resCurRow){nrow(resCurRow$predicted)})
	glomorowsused<-do.call(c, lapply(predPerRow, "[[", "glomorowsused"))
	return(list(predicted=result, glomorowsused=glomorowsused, repsperrow=repsPerRow))
}

if(FALSE)
{
#	aNDfr.MD.guids<-getGuidData(glomo=aGLoMo.RF.N, dfr=aNDfr.MD, guidPerObservation=NULL)
#	a.nsamplesperrow<-sample.int(10, size=nrow(aNDfr.MD), replace=TRUE)
	aNDfr.MD.predcond<-predict.conditional.allrows.GLoMo(glomo=aGLoMo.RF.N,
		nobs=a.nsamplesperrow, dfr=aNDfr.MD, forrows=seq(nrow(aNDfr.MD)),
		validateFunction=validateFunction.useprob, guiddata=aNDfr.MD.guids,
		otherData=rep(0.5, nrow(aNDfr.MD)), initialSuccessRateGuess=0.7, verbosity=20)
	str(aNDfr.MD.predcond)

#	(aGLoMo.RF.N, nobs=a.nsamplesperrow, newdata=aNDfr.MD,
#		reusabledata=aNDfr.MD.guids, returnRepeats=TRUE, returnSelectedGlomoRows=TRUE,
#		verbosity=10)
	package.skeleton(name="GLoMo", path="C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\R", namespace = TRUE, code_files="GLoMo.r")
}


if(FALSE)
{
	require(addendum)
	require(NumDfr)
	setwd("C:/users/nisabbe/Documents/My Dropbox/Doctoraat/Bayesian Lasso")
	load("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
	source("GLoMo.R")
	save.image("C:\\Users\\nisabbe\\Documents\\My Dropbox\\Doctoraat\\Bayesian Lasso\\GLoMo.RData")
}

#idea to make numdfr even more efficient, especially in terms of memory:
#remember which 'original' row each row refers to, and keep only the 'replacement'
#values !!! This way, when a row is repeated 20 times and only one missing value
#was there, there only needs to be one copy of the repeated values, and 20 different
#'replacement' values!!! Need to check this!
#However: this may render the name 'numdfr' somewhat unsatisfying
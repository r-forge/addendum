\name{addendum-package}
\alias{addendum-package}
\alias{addendum}
\docType{package}
\title{
Set of utility functions to make life easier
}
\description{
Constructed over the course of 3 years, a set of often used and reoccurring utility
functions.
}
\details{
\tabular{ll}{
Package: \tab addendum\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-06-22\cr
License: \tab GNU\cr
LazyLoad: \tab yes\cr
}
\enumerate{
	\item \code{\link{glmnet}} extensions \enumerate{
		\item \code{\link{addCVPlot}}
		\item \code{\link{addLamIndexAxis}}
		\item \code{\link{factorsToDummyVariables}}
		\item \code{\link{originalColumnNamesFromDummyVars}}
		\item \code{\link{makeFactorsAndSetFrontal}}
		\item \code{\link{plot2.cv.glmnet}}
		\item \code{\link{showNonZeroCoef}}
	}
	\item plotting \enumerate{
		\item \code{\link{getColorRange}}
		\item \code{\link{histAndDensity}}
		\item \code{\link{plot4d}}
		\item \code{\link{plotAllScatters}}
		\item \code{\link{plotAndSmooth}}
		\item \code{\link{qqWithPred}}
		\item \code{\link{smartpairs}}
		\item \code{\link{showNumericalHeatMap}}
	}
	\item formulas \enumerate{
		\item \code{\link{basisExpansionFormula}}
		\item \code{\link{modelText}}
		\item \code{\link{asString}}
		\item \code{\link{formulaCols}}
	}
	\item Generalized binary classification/regression \enumerate{
		\item \code{\link{displayPreds}}
		\item \code{\link{doFormulaContCV}}
		\item \code{\link{crossPredictContinuous}}
		\item \code{\link{fitAndPredictContinuous.General}}
		\item \code{\link{createCVResult}}
		\item \code{\link{evaluatePredictions.lms}}
		\item \code{\link{getFormulaCVDescription}}
		\item \code{\link{print.CVRes}}
		\item \code{\link{doFormulaCV}}
		\item \code{\link{crossPredictBinary}}
		\item \code{\link{fitAndPredictBinary.General}}
	}
	\item Multiple testing \enumerate{
		\item \code{\link{checkSig}}
		\item \code{\link{print.checkSig}}
		\item \code{\link{HB}}
	}
	\item Profiling \enumerate{
		\item Text \enumerate{
			\item \code{\link{catif}}
			\item \code{\link{catt}}
			\item \code{\link{cattif}}
			\item \code{\link{printif}}
			\item \code{\link{newLine}}
		}
		\item Memory \enumerate{
			\item \code{\link{getAllCurrentObjectMemoryUsage}}
			\item \code{\link{memorySize}}
			\item \code{\link{showMemoryUsage}}
			\item \code{\link{showMemoryUsageList}}
		}
		\item Progress \enumerate{
			\item \code{\link{simpleDescription}}
			\item \code{\link{showParList}}
			\item \code{\link{stopIfRanTooLong}}
			\item \code{\link{getTimeOut}}
			\item \code{\link{setTimeOut}}
			\item \code{\link{tryRet}}
			\item \code{\link{tshort}}
			\item \code{\link{ttxt}}
		}
		\item Call stack \enumerate{
			\item \code{\link{currentFuncDependencyTree}}
			\item \code{\link{getAllCurrentFuncNames}}
			\item \code{\link{getAllFuncsInDependencyOrder}}
			\item \code{\link{printFuncs}}
			\item \code{\link{printFuncSource}}
			\item \code{\link{listFuncsOfType}}
			\item \code{\link{showCallStackObjects}}
		}
	}
	\item AUC \enumerate{
		\item \code{\link{bootStrapAUC}}
		\item \code{\link{calcAUC.Binary}}
		\item \code{\link{seAUC}}
	}
	\item Data manipulation \enumerate{
		\item \code{\link{combineSimilarDfrList}}
		\item \code{\link{combineSimilarDfrList.prev}}
		\item \code{\link{doSimplify}}
		\item \code{\link{getListItemsAsColumn}}
		\item \code{\link{invertSymmetric}}
		\item \code{\link{qrmvnorm}}
		\item \code{\link{qrmvnormsqrt}}
		\item \code{\link{randomFillDS}}
		\item \code{\link{refactor}}
		\item \code{\link{removeItems}}
		\item \code{\link{removeCols}}
		\item \code{\link{removeUnivariateOutliers}}
		\item \code{\link{repeatDataRow}}
		\item \code{\link{similarSizeGroups}}
	}
	\item Data exploration \enumerate{
		\item \code{\link{colsOfType}}
		\item \code{\link{corWithCat}}
		\item \code{\link{corWithoutCat}}
		\item \code{\link{findParentTree}}
		\item \code{\link{naPos}}
		\item \code{\link{outlierIndices}}
		\item \code{\link{pickBestConservative}}
		\item \code{\link{safeLowess}}
	}
	\item Internal \enumerate{
		\item \code{\link{demoCodeWorkSpace}}
		\item \code{\link{demoMemUsage1}}
		\item \code{\link{demoMemUsage2}}
		\item \code{\link{doSf}}
	}
}
}
\author{
Nick Sabbe

Maintainer: <nick.sabbe@ugent.be>
}
\keyword{ package }
\examples{
aFormStr<-"y~x1+x2"
cat(asString(aFormStr), "\n")
cat(asString(formula(aFormStr)), "\n")
}

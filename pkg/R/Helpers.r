#When you have a list where each item has a sub-item with the
#given name, this function returns alist with all those subitems
getListItemsAsColumn<-function(lst, name)
{
	sapply(lst, function(curli){
			return(curli[[name]])
		})
}

#returns a vector based on fromWhat, without the elements in removeWhat
#verbose TRUE gives feedback on how many elements actually removed
removeItems<-function(removeWhat,fromWhat, verbose=FALSE)
{
	if(verbose)
	{
		cat("Try to remove\n")
		print(removeWhat)
		cat("From\n")
		print(fromWhat)
	}
	found<-unique(do.call(c, lapply(removeWhat, function(w){
			if(is.na(w)){which(is.na(fromWhat))}else{which(fromWhat==w)}
		})))
	if(length(found) > 0)
	{
		if(verbose) cat("Removing", length(found), "items from the data set.\n")
		return(fromWhat[-found])
	}
	else
	{
		return(fromWhat)
	}
}

removeCols<-function(dfr, colsToRemove, verbose=FALSE)
{
	cn<-colnames(dfr)
	ncn<-removeItems(colsToRemove, cn, verbose=verbose)
	dfr[,ncn]
}

#same functionality as in sapply: changes list to vector / matrix if applicable
#answer: list, e.g. return value of lapply, one item for each element of X
#X: vector/list that holds the names (like what you pass to sapply) -> only
#   relevant if USE.NAMES=TRUE
#USE.NAMES: see sapply
doSimplify<-function(answer,X=NULL, USE.NAMES = TRUE)
{
  if (USE.NAMES && (!is.null(X)) && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (length(answer) && length(common.len <- unique(unlist(lapply(answer,
    length)))) == 1L) {
    if (common.len == 1L)
      unlist(answer, recursive = FALSE)
    else if (common.len > 1L)
      array(unlist(answer, recursive = FALSE), dim = c(common.len,
        length(X)), dimnames = if (!(is.null(n1 <- names(answer[[1L]])) &
        is.null(n2 <- names(answer))))
        list(n1, n2))
    else answer
  }
  else answer
}

#Find univariate outliers in the boxplot sense (outside the whiskers, i.e. 1.5
#   times the IQR
outlierIndices<-function(cvar)
{
	bps<-boxplot(cvar, plot=FALSE)$stats[,1]
#	sumVar<-summary(cvar);
#	Q1<-sumVar[2]; Q3<-sumVar[5];
#	IQR<-as.numeric(Q3-Q1);
#	whiskl<-Q1-1.5*IQR
#	whiskr<-Q3+1.5*IQR
#	whiskl<-min(cvar[cvar>=whiskl])
#	whiskr<-max(cvar[cvar<=whiskr])
	list(indices=which((cvar<bps[1])|(cvar>bps[5])), Q1=bps[2], Q3=bps[4],
		IQR=bps[4]-bps[2], whiskl=bps[1], whiskr=bps[5])
}

#Plot a histogram including a density estimation (in red)
histAndDensity<-function(x, ...)
{
  retval<-hist(x, freq=FALSE, ...)
  lines(density(x, na.rm=TRUE), col="red")
  invisible(retval)
}

#Scatterplot + smoothed version(in red)
plotAndSmooth<-function(x,y,plotfunc=plot,...)
{
  plotfunc(x,y,...)
  lines(safeLowess(x,y),col="red")
  invisible()
}

#lowess wrapper that handles missing values etc. (by not including them)
safeLowess<-function(x,y,...)
{
  invalid<-is.na(x) | is.nan(x) | is.infinite(x) | is.na(y) | is.nan(y) | is.infinite(y)
  if(sum(invalid)>0)
  {
    lowess(x[!invalid],y[!invalid],...)
  }
  else
  {
    lowess(x,y,...)
  }
}

#QQ-plot including qqline (in red)
qqWithPred<-function(x,...)
{
  qqnorm(x, ...)
  qqline(x, col="red")
}

#Given a number of plots to display at the same time, find a layout that fits
#that many and is more or less square
squareLikeLayoutForNGraphs<-function(n, useLayout=TRUE, traceRes=FALSE)
{
  side<-ceiling(sqrt(n))
  oside<-ceiling(n / side)
  #c(side, oside)
  if(traceRes)
	{
		cat("For", n, ":", side, "x", oside,"\n")
	}
  if(useLayout)
  {
	  rest<-side*oside-n
	  cont<-c(1:n, rep(0,rest))
	  m<-matrix(cont, nrow=oside, byrow=TRUE)
	  layout(m)
  }
  else
  {
  	par(mfrow=c(side, oside))
  }
  #layout.show(n)

  c(side, oside)
}

#Plot all the relations between any column in dfr to the given column
#comparingColName. It is possible the indicate some particular column relations
# (markCols), or only showa subsetof the column relations (onlyMarkCols).
#Finally, you can make a set of observations, defined by a vector of rowindices, 
#be indicated in a different color (e.g.: outlying observations)
#Decides which type of graph is most appropriate.
plotAllScatters<-function(comparingColName, dfr, ylab, markCols=c(),
	onlyMarkCols=FALSE, indicateObs=c(), excludeOrg=FALSE,
	avoidUnivariateOutliers=FALSE)
{
	markColor<-"green"
	indicateColor<-"orange"
	stdColor<-"black"
	extraLineColor<-"red"

	if(avoidUnivariateOutliers)
	{
		dfr<-removeUnivariateOutliers(dfr, verbosity=0)
	}

  comparingCol<-dfr[,comparingColName]
  useCols<-colnames(dfr)
  if(onlyMarkCols)
  {
    useCols<-markCols
    markCols<-c()
  }
  useCols<-unique(useCols)
  if(excludeOrg)
  {
		if(comparingColName %in% useCols)
		{
			useCols<-useCols[-match(comparingColName, useCols)]
		}
  }
  n<-length(useCols)
  squareLikeLayoutForNGraphs(n)
  for(colnm in useCols)
  {
    curcol<-dfr[,colnm]
    par(mar=c(4,1,1,1))
    txtcol<-stdColor
    if(is.element(colnm, markCols))
    {
      txtcol<-markColor
    }
    if(colnm==comparingColName)
    {
      if(is.factor(curcol))
      {
        barplot(table(curcol), main = "", xlab=colnm)
      }
      else
      {
        hist(curcol, freq=FALSE, main = "", xlab=colnm)
        lines(density(curcol, na.rm=TRUE), col=extraLineColor)
      }
    }
    else
    {
      if(is.factor(curcol))
      {
        if(is.factor(comparingCol))
        {
          ttt<-table(data.frame(curcol,comparingCol))
          tt<-data.frame(ttt)
          colnames(tt)<-c(colnm, comparingColName, "Freq")
          #print(names(tt))
          tt[,colnm]<-as.factor(tt[,colnm])
          tt[,comparingColName]<-as.factor(tt[,comparingColName])

          ppin <- par("pin")
          xsize<-ppin[1] / (length(levels(curcol)) + 1)
          ysize<-ppin[2] / (length(levels(comparingCol)) + 1)
          sunflowerplot(tt[,colnm],tt[,comparingColName], number=tt$Freq,
            xlim = c(0.5,length(levels(curcol)) + 0.5),
            ylim = c(0.5,length(levels(comparingCol)) + 0.5),
            xlab = colnm, size=min(xsize,ysize)/2, main="")
        }
        else
        {
          boxplot(comparingCol~curcol, xlab=colnm, ylab=ylab, col.lab=txtcol,
						main="")
          if(length(indicateObs) > 0)
          {
            boxplot(comparingCol[indicateObs]~curcol[indicateObs], xlab="",
							ylab="", add=TRUE, col=indicateColor, main="")
          }
        }
      }
      else
      {
        if(is.factor(comparingCol))
        {
          boxplot(curcol~comparingCol, xlab=colnm, ylab=ylab, col.lab=txtcol,
						main="",horizontal=TRUE)
          if(length(indicateObs) > 0)
          {
            boxplot(curcol[indicateObs]~comparingCol[indicateObs], xlab="",
							ylab="", add=TRUE, col=indicateColor, main="",horizontal=TRUE)
          }
        }
        else
        {
          plot(curcol, comparingCol, xlab=colnm, ylab=ylab, col.lab=txtcol,
						main="")
          validDots<-(!is.infinite(curcol)) & (!is.na(curcol)) &
						(!is.nan(curcol)) & (!is.infinite(comparingCol)) &
						(!is.na(comparingCol)) & (!is.nan(comparingCol))
          scuc<-curcol[validDots]
          scoc<-comparingCol[validDots]
          lines(lowess(scuc, scoc),col="red")
          if(length(indicateObs) > 0)
          {
            points(curcol[indicateObs], comparingCol[indicateObs], xlab="",
							ylab="", col=indicateColor)
          }
        }
      }
    }
  }
  invisible(layout(1))
}

#Sunflowerplot for use in smartplot thus in smartpairs
#Note: slightly different versions of this function available, fitting different
#contexts
plotTwoCats<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = FALSE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, ...)
{
  ttt<-table(data.frame(x,y))
  tt<-data.frame(ttt)
  #print(names(tt))
  tt$x<-as.factor(tt$x)
  tt$y<-as.factor(tt$y)

  ppin <- par("pin")
  xsize<-ppin[1] / (length(levels(x)) + 1)
  ysize<-ppin[2] / (length(levels(y)) + 1)
  sunflowerplot(tt$x,tt$y, number=tt$Freq,
    xlim = c(0.5,length(levels(x)) + 0.5),
    ylim = c(0.5,length(levels(y)) + 0.5),
    log = log, main = main, sub = sub,
    xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot,
    panel.first = panel.first, panel.last = panel.last, asp = asp,
    size=min(xsize,ysize)/2,
    ...)

  invisible()
}

#new version that doesn't use sunflower plots but more easily interpreted one:
#in this version, color indicates membership
#TO DO: use cex or similar to size the text according to the room available...
plotTwoCats2<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = FALSE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, ...)
{
  ttt<-table(data.frame(x,y))
  tt<-data.frame(ttt)
  #print(names(tt))
  tt$x<-as.factor(tt$x)
  tt$y<-as.factor(tt$y)
  usr<-par("usr"); on.exit(par(usr))

	minFreq<-min(tt$Freq)
	maxFreq<-max(tt$Freq)
	numX<-length(levels(tt$x))
	numY<-length(levels(tt$y))
	#par(usr=c(0, numX, 0, numY))
  ppin <- par("pin")
  xsize<-ppin[1] / (length(levels(x)) + 1)
  ysize<-ppin[2] / (length(levels(y)) + 1)
	plot(as.numeric(x),as.numeric(y),
		xlim=c(0.5,length(levels(x)) + 0.5),
		ylim=c(0.5,length(levels(y)) + 0.5),
    log = log, main = main, sub = sub,
    xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot,
    panel.first = panel.first, panel.last = panel.last, asp = asp, ...)


	sapply(seq(numX), function(xi){
			sapply(seq(numY), function(yi){
					frq<-tt$Freq[(as.numeric(tt$x)==xi) & (as.numeric(tt$y)==yi)]
					freqpct<-(frq-minFreq)/(maxFreq-minFreq)
					clr<-rgb(freqpct, 1-freqpct, 0, 1)
					rect(-0.5+xi, -0.5+yi, 0.5+xi, 0.5+yi, col=clr)
					text(xi, yi, labels=frq)
					invisible()
				})
			invisible()
		})
  invisible()
}

#Boxplot for use in smartplot thus in smartpairs
#Note: slightly different versions of this function available, fitting different
#contexts
plotBox<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, horizontal=FALSE, ...)
{
  boxplot(y~x,names=rep("", length(levels(x))), xlim = xlim, ylim = ylim,
		log = log, main = main, sub = sub,
    xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot,
    panel.first = panel.first, panel.last = panel.last, asp = asp, notch=FALSE,
    show.names=FALSE, horizontal=horizontal, ...)
}

#For use in smartpairs: decide which scatter-like plot is most appropriate for 
#two variables, and plot this one.
#Diagonal (see smartpairs): barplot (categorical) / histogram (continuous)
#Rest:
#   * 2 categoricals: sunflowerplot
#   * 1 categorical: grouped boxplots
#   * 2 continuous: scatter
#Note: slightly different versions of this function available, fitting different
#contexts
smartplot<-function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL,
    panel.last = NULL, asp = NA, diagon=FALSE, twoCatPlot=plotTwoCats,
		verbosity=0, ...)
{
  if(diagon)
  {
    #assume we are on the diagonal...
    #may be nice to plot a histogram then?
    if(is.factor(x))
    {
    	cattif(verbosity > 0, "smartplot, diagonal, factor")
      barplot(table(x), main = main, sub = sub,
        xlab = xlab, ylab = ylab, ann = ann, axes = axes, axisnames=FALSE,
        panel.first = panel.first, panel.last = panel.last, asp = asp, ...)
    }
    else
    {
    	cattif(verbosity > 0, "smartplot, diagonal, non-factor")
      hist(x, freq=FALSE, main = main, sub = sub,
        xlab = xlab, ylab = ylab, ann = ann, axes = axes,
        panel.first = panel.first, panel.last = panel.last, asp = asp, ...)
      lines(density(x, na.rm=TRUE), col="red")
    }
  }
  else
  {
    if(is.factor(x))
    {
      if(is.factor(y))
      {
				cattif(verbosity > 0, "smartplot, off diagonal, 2 factors")
        twoCatPlot(x, y, type = type, xlim = xlim, ylim = ylim, log = log,
					main = main, sub = sub, xlab = xlab, ylab = ylab, ann = ann,
					axes = axes, frame.plot = frame.plot, panel.first = panel.first,
					panel.last = panel.last, asp = asp, ...)
      }
      else
      {
				cattif(verbosity > 0, "smartplot, off diagonal, 1 factor (x)")
        plotBox(x,y, xlim = xlim, ylim = ylim, log = log, main = main,
					sub = sub, xlab = xlab, ylab = ylab, ann = ann, axes = axes,
					frame.plot = frame.plot, panel.first = panel.first,
					panel.last = panel.last, asp = asp, ...)
      }
    }
    else if(is.factor(y))
    {
			cattif(verbosity > 0, "smartplot, off diagonal, 1 factor (y)")
      plotBox(y,x, horizontal=TRUE, xlim = xlim, ylim = ylim, log = log,
				main = main, sub = sub, xlab = xlab, ylab = ylab, ann = ann,
				axes = axes, frame.plot = frame.plot, panel.first = panel.first,
				panel.last = panel.last, asp = asp, ...)
    }
    else
    {
			cattif(verbosity > 0, "smartplot, off diagonal, numeric")
      x<-as.numeric(x)
      y<-as.numeric(y)
      plot(x,y, type = type, xlim = xlim, ylim = ylim, log = log, main = main,
				sub = sub, xlab = xlab, ylab = ylab, ann = ann, axes = axes,
				frame.plot = frame.plot, panel.first = panel.first,
				panel.last = panel.last, asp = asp, ...)
      lines(safeLowess(x,y),col="red", xlim = xlim, ylim = ylim,
        panel.first = panel.first, panel.last = panel.last, asp = asp, ...)
    }
  }
  invisible()
}

#For use in smartpairs: draw the right kind of axis at the side
smartAxis<-function(x = NULL, at = NULL, ..., side, labels = NULL,
	otherIsFactor = FALSE, last = FALSE)
{
  if(is.factor(x))
  {
    Axis(x=(1:length(levels(x))), at=(1:length(levels(x))), ..., side=side,
			labels = as.character(levels(x)))
  }
  else
  {
    Axis(x=x, at=at, ..., side=side, labels = labels)
  }
}

removeUnivariateOutliers<-function(x, setNA=TRUE, verbosity=0)
{
	nc<-dim(x)[2]
	indices<-lapply(seq(nc), function(i){
		if(sum(is.numeric(x[,i])) > 0)
		{
			rv<-outlierIndices(x[,i])$indices
			if(verbosity > 0)
			{
				if(length(rv) > 0)
				{
					cat("The following observations are outliers for column", i, ":\n")
					print(rv)
				}
			}
			rv
		}
		else
		{
			if(verbosity > 0) cat("Non numeric column", i, "ignored.\n")
			NULL
		}
	})
	if(setNA)
	{
		hasOut<-which(sapply(indices, length)>0)
		for(i in hasOut)
		{
			if(verbosity > 0) cat("Setting NAs for column", i, "\n")
			rws<-indices[[i]]
			x[rws, i]<-NA
		}
	}
	else
	{
		outlierRows<-unique(do.call(c, indices))
		if(length(outlierRows) > 0)
		{
			x<-x[-outlierRows,]
		}
	}
	return(x)
}

#Draws a huge matrix showing the one-to-one relatioships between variables in
# a dataframe.
#This is very similar to pairs, but uses smarter graphs for some cases (factors)
smartpairs<-function (truex, labels, panel = points, ..., lower.panel = panel,
    upper.panel = panel, diag.panel = NULL, text.panel = textPanel,
    label.pos = 0.5 + has.diag/3, cex.labels = NULL, font.labels = 1,
    row1attop = TRUE, gap = 1, localPlot=defaultLocalPlot,
		localAxis=defaultLocalAxis,	twoCatPlot=plotTwoCats2, verbosity=0,
		avoidUnivariateOutliers=FALSE)
{
		if(avoidUnivariateOutliers)
		{
			truex<-removeUnivariateOutliers(truex, verbosity=verbosity)
		}
    x<-truex
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x,
        y, txt, cex = cex, font = font)
    defaultLocalAxis <- function(side, x, y, xpd, bg, col = NULL, main,
      oma, ...) {
        if (side%%2 == 1)
        {
            smartAxis(x, side = side, xpd = NA, otherIsFactor=is.factor(y), ...)
				}
        else
				{
						smartAxis(y, side = side, xpd = NA, otherIsFactor=is.factor(x), ...)
				}
    }
    defaultLocalPlot <- function(..., main, oma, font.main, cex.main){
			smartplot(..., twoCatPlot=twoCatPlot, verbosity=verbosity-1)}
    localLowerPanel <- function(..., main, oma, font.main, cex.main){
			lower.panel(...)}
    localUpperPanel <- function(..., main, oma, font.main, cex.main){
			upper.panel(...)}
    localDiagPanel <- function(..., main, oma, font.main, cex.main){
			diag.panel(...)}
    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for (i in seq_along(names(x))) {
            if (is.factor(x[[i]]) || is.logical(x[[i]]))
                x[[i]] <- as.numeric(x[[i]])
            if (!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    }
    else if (!is.numeric(x))
        stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel))
        diag.panel <- match.fun(diag.panel)
    if (row1attop) {
        tmp <- lower.panel
        lower.panel <- upper.panel
        upper.panel <- tmp
        tmp <- has.lower
        has.lower <- has.upper
        has.upper <- tmp
    }
    nc <- ncol(x)
    if (nc < 2)
        stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels))
            labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels))
        has.labs <- FALSE
    oma <- if ("oma" %in% nmdots)
        dots$oma
    else NULL
    main <- if ("main" %in% nmdots)
        dots$main
    else NULL
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main))
            oma[3L] <- 6
    }
    opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))
    for (i in if (row1attop)
        1L:nc
    else nc:1) for (j in 1L:nc) {
				cattif(verbosity > 0, "smartPairs plotting (", i, ", ", j, ")")
				if(length(colnames(truex)) > 0)
				{
					cattif(verbosity > 0, "i.e. (", colnames(truex)[i], ", ",
						colnames(truex)[j], ")")
				}
        dia<-ifelse(i==j,TRUE,FALSE)
        localPlot(truex[, j], truex[, i], xlab = "", ylab = "", axes = FALSE,
            type = "n",diagon=dia, ...)
        if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
            box()
            firstvec<-truex[, j]
            secondvec<-truex[, i]
            if((i == nc) && (j == nc) && (nc%%2))
            {
            }
            if (i == 1 && (!(j%%2) || !has.upper || !has.lower))
                localAxis(1 + 2 * row1attop, firstvec, secondvec,
                  ...)
            if (i == nc && (j%%2 || !has.upper || !has.lower))
            {
              if(j == nc )
              {
                localAxis(3 - 2 * row1attop, firstvec, secondvec,last=TRUE,...)
              }
              else
              {
                localAxis(3 - 2 * row1attop, firstvec, secondvec,...)
              }
            }
            if (j == 1 && (!(i%%2) || !has.upper || !has.lower))
                localAxis(2, firstvec, secondvec, ...)
            if (j == nc && (i%%2 || !has.upper || !has.lower))
            {
              if(i == nc )
              {
                localAxis(4, firstvec, secondvec,last=TRUE, ...)
              }
              else
              {
                localAxis(4, firstvec, secondvec, ...)
              }
            }
            mfg <- par("mfg")
            if (i == j) {
                if (has.diag)
                  localDiagPanel(as.vector(x[, i]), ...)
                if (has.labs) {
                  par(usr = c(0, 1, 0, 1))
                  if (is.null(cex.labels)) {
                    l.wid <- strwidth(labels, "user")
                    cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
                  }
                  text.panel(0.5, label.pos, labels[i], cex = cex.labels,
                    font = font.labels)
                }
            }
            else if((!is.factor(truex[[i]])) && (!is.factor(truex[[j]])))
            {
              if (i < j)
                  localLowerPanel(as.vector(x[, j]), as.vector(x[,
                    i]), ...)
              else localUpperPanel(as.vector(x[, j]), as.vector(x[,
                  i]), ...)
            }
            if (any(par("mfg") != mfg))
                stop("the 'panel' function made a new plot")
        }
        else par(new = FALSE)
    }
    if (!is.null(main)) {
        font.main <- if ("font.main" %in% nmdots)
            dots$font.main
        else par("font.main")
        cex.main <- if ("cex.main" %in% nmdots)
            dots$cex.main
        else par("cex.main")
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}

#Tries to execute the given expression.
#If it fails, return an object of class "try-error", holding the error message
#expr: the expression to be executed
#silent: if TRUE, the error message is not immediately displayed (only returned,
#		unless errRet is not NULL)
#errRet: if not NULL, this is returned when an error occurred. Unless silent was
#   FALSE (and the error was thus output), there is no way to recover 
#   information on what error occurred anymore...
tryRet<-function(expr, silent = FALSE, errRet = NULL)
{
  tryCatch(expr, error = function(e) {
    call <- conditionCall(e)
    if (!is.null(call)) {
      if (identical(call[[1L]], quote(doTryCatch)))
        call <- sys.call(-4L)
      dcall <- deparse(call)[1L]
      prefix <- paste("Error in", dcall, ": ")
      LONG <- 75L
      msg <- conditionMessage(e)
      sm <- strsplit(msg, "\n")[[1L]]
      w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L],
        type = "w")
      if (is.na(w))
        w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L],
          type = "b")
      if (w > LONG)
        prefix <- paste(prefix, "\n  ", sep = "")
    }
    else prefix <- "Error : "
    msg <- paste(prefix, conditionMessage(e), "\n", sep = "")
    .Internal(seterrmessage(msg[1L]))
    if (!silent && identical(getOption("show.error.messages"),
      TRUE)) {
      cat(msg, file = stderr())
      .Internal(printDeferredWarnings())
    }
    if(is.null(errRet))
    {
      invisible(structure(msg, class = "try-error"))
    }
    else
    {
      errRet
    }
  })
}

#Given an outcome column name (outCol) and a set of predictor column names
#		(predCols), generate a model formula text
modelText<-function(outCol, predCols)
{
	if(length(predCols)>0)
	{
		predfrm<-paste(predCols, collapse=" + ")
	}
	else
	{
		predfrm<-"1"
	}
  paste(outCol, predfrm, sep=" ~ ")
}

#char version of formula for proper display
asString<-function(form)
{
  if(is.character(form))
  {
    return(form)
  }
  else
  {
    parts<-as.character(form)
    return(modelText(parts[2], parts[-c(1,2)]))
  }
}

#get the predictor column names from a formula
formulaCols<-function(form)
{
  parts<-as.character(as.formula(form))
  list(outcol=parts[2], predcols=parts[-c(1,2)])
}

#sped up version of rmvnorm, using a choleski decomposition of the covar matrix
#in quite a few cases we already know the chol decomp of sigma!
qrmvnormsqrt<-function (n, mean, cholsigma) 
{
	#see qrmvnorm for more info
	#This version assumes a choleski decomposed version of the covariance
	#matrix is already passed.
	#Note: pivot=TRUE is supposed!!!
	if(is.null(dim(mean)))
	{
		if(length(mean)==1)
		{
			#assume sigma is also simply a number then
			return(matrix(rnorm(n, mean=mean, sd=cholsigma), ncol=1))
		}
	}
	retval <- cholsigma
	o <- order(attr(retval, "pivot"))
	retval <- retval[, o]
  retval <- matrix(rnorm(n * ncol(cholsigma)), nrow = n) %*% retval
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}

#sped up version of rmvnorm, leaving out all sanity checks
qrmvnorm<-function (n, mean, sigma)
{
	#note: code copied from rmvnorm, + adapted : faster/more fit for my case
	#Change 1: if only 1 mean, simply use rnorm
	if(is.null(dim(mean)))
	{
		if(length(mean)==1)
		{
			#assume sigma is also simply a number then
			return(matrix(rnorm(n, mean=mean, sd=sqrt(sigma)), ncol=1))
		}
	}
	#change 2: leave out checks
	#Change 3: always use chol
	retval <- chol(sigma, pivot = TRUE)
	o <- order(attr(retval, "pivot"))
	retval <- retval[, o]

  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n) %*% retval
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}

#Typical use: for glmnet. Convert a dataframe to a matrix, where factor
#   columns are split into dummy variables (first level = reference)
#betweenColAndLevel: in the name of the dummy columns, what comes between the
#   original column name and the column level
factorsToDummyVariables<-function(dfr, betweenColAndLevel="")
{
	#note this version seems a lot faster than
	#dfrTmp<-model.frame(dfrPredictors, na.action=na.pass)
	#return(as.matrix(model.matrix(as.formula(form), data=dfrTmp))[,-1])
	nc<-dim(dfr)[2]
	firstRow<-dfr[1,]
	coln<-colnames(dfr)
	retval<-do.call(cbind, lapply(seq(nc), function(ci){
			if(is.factor(firstRow[,ci]))
			{
				lvls<-levels(firstRow[,ci])[-1]
				stretchedcols<-sapply(lvls, function(lvl){
						rv<-dfr[,ci]==lvl
						mode(rv)<-"integer"
						return(rv)
					})
				if(!is.matrix(stretchedcols)){
					stretchedcols<-matrix(stretchedcols, nrow=1)}
				colnames(stretchedcols)<-paste(coln[ci], lvls, sep=betweenColAndLevel)
				return(stretchedcols)
			}
			else
			{
				curcol<-matrix(dfr[,ci], ncol=1)
				colnames(curcol)<-coln[ci]
				return(curcol)
			}
		}))
	rownames(retval)<-rownames(dfr)
	return(retval)
}

#given a set of dummy column names (see factorsToDummyVariables), try to find
#   the column from which it originates
#dfr: original dfr (or at least one with the original column names)
#betweenColAndLevel: in the name of the dummy columns, what comes between the
#   original column name and the column level
originalColumnNamesFromDummyVars<-function(dummyVarNames, dfr,
	betweenColAndLevel="")
{
	nc<-dim(dfr)[2]
	firstRow<-dfr[1,]
	coln<-colnames(dfr)
	mapping<-do.call(rbind, lapply(seq(nc), function(ci){
			if(is.factor(firstRow[,ci]))
			{
				lvls<-levels(firstRow[,ci])[-1]
				return(data.frame(org=coln[ci], new=paste(coln[ci], lvls,
					sep=betweenColAndLevel)))
			}
			else
			{
				return(data.frame(org=coln[ci], new=coln[ci]))
			}
		}))
#	#mapping$new==dummyVarNames
#	#print(mapping)
#	#note: we are supposing there are no two 'original' column names where
#	#	one is the start of the other.
#	mapping<-sapply(colnames(dfr), function(cn)
#		{
#			dumVN<-substr(dummyVarNames, 1, nchar(cn))
#			matches<-which(dumVN==cn)
#			if(length(matches)>0)
#			{
#				retval<-data.frame(orgname=rep(cn, length(matches)),
#					dummyname=dummyVarNames[matches])
#			}
#			else
#			{
#				retval<-NULL
#			}
#			retval
#		})
#	mapping<-do.call(rbind, mapping)
#	#print(mapping)

	unfoundDummies<-setdiff(dummyVarNames, mapping$new)
	if(length(unfoundDummies) > 0)
	{
		mapping<-rbind(mapping, data.frame(org=rep(NA, length(unfoundDummies)),
			new=unfoundDummies))
	}
	colnames(mapping)<-c("orgname", "dummyname") #backwards compatibility
	mapping$orgname<-as.character(mapping$orgname)
	mapping$dummyname<-as.character(mapping$dummyname)
	mapping$rest<-substr(mapping$dummyname, nchar(mapping$orgname)+1+nchar(betweenColAndLevel),
		nchar(mapping$dummyname))

	mapping
}

#Given a dataframe row (dr), return a dataframe holding nrOfTimes copies
#   of that row
repeatDataRow<-function(dr, nrOfTimes)
{
	dr[rep(1, nrOfTimes),]
}

#displays heat map for a matrix (R).
#ncolors: in how many pieces is the range of R values subdivided
#showLegend: if TRUE, adds a legend mapping colors to R values
#roundLegend: if the legend is shown, round R values up to so many digits
showNumericalHeatMap<-function(R, ncolors=2, showLegend=TRUE, roundLegend=2)
{
	if(showLegend)
	{
		layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4,1))
	}
	rg<-range(R)
	mn<-rg[1]-((rg[2] - rg[1])/10)
	mx<-rg[2]+((rg[2] - rg[1])/10)
	brks<-seq(mn, mx, length.out = ncolors+1)
	cat("Numerical heat map range:", mn, "-", mx, "gives breaks:", "\n")
	print(brks)
	n<-dim(R)[1]
	#Notice that image interprets the z matrix as a table of f(x[i], y[j]) values,
	#so that the x axis corresponds to row number and the y axis to column number,
	#with column 1 at the bottom, i.e. a 90 degree counter-clockwise rotation of
	#the conventional printed layout of a matrix.
	rv<-image(1:n, 1:n, t(R)[1:n,n:1], col = heat.colors(ncolors), breaks=brks,
		xlab=paste("1-", n, sep=""), ylab=paste("1-", n, sep=""))
	if(showLegend)
	{
		brks[1]<-rg[1]
		brks[length(brks)]<-rg[2]
		leg<-sapply(seq(ncolors), function(curbr)
			{
				paste(round(brks[curbr], roundLegend),
					round(brks[curbr+1], roundLegend), sep=" - ")
			})
		plot(0,0,type="n", axes = FALSE, frame.plot = FALSE)
		legend("topleft", legend=leg, text.col=heat.colors(ncolors), bg="black")
		layout(1)
	}
	invisible(rv)
}

#start snowfall and run initial scripts on all instances
#antonov: if TRUE, use server paths, else use local paths
#parallel: if FALSE, run the work sequentially
#serverFolder / localFolder: local path where initial script are
#scriptsInSubFolderToRun: subpath/filenames to the initial scripts
#serverCPUs / localCPUs: number of CPUs to be used if parallel
doSf<-function(antonov=FALSE, parallel=TRUE,
	scriptsInSubFolderToRun=c("Dysphagia/ModelSelection.Prediction.MT.MICE.r",
		"Dysphagia/Dysphagia.ModelSelection.Prediction.MT.MICE.r"),
	serverCPUs=10, localCPUs=2, serverFolder="/home/nsabbe/Dropbox/Doctoraat/",
	localFolder="C:\\users\\nisabbe\\Documents\\@Doctoraat\\")
{
	require(snowfall)
	cpus<-ifelse(antonov, serverCPUs, localCPUs)
	if(antonov)
	{
		parallel<-TRUE
	}
	sfInit(parallel=parallel, cpus=cpus)
	cat("Started snowfall. Will now run scripts:\n")
	whereToRun<-ifelse(antonov, serverFolder, localFolder)
	whatToRun<-paste(whereToRun, scriptsInSubFolderToRun, sep="")
	print(whatToRun)
	sapply(whatToRun, sfSource)
}

#typical use:
#require(snowfall)
#doSf(antonov=TRUE)
#sfLibrary(snowfall)

#function to find significant testresults when controlling the FDR at alpha
HB<-function(p,alpha=0.05) {
	# p: vector with p-values
	# Hochberg and Benjamini procedure for FDR at level alpha
	o<-order(p) # indices of sorted p
	p<-sort(p)
	m<-length(p)
	boundary<-(1:m)*alpha/m
	condition<-(p<=boundary)
	reject<-NULL
  if(sum(condition)>0)#any are smaller than the boundaries
	{
    q<-max((1:m)[condition]) #biggest position smaller than its boundary
  	reject<-o[1:q] # indices of rejected null hypotheses
  }
  return(reject)
}

#Three types of 'significant' results: no correction, FDR (HB) and FWER
#		(Bonferroni)
#sa: dataframe or similar with one row holding p-values
#rown: row name/number where the p-values are
#alpha: control FDR or similar at alpha level
#showSig: if TRUE, the results show the significant values, otherwise the non-
#   significant values are returned
#Name: for display, see print.checkSig
checkSig<-function(sa, rown, alpha, showSig=TRUE, Name=rown)
{
	pvals<-sa[rown,]
	sgn<-ifelse(showSig, 1, -1)

	HBPValsWhere<-HB(pvals, alpha) #Hochberg-Benjamini correction (FDR)
	HBPVals<-pvals[sgn*HBPValsWhere]
	names(HBPVals)<-colnames(sa)[sgn*HBPValsWhere]

	n<-length(pvals)
	BFPValsWhere<-HB(pvals, alpha/n) #Bonferroni correction (FWER)
	BFPVals<-pvals[sgn*BFPValsWhere]
	names(BFPVals)<-colnames(sa)[sgn*BFPValsWhere]

	StdPValsWhere<-which(pvals<alpha) #Uncorrected
	StdPVals<-pvals[sgn*StdPValsWhere]
	names(StdPVals)<-colnames(sa)[sgn*StdPValsWhere]

	rv<-list(HBPVals=HBPVals, BFPVals=BFPVals, StdPVals=StdPVals, Name=Name,
		Alpha=alpha)
	class(rv)<-"checkSig"
	rv
}

#display function for checkSig class
print.checkSig<-function(x,...)
{
	printNamedVectorAsCol<-function(nv){print(unlist(nv))}
	cat("Significance checks for measure", x$Name, ", with alpha=", x$Alpha,
		":\n")
	cat("1) When using Hochberg-Benjamini correction (FDR):\n")
	printNamedVectorAsCol(x$HBPVals)
	cat("2) When using Bonferroni correction (FWER):\n")
	printNamedVectorAsCol(x$BFPVals)
	cat("3) When using NO correction:\n")
	printNamedVectorAsCol(x$StdPVals)
}

#Invert a symmetric matrix
#careful: if you're not entirely sure the matrix is positive definite (or other
#		problems with the matrix set this to TRUE. If cholesky decomposition fails,
#		it will use solve next (which might also fail though). Probably slower.
#silent: if TRUE, the error occurring is in cholesky is hidden from the user
invertSymmetric<-function(symMat, careful=FALSE, silent=FALSE)
{
	if(careful)
	{
		#I expect this to slow things down considerably!!
		inv<-tryRet(chol2inv(chol(symMat)), silent = silent, errRet = NULL)
		if(class(inv)=="try-error")
		{
			inv<-solve(symMat)
		}
		return(inv)
	}
	else
	{
		return(chol2inv(chol(symMat)))
	}
}

#similar as cat, but appends current system time + a newline
catt<-function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
	append = FALSE)
{
	cat(..., format(Sys.time(), "(%Y-%m-%d %H:%M:%S)"), "\n", file = file, 
		sep = sep, fill = fill, labels = labels, append = append)
}

#only output if condition is TRUE
catif<-function(cond=TRUE, ...)
{
	if(cond)
	{
		cat(...)
	}
}

#Combination of catif and catt
cattif<-function(cond=TRUE, ...)
{
	if(cond)
	{
		catt(...)
	}
}

#cattif(TRUE, "test", "hallo", c(1,2,3))

#only print if condition == TRUE
printif<-function(cond=TRUE, ...)
{
	if(cond)
	{
		print(...)
	}
}

#get colors for a set of values to display them
#x: values to translate to color
#mini: value that refers to the lowest possible value, i.e. the 'smallest' color
#maxi:...
#minFirstColor: minimum value for the first color in the gradient
#		(between 0 and 1)
#maxSecondColor: maximum value for the second color in the gradient 
#   (between 0 and 1)
#constThirdColor: third color is held constant at this value (between 0 and 1)
#returnAsString: if TRUE, return #00FF00 style, otherwise return rgb color
#orderColors: can hold any order of the 3 "r", "g" and "b" -> first, second and
#		third colors above
#verbosity: if > 0: display information on forming the colors, mainly for
#   debugging
getColorRange<-function(x, mini=min(x), maxi=max(x), minFirstColor=0.5,
	maxSecondColor=0.5, constThirdColor=0, returnAsString=FALSE,
	orderColors=c("r", "g", "b"), verbosity=0)
{
	if(any(x < mini))
	{
		stop("values are not allowed to be smaller than the minimum (", mini, ")")
	}
	if(any(x > maxi))
	{
		stop("values are not allowed to be bigger than the maximum (", maxi, ")")
	}
	x.pctOfMaxMinMin<-(x - mini)/(maxi - mini)
	fcFact<-1-minFirstColor
	on1Scale<-cbind(fcFact*x.pctOfMaxMinMin+minFirstColor,
		maxSecondColor*(1-x.pctOfMaxMinMin), constThirdColor)
	cattif(verbosity>0, "Color values on scale 1:")
	if(verbosity > 0)
	{
		print(on1Scale)
	}
	rOn1Scale<-on1Scale[,orderColors=="r"]
	gOn1Scale<-on1Scale[,orderColors=="g"]
	bOn1Scale<-on1Scale[,orderColors=="b"]
	cattif(verbosity>0, "Color values on scale 1, in order RGB:")
	if(verbosity > 0)
	{
		print(cbind(rOn1Scale,gOn1Scale,bOn1Scale))
	}
	#sprintf("%02X", 255)
	if(returnAsString)
	{
		r.s<-paste( sprintf("%02X", round(rOn1Scale*255)), sep="")
		g.s<-paste( sprintf("%02X", round(gOn1Scale*255)), sep="")
		b.s<-paste( sprintf("%02X", round(bOn1Scale*255)), sep="")
		return(paste("#", r.s, g.s, b.s, sep=""))
	}
	else
	{
		return(rgb(rOn1Scale, gOn1Scale, bOn1Scale, 1))
	}
}

#getColorRange(c(1,2,3), returnAsString=TRUE, orderColors=c("g", "b", "r"),
#	verbosity=1)

#find Direct incidences in a supposed order
#x: list holding objects to be compared (e.g. sets, models, ...)
#isAncestorOf: function that checks whether its lhs argument is an ancestor of
#   its rhs argument (i.e.: the order function)
#returnPosPairs:
#		* if "indices", return a matrix with two columns, child and parent, holding
#       indices within x
#		* if "items", return a list where each item is itself a list with members
#        child and parent, holding the items themselves
#   * otherwise, return a list where each item is itself a list with members
#         curx (actual child item) and parents (indices of parents within x)
#verbosity: if > 0, display progress (only passed to removeItems at this time)
findParentTree<-function(x, isAncestorOf=get(">"), returnPosPairs="indices",
	verbosity=0)
{
	allAncestors<-lapply(x, function(curx){
			list(curx=curx, ancestors=which(sapply(x, isAncestorOf, curx,
				USE.NAMES = FALSE)))
		})
	allParents<-lapply(allAncestors, function(ancestorsOfCurX){
			metaAncestorsOfCurX<-lapply(ancestorsOfCurX$ancestors,
				function(ancestorIndex){
					allAncestors[[ancestorIndex]]$ancestors
				})
			if(length(metaAncestorsOfCurX) > 0)
			{
				metaAncestorsOfCurX<-do.call(c, metaAncestorsOfCurX)
				metaAncestorsOfCurX<-unique(metaAncestorsOfCurX)
				list(curx=ancestorsOfCurX$curx,
					parents=removeItems(metaAncestorsOfCurX,ancestorsOfCurX$ancestors,
					verbose=(verbosity>1)))
			}
			else
			{
				list(curx=ancestorsOfCurX$curx, parents=NULL)
			}
		})
	if(! is.null(returnPosPairs))
	{
		if(returnPosPairs=="indices")
		{
			directLinks<-do.call(rbind, lapply(seq_along(allParents), function(curxi){
					if(length(allParents[[curxi]]$parents) > 0)
					{
						cbind(child=curxi, parent=unlist(allParents[[curxi]]$parents,
							use.names = FALSE))
					}
					else
					{
						return(NULL)
					}
				}))
			return(directLinks)
		}
		else if(returnPosPairs=="items")
		{
			directLinks<-do.call(c, lapply(allParents, function(parentsForCurX){
					lapply(parentsForCurX$parents, function(curParI){
							list(child=parentsForCurX$curx, parent=allParents[[curParI]]$curx)
						})
				}))
			return(directLinks)
		}
	}
	return(allParents)
}

#get a conservative estimate for the standard error of an AUC, given only AUC
seAUC<-function(AUCHat, nZero, nOne)
{
	#following Hanley en McNeil 1982
	Q1<-AUCHat/(2-AUCHat)
	Q2<-2*AUCHat^2 / (1+AUCHat)
	sqrt((AUCHat*(1-AUCHat) + (nZero-1)*(Q1 - AUCHat^2) +
		(nOne-1)*(Q2 - AUCHat^2)) / (nZero * nOne))
}

bootStrapAUC<-function(pos.scores, neg.scores, bootStrap)
{
	bs.pos<-sample(pos.scores,bootStrap,replace=TRUE)
	bs.neg<-sample(neg.scores,bootStrap,replace=TRUE)
	return(mean(bs.pos > bs.neg) + 0.5 * mean(bs.pos == bs.neg))
}

#calculate AUC for binary outcomes
#probs: (predicted) probabilities
#trueOnes: which items are really 'cases'
#trueZeroes: which items are really 'controls'
#bootStrap: if <= 0: use true observed AUC, else use given number of bootstrap
#   repetitions
#includeSE: if TRUE, also calculate the variance (note the misnamedness!!)
calcAUC.Binary<-function(probs, trueOnes, trueZeroes, bootStrap=0,
	includeSE=FALSE, verbosity=0) #note: does not accoun for NAs
{
	if(! is.logical(trueOnes))
	{
		#make it logical then
		trueOnes<-seq(length(probs)) %in% trueOnes
	}
	if(missing(trueZeroes))
	{
		trueZeroes<-!trueOnes
	}
	else
	{
		if(! is.logical(trueZeroes))
		{
			#make it logical then
			trueZeroes<-seq(length(probs)) %in% trueZeroes
		}
	}
	if(verbosity > 0)
	{
		catt("true zeroes:")
		print(trueZeroes)
		catt("true ones:")
		print(trueOnes)
	}
	NumTZ<-sum(trueZeroes)
	NumTO<-sum(trueOnes)
	if((NumTZ==0) | (NumTO==0))
	{
		AUC<-NA
		varAUC<-NA
	}
	else
	{
		if(bootStrap > 0)
		{
			pos.scores<-probs[trueOnes]
			neg.scores<-probs[trueZeroes]
			cattif(verbosity > 0, "For bootstrapping(", bootStrap, "); pos.scores / neg.scores:")
			if(verbosity > 0)
			{
				print(pos.scores)
				print(neg.scores)
			}
			#bs.pos<-sample(pos.scores,bootStrap,replace=T)
			#bs.neg<-sample(neg.scores,bootStrap,replace=T)
			AUC<-bootStrapAUC(pos.scores, neg.scores, bootStrap)#mean(bs.pos > bs.neg) + 0.5 * mean(bs.pos == bs.neg)
			if(includeSE)
			{
				aucs = replicate(1000,bootStrapAUC(pos.scores, neg.scores, bootStrap))
				varAUC<-var(aucs)
				if(varAUC==0)
				{
					#this should not turn up zero, and is somewhat conservative,
					#i.e. greater variance than occurs
					varAUC<-(seAUC(AUC, nZero=NumTZ, nOne=NumTO))^2
				}
			}
		}
		else
		{
			AUC<-sum(sapply(probs[trueZeroes], function(curControlProb)
				{
					numCaseBigger<-sum(probs[trueOnes] > curControlProb)
					numCaseEqual<-sum(probs[trueOnes] == curControlProb)
					numCaseBigger + (numCaseEqual/2)
				})) / ( (NumTZ) * (NumTO) )
			if(includeSE)
			{
				varAUC<-(seAUC(AUC, nZero=NumTZ, nOne=NumTO))^2
			}
		}
	}
	if(includeSE)
	{
		return(list(AUC=AUC, varAUC=varAUC))
	}
	else
	{
		return(AUC)
	}
}

#Given a vector of criteria (crit) and variances (varcrit) for these, find the
#index of the biggest item that is below the maximum criterion - a given times
#its se
#crit: vector of criterion values (e.g. AUCs)
#varcrit: known/estimated variance for each of the criterion values
#findMaxOf: for each item in crit, after subsetting for valid values based on 
#   crit, this is the value that is maximized within the remaining set. e.g. in 
#   glmnet find the smallest lambda that has AUC below maxAUC-seAUC. Note: if 
#   you want the smallest value just negate the values...
#timesSE: how many times should the SE be subtracted from the highest crit 
#   (could)be zero!!
#verbosity: if >0, progress info is displayed
pickBestConservative<-function(crit, varcrit, findMaxOf=crit, timesSE=1,
	verbosity=0)
{
	#finds the biggest findMaxOf value (i.e.: its index) for which the crit
	#is below or equal to (the maximum crit - its se * timesSE)
	if(length(crit) != length(varcrit)){
		stop("crit and varcrit should be of equal length")}
	if(length(crit) != length(findMaxOf)){
		stop("crit and findMaxOf should be of equal length")}
	mi<-which.max(crit)
	cattif(verbosity > 0, "Original criterium maximum found at position:", mi,
		", value:", crit[mi])
	thres<-crit[mi] - timesSE * sqrt(abs(varcrit[mi]))
	cattif(verbosity > 0, "Criterium threshold:", thres)
	mayberes<-which(crit <= thres)
	if(length(mayberes)==0)
	{
		cattif(verbosity > 0,
			"None were smaller in criterium, so returning the minimum:",
			crit[which.min(crit)], "at position", which.min(crit))
		return(which.min(crit)) #emergency escape
	}
	cattif(verbosity > 0,
		"The following indexes had smaller/equal values for the criterion:\n",
		mayberes)
	truecandidates<-findMaxOf[mayberes] <= findMaxOf[mi]
	if(sum(truecandidates) > 0)
	{
		mayberes<-mayberes[truecandidates]
		cattif(verbosity > 0,
			"Of these, the following had smaller findMaxOf:\n",
			mayberes)
	}
	else
	{
		cattif(verbosity > 0,
			"None were smaller in criterium and smaller in findMaxOf, so returning the minimum:",
			crit[which.min(crit)], "at position", which.min(crit))
		return(which.min(crit)) #emergency escape
	}
	subindex<-which.max(findMaxOf[mayberes])
	cattif(verbosity > 0, "The maximum value for findMaxOf was found at the:",
		subindex, "th position of these, i.e.:", mayberes[subindex], "with value:",
		findMaxOf[mayberes[subindex]])
	return(mayberes[subindex])
}

plot4d<-function(x,y,z, u, main="", xlab="", ylab="", zlab="", ulab="")
{
	require(rgl)#may need to install this package first

	#standard trick to get some intensity colors
	uLim<-range(u)
	uLen<-uLim[2] - uLim[1] + 1
	colorlut<-terrain.colors(uLen)
	col<-colorlut[u - uLim[1] + 1]

	open3d()#Open new device
	points3d(x=x, y=y, z=z,  col=col)
	aspect3d(x=1, y=1, z=1) #ensure bounding box is in cube-form
		#(scaling variables)
	#note: if you want to flip an axis, use -1 in the statement above

	axes3d() #Show axes
	title3d(main = main, sub=paste("Green is low", ulab, ", red is high"),
		xlab = xlab, ylab = ylab, zlab = zlab)
	invisible()
}

#makes it easy to generalize showNonZeroCoef below
coef.Matrix<-function(object,s=dim(object)[2],...)
{
	#cat("Hoorah my coef\n")
	object[,s,drop=FALSE]
}

showNonZeroCoef<-function(x, ...) UseMethod("showNonZeroCoef")
showNonZeroCoef.default<-function(x, atLeast=1, thres=0.00001,s=NULL,...)
{
	#typically used for lists, note that the form in the else
	#may lead to infinite recursion in some cases.
	#This should not happen in glmnet (where the call will be
	#dispatched to the Matrix version
	require(Matrix)
	#cat("default\n")
	if(is.list(x))
	{
		#assume they are all of the same length
		if(is.null(s))
		{
			resList<-lapply(x, coef,...) #allow default to kick in!
		}
		else
		{
			resList<-lapply(x, coef, s,...)
		}
		#cat("resList:\n")
		#print(resList)
		fullx<-do.call(cBind, resList)
		#cat("fullx:\n")
		#print(fullx)
		rv<-showNonZeroCoef(fullx,atLeast=atLeast, thres=thres)
		colnames(rv)<-names(x)
		rv
	}
	else
	{
		showNonZeroCoef(coef(x,...),atLeast=atLeast, thres=thres)
	}
}
showNonZeroCoef.glmnet<-function(x, atLeast=1, thres=0.00001,...)
{
	#cat("glmnet\n")
	showNonZeroCoef(coef(x,...),atLeast=atLeast, thres=thres)
}
showNonZeroCoef.cv.glmnet<-function(x, atLeast=1, thres=0.00001,
	s="lambda.1se",...)
{
	#cat("cv.glmnet\n")
	if(s=="lambda.all")
	{
		showNonZeroCoef(x$glmnet.fit,...)
	}
	else
	{
		showNonZeroCoef(coef(x,s=s,...),atLeast=atLeast, thres=thres)
	}
}
showNonZeroCoef.Matrix<-function(x, atLeast=1, thres=0.00001,...)
{
	#cat("Matrix\n")
	numPosPerCoef<-sapply(seq(dim(x)[1]), function(rn){
		sum(abs(as.vector(x[rn,])) > thres)})
	#cat("numPosPerCoef\n")
	#print(numPosPerCoef)
	#cat("atLeast: ", atLeast, "\n")
	showWhich<-which(numPosPerCoef >= atLeast)
	#cat("showWhich\n")
	#print(showWhich)
	x[showWhich,]
}

displayLinearFromNZC<-function(nZeroCoef, SEs, roundDigits)
{
	if(length(dim(nZeroCoef)) > 1)#also works if nZeroCoef has no dim (e.g. a vector)
	{
		nms<-colnames(nZeroCoef)
		vals<-unlist(nZeroCoef[1,])
	}
	else
	{
		nms<-names(nZeroCoef)
		vals<-unlist(nZeroCoef)
	}
	if(! is.null(SEs))
	{
		if(length(dim(SEs)) > 1)#also works if SEs has no dim (e.g. a vector)
		{
			sess<-unlist(SEs[1,])
		}
		else
		{
			sess<-unlist(SEs)
		}
		sess<-abs(round(1.96*sess, roundDigits))#not perfect, but close enough for most purposes
		sespart<-ifelse(is.na(sess), "", paste("(+/-", sess, ")", sep="") )
	}
	else
	{
		sespart<-rep("", length(vals) )
	}
	seps<-ifelse(vals<0, " -", " +")
	if(seps[1]==" +") seps[1]<-""
	vals<-abs(round(vals, roundDigits))
	nms[nms=="(Intercept)"]<-""

	return(paste(seps, vals, sespart , nms, sep=" ", collapse=""))
}

#ripped this from glmnet to be able to use it directly
gn.error.bars<-function (x, upper, lower, width = 0.02, ...)
{
	xlim <- range(x)
	barw <- diff(xlim) * width
	segments(x, upper, x, lower, ...)
	segments(x - barw, upper, x + barw, upper, ...)
	segments(x - barw, lower, x + barw, lower, ...)
	range(upper, lower)
}

#similar to the start of plot.cv.glmnet
getXIndices<-function(cvobj, xvar=c("norm", "lambda", "dev"))
{
	require(glmnet)
	xvar = match.arg(xvar)
  switch(xvar, norm = {
	  whichNZ = nonzeroCoef(cvobj$glmnet.fit$beta)#it appears there is a
			#	nonzeroCoef in glmnet!!!
	  theBeta = as.matrix(cvobj$glmnet.fit$beta[whichNZ, ])
		index <- apply(abs(theBeta), 2, sum)
  }, lambda = {
		index <- log(cvobj$lambda)
  }, dev = {
		index <- cvobj$glmnet.fit$dev.ratio
  })
  return(index)
}

#add the crossvalidation plot to a recently created coefficient plot for glmnet
addCVPlot<-function(cvobj, xvar=c("norm", "lambda", "dev"), numTicks)
{
	require(glmnet)
 	x<-getXIndices(cvobj, xvar=xvar)
	yrange<-range(coef(cvobj$glmnet.fit))
	truerange<-range(c(cvobj$cvup, cvobj$cvlo))
	scaleFact<-(yrange[2] - yrange[1])/(truerange[2] - truerange[1])
	yvalue<-function(untrans){yrange[1] + (untrans - truerange[1]) * scaleFact  }
  gn.error.bars(x, yvalue(cvobj$cvup), yvalue(cvobj$cvlo), width = 0.01,
		col = "darkgrey")
  points(x, yvalue(cvobj$cvm), pch = 20, col = "red")
  axis(side = 4, at = seq(yrange[1], yrange[2], length.out=numTicks),
		labels = paste(round(seq(truerange[1], truerange[2], length.out=numTicks),
			2)),
		tick = TRUE, line = 0)
  abline(v = x[match(cvobj$lambda.min, cvobj$lambda)], lty = 3)
  abline(v = x[match(cvobj$lambda.1se, cvobj$lambda)], lty = 3)
  invisible()
}

#combine the glmnet plot with the crossvalidation plot
plot2.cv.glmnet<-function(cvobj, xvar=c("norm", "lambda", "dev"), numTicks=5,
	...)
{
	plot(cvobj$glmnet.fit, xvar, ...)
	addCVPlot(cvobj, xvar=xvar, numTicks=numTicks)
}

addLamIndexAxis<-function(cvobj, xvar=c("norm", "lambda", "dev"), numTicks=5,...)
{
 	x<-getXIndices(cvobj, xvar=xvar)
 	useIndex<-as.integer(seq(from=1, to=length(x), length.out=numTicks))
 	useX<-x[useIndex]
  axis(at = useX, labels = paste(useIndex), ...)
}

colsOfType<-function(dfr, type=c("factor", "char"))
{
	useFunc<-function(cl){sum(is.na(cl))}
	if(is.function(type))
	{
		useFunc<-type
	}
	else if(type=="factor")
	{
		useFunc<-is.factor
	}
	else if(type=="char")
	{
		useFunc<-is.character
	}
	#print(useFunc)
	if(inherits(dfr, "data.frame"))
	{
		sapply(dfr, useFunc)
	}
	else
	{
		numFacts<-sapply(seq(dim(dfr)[2]), function(curcn){useFunc(dfr[1,curcn])})
		return(numFacts)
	}
}


refactor<-function(x, labels)
{
	#assumes x is a vector of integers within the range of labels
	factor(x, levels=seq_along(labels), labels=labels)
}

#When you have a list of dataframes of the same format, and you want to combine
#them (as rows) into one...
#Not pretty, but a lot better than rbind!
combineSimilarDfrList<-function(dfrlst)
{#http://stackoverflow.com/questions/5980240/performance-of-rbind-data-frame
	tempRes <- lapply(dfrlst, function(x)
	                     matrix(unlist(x), ncol=ncol(x)))
	result<-as.data.frame(do.call(rbind, tempRes))
	a <- dfrlst[[1]]
	f <- which(sapply(a, class)=="factor")
	#f <- which(sapply(a, is.factor))
	for(i in f) {
	  lev <- levels(a[[i]])
	  result[[i]] <- factor(result[[i]], levels=seq_along(lev), labels=lev)
	}
	colnames(result)<-colnames(dfrlst[[1]])
	return(result)
}

#older version of this
combineSimilarDfrList.prev<-function(dfrlst)
{#http://stackoverflow.com/questions/5980240/performance-of-rbind-data-frame
	n <- 1:ncol(dfrlst[[1]])
	names(n) <- names(dfrlst[[1]])
	as.data.frame(lapply(n, function(i) unlist(lapply(dfrlst, `[[`, i))))
}

naPos<-function(dfr)
{
	percol<-lapply(dfr, function(cl){which(is.na(cl))})
	maybenames<-colnames(dfr)
	do.call(rbind, lapply(names(percol), function(nm){
			curcolrows<-unlist(percol[nm])
			repcol<-rep(match(nm, maybenames), length(curcolrows))
			cbind(cl=repcol, rw=curcolrows)
		}))
}

similarSizeGroups<-function(ngroups, nobs, rand=TRUE)
{
	repsPerPart<-rep(nobs %/% ngroups, ngroups)
	rem<-nobs %% ngroups
	if(rem > 0)
	{
		repsPerPart[seq(rem)]<-repsPerPart[seq(rem)]+1
	}
	if(rand)
	{
		return(sample(rep(seq(ngroups), repsPerPart), size=nobs))
	}
	else
	{
		return(repsPerPart)
	}
}

#typical use: patternNumericCols = "s(X)"
#will result in model text like outCol ~ s(firstColName) + s(ThirdColName)
# + SecondColName
#if only the second column is nonnumeric.
basisExpansionFormula<-function(dfr, outCol, patternNumericCols="X",
	subPatternCol="X", fixed=TRUE,...)
{
	if((patternNumericCols=="X") | (patternNumericCols=="."))
	{
		#rhs<-paste(removeItems(outcol, colnames(dfr)), collapse=" + ")
		rhs<-"."
		return(paste(outCol, rhs, sep=" ~ "))
	}
	numCols<-colnames(dfr)[sapply(dfr, is.numeric)]
	otherCols<-colnames(dfr)[!sapply(dfr, is.numeric)]
	numCols<-removeItems(outCol, numCols)
	otherCols<-removeItems(outCol, otherCols)
	dblPattern1<-paste(subPatternCol, subPatternCol, sep=",") #X,X
	dblPattern2<-paste(subPatternCol, subPatternCol, sep=", ") #X, X
	#if these are found, replace this by all comma-separated numeric variables
	#e.g. for te(X, X, k=5) -> becomes te(X1, X2, X3, X4, k=5)
	if(grepl(dblPattern1, patternNumericCols, fixed=fixed, ...))
	{
		allNumCols<-paste(numCols, collapse=", ")
		convertedNumCols<-gsub(dblPattern1, allNumCols, patternNumericCols,
			fixed=fixed,...)
	}
	else if(grepl(dblPattern2, patternNumericCols, fixed=fixed, ...))
	{
		allNumCols<-paste(numCols, collapse=", ")
		convertedNumCols<-gsub(dblPattern2, allNumCols, patternNumericCols,
			fixed=fixed,...)
	}
	else
	{
		convertedNumCols<-sapply(numCols, function(cc){
				gsub(subPatternCol, cc, patternNumericCols, fixed=fixed,...)
			})
	}
	rhs<-paste(c(convertedNumCols, otherCols), collapse=" + ")
	return(paste(outCol, rhs, sep=" ~ "))
}

getTimeOut<-function(toname="usedTimeOut")
{
	mget(toname, envir=globalenv(), ifnotfound=0)[[1]]
}
setTimeOut<-function(to, toname="usedTimeOut")
{
	invisible(assign(toname, to, envir=globalenv()))
}

#setTimeOut(120)
#getTimeOut()

#testt<-system.time({for(i in 1:1000) cat(i, "\n")})
stopIfRanTooLong<-function(expr, to=getTimeOut(), verbosity=0)
{
	if(verbosity > 0) cat("stopIfRanTooLong with timeout: ", to, "\n")
	st<-system.time({
			rv<-eval(expr, envir=parent.frame())
		})
	if((to > 0) && (tshort(st)[3] >= to)){
		stop("Process took too long, so skipping the rest")}
	invisible(rv)
}

#should cover a lot of cases wothout having to explicitly write a
#fitAndPredictBinary function for it...
fitAndPredictBinary.General<-function(trainDfr, valDfr, outcol, lvls, fitFunc,
	predictType, predictMember, formulaPattern=".", verbosity=0,...)
{
		gf <- basisExpansionFormula(trainDfr, outcol,
			patternNumericCols=formulaPattern)
		curres=fitFunc(formula(gf),data=trainDfr,...)
		pred<-predict(curres, newdata=valDfr, type=predictType)
		if((! missing(predictMember)) && (! is.null(predictMember)) &&
			(predictMember!= "."))
		{
			if(verbosity > 0) cat("Use predictMember: ", predictMember, "\n")
			pred<-unlist(pred[predictMember])
			if(verbosity > 0) cat("Results in pred with structure: ", str(pred), "\n")
		}
		if(is.numeric(pred))
		{
			if(verbosity > 0) cat("Numeric predictions, assuming probabilities...\n")
			#assume these are probabilities
			return(lvls[ifelse(pred > 0.5, 2, 1)])
		}
		else
		{
			return(pred)
		}
}

crossPredictBinary<-function(dfr, outcol, fold=10, fitAndPredictBinary,
	verbosity=0,...)
{
	numObs<-dim(dfr)[1]
	rndgrps<-similarSizeGroups(fold, numObs)
	if(length(outcol)==1)
	{
		predOut<-dfr[,outcol]
	}
	else
	{
		if(verbosity > 0)
		{
			cat("***Special case in crossPredictBinary: outcol was not 1 string, ")
			cat("so assuming it is the outcome itself.\n")
		}
		predOut<-outcol
		orgoutcol<-outcol
	}
	if(is.factor(predOut))
	{
		lvls<-levels(predOut)
	}
	else
	{
		lvls<-sort(unique(predOut))
	}
	for(i in seq(fold))
	{
		if(length(outcol)!=1)
		{
			outcol<-orgoutcol[rndgrps!=i]
		}
		if(verbosity > 0) cat("Currently crossvalidating", i, "/", fold, "\n")
		stopIfRanTooLong({
				if(is.list(fitAndPredictBinary))
				{
					if(verbosity > 0) cat("General fitAndPredictBinary for list!\n")
					predOut[rndgrps==i]<-fitAndPredictBinary.General(dfr[rndgrps!=i,],
						dfr[rndgrps==i,], outcol=outcol, lvls=lvls,
						fitFunc=fitAndPredictBinary$fitFunc,
						predictType=fitAndPredictBinary$predictType,
						predictMember=fitAndPredictBinary$predictMember,
						verbosity=verbosity-1, ...)
				}
				else
				{
					predOut[rndgrps==i]<-fitAndPredictBinary(dfr[rndgrps!=i,],
						dfr[rndgrps==i,],	outcol=outcol, lvls=lvls, verbosity=verbosity-1,
						...)
				}
			})
	}
	return(predOut)
}

doFormulaCV<-function(dfr, outcol, fitFunc, fitAndPredictBinary,
	mainname=paste("m", as.character(match.call()$fitFunc), sep=""),
	classname=paste(as.character(match.call()$fitFunc), "ex", sep=""), 
	formulaPattern="X", passMainRes=FALSE, verbosity=0,...)
{
	#cat("main=", mainname, ", class=", classname, "\n")
	if((! missing(fitFunc)) && (! is.null(fitFunc)) && (is.function(fitFunc)))
	{
		gf<-basisExpansionFormula(dfr, outcol, patternNumericCols=formulaPattern)
		if(verbosity > 2)
		{
			cat("Formula used in doFormulaCV:\n", gf, "\n")
		}
		#add timeout here if mainres takes too long
		stopIfRanTooLong({
			mainres<-fitFunc(formula(gf),data=dfr,...)
		})
	}
	else
	{
		mainres<-"no main result requested"
		cat(mainres, "\n")
	}
	#little solution for often occurring variants -> fitAndPredictBinary.General
	#instead of passing a function, pass a a list w at most 3 members (see above)
	#alternatively, just pass the type argument of predict (as a string)
	if(is.list(fitAndPredictBinary))
	{
		if(verbosity > 0) cat("fitAndPredictBinary was a list\n")
		if(!("fitFunc" %in% names(fitAndPredictBinary)))
		{
			fitAndPredictBinary$fitFunc<-fitFunc
		}
	}
	else if(is.character(fitAndPredictBinary))
	{
		if(verbosity > 0) cat("fitAndPredictBinary was a character\n")
		#assume it was the predictType that was really passed along
		fitAndPredictBinary<-list(fitFunc=fitFunc, predictType=fitAndPredictBinary)
	}
	#do tenfold crossvalidation
	if(verbosity > 0) cat("Will start crossvalidating now\n")
	if(passMainRes)
	{
		if(verbosity > 1) cat("Passing along the main result...\n")
		predOut<-crossPredictBinary(dfr=dfr, outcol=outcol,
			fitAndPredictBinary=fitAndPredictBinary, verbosity=verbosity-1,
			formulaPattern=formulaPattern, mainRes=force(mainres),...)
	}
	else
	{
		if(verbosity > 1) cat("Ignoring the main result...\n")
		predOut<-crossPredictBinary(dfr=dfr, outcol=outcol,
			fitAndPredictBinary=fitAndPredictBinary, verbosity=verbosity-1,
			formulaPattern=formulaPattern,...)
	}
	retval<-list(mainres, cv=predOut)
	names(retval)[1]<-mainname
	class(retval)<-classname
	return(retval)
}

corWithCat<-function(dfr,...)
{
	facts<-which(sapply(dfr, is.factor))
	for(i in facts)
	{
		dfr[[i]]<-as.integer(dfr[[i]])
	}
	cor(dfr,...)
}

corWithoutCat<-function(dfr,...)
{
	facts<-which(sapply(dfr, is.factor))
	if(length(facts) > 0)
	{
		dfr<-dfr[,-facts]
	}
	cor(dfr,...)
}










simpleDescription<-function(xparlist, depth=-1)
{
	xparpart<-sapply(seq_along(xparlist), function(curpari){
			if(is.function(xparlist[[curpari]]))
			{
				return(paste("function:", names(xparlist)[curpari]))
			}
			else
			{
				curprt<-xparlist[[curpari]]
				if(! is.null(dim(curprt)))
				{
					curprttxt<-paste(class(curprt), " of dimensions (", dim(curprt)[1],
						", ", dim(curprt)[2], ")", sep="")
				}
				else if(is.list(curprt))
				{
					if(depth != 0)
					{
						curprttxt<-paste("[", simpleDescription(curprt, depth=depth-1),
							"]", sep="")
					}
					else
					{
						curprttxt<-paste("list of length ", length(curprt), sep="")
					}
				}
				else if(length(curprt) > 1)
				{
					curprttxt<-paste(class(curprt), " of length ", length(curprt), sep="")
				}
				else
				{
					curprttxt<-as.character(curprt)
				}

				paste(names(xparlist)[curpari], "=", curprttxt, sep="")
			}
		})
	paste(xparpart, collapse=", ")
}

showParList<-function(xparlist, depth=-1)
{
	invisible(cat(simpleDescription(xparlist, depth=depth), "\n"))
}

fitAndPredictContinuous.General<-function(trainDfr, valDfr, outcol, 
	orgList, formulaPattern=".", verbosity=0,...)
{
	fitFunc<-orgList$fitFunc
	predictType<-orgList$predictType
	predictMember<-orgList$predictMember
	postProcessPred<-orgList$postProcessPred
	orgList$fitFunc<-NULL
	orgList$predictType<-NULL
	orgList$predictMember<-NULL
	orgList$postProcessPred<-NULL

	if(verbosity > 0)
	{
		cat("In fitAndPredictContinuous.General.\n")
		cat("\tdim(trainDfr):", dim(trainDfr), "\n")
		cat("\tdim(valDfr):", dim(valDfr), "\n")
		if(length(outcol) > 1)
		{
			cat("\tlength(outcol):", length(outcol), "\n")
		}
		else
		{
			cat("\toutcol:", outcol, "\n")
		}
		cat("\tformulaPattern:", formulaPattern, "\n")
		if(length(list(...)) > 0)
		{
			cat("Extra params:(", length(list(...)), ")\n")
			showParList(list(...))
			cat("End extra parameters\n")
		}
		else
		{
			cat("NO Extra params\n")
		}
		if(length(orgList) > 0)
		{
			cat("orgList:(", length(orgList), ")\n")
			showParList(orgList)
			cat("End orgList\n")
		}
	}
	gf <- basisExpansionFormula(trainDfr, outcol,
		patternNumericCols=formulaPattern)
	if(verbosity > 0) cat("Formula obtained:\n", gf, "\n")

	curres<-fitFunc(formula(gf),data=trainDfr,...)
	if((missing(predictType)) || (is.null(predictType)) || (predictType==""))
	{
		pred<-predict(curres, newdata=valDfr)
	}
	else
	{
		pred<-predict(curres, newdata=valDfr, type=predictType)
	}
	if((! missing(predictMember)) && (! is.null(predictMember)) &&
		(predictMember!= "."))
	{
		if(verbosity > 0) cat("Use predictMember: ", predictMember, "\n")
		pred<-unlist(pred[predictMember])
	}
	if((! missing(postProcessPred)) && (! is.null(postProcessPred)) &&
		(is.function(postProcessPred)))
	{
		if(verbosity > 0) cat("Postprocessing prediction\n")
		orgList$pred<-pred
		orgList<-c(orgList, list(...))
		pred<-do.call(postProcessPred, orgList)
	}
	if(verbosity > 0)
	{
		cat("->fitAndPredictContinuous.General Results in pred with structure:\n")
		print(str(pred))
	}
	return(pred)
}

crossPredictContinuous<-function(dfr, outcol, fold=10, fitAndPredictContinuous,
	verbosity=0,...)
{
	numObs<-dim(dfr)[1]
	rndgrps<-similarSizeGroups(fold, numObs)
	if(length(outcol)==1)
	{
		predOut<-dfr[,outcol]
	}
	else
	{
		if(verbosity > 0)
		{
			cat("***Special case in crossPredictContinuous: outcol was not 1 string")
			cat(", so assuming it is the outcome itself.\n")
		}
		predOut<-outcol
		orgoutcol<-outcol
	}
	for(i in seq(fold))
	{
		if(length(outcol)!=1)
		{
			outcol<-orgoutcol[rndgrps!=i]
		}
		if(verbosity > 0) cat("Currently crossvalidating", i, "/", fold, "\n")
		if(is.list(fitAndPredictContinuous))
		{
			if(verbosity > 0) cat("General fitAndPredictContinuous for list!\n")
			predOut[rndgrps==i]<-fitAndPredictContinuous.General(dfr[rndgrps!=i,],
				dfr[rndgrps==i,], outcol=outcol, 
				orgList=fitAndPredictContinuous,
				verbosity=verbosity-1, ...)
		}
		else
		{
			predOut[rndgrps==i]<-fitAndPredictContinuous(dfr[rndgrps!=i,],
				dfr[rndgrps==i,],	outcol=outcol, verbosity=verbosity-1, ...)
		}
	}
	return(predOut)
}


evaluatePredictions.lms<-function(predicted, realval)
{
	return(sqrt(mean((predicted-realval)^2)))
}

getFormulaCVDescription<-function(fitFunc, ffDesc, formulaPattern, xparlist)
{
	expa<-simpleDescription(xparlist)
	if(is.null(expa)) expa<-""
	expat<-ifelse(expa=="","no extra parameters",paste("extra parameters", expa))
	if(!is.character(ffDesc))
	{
		cat("Unexpected in getFormulaCVDescription: ffDesc was not character.\n")
		ffDesc<-as.character(match.call()$fitFunc)
		if(!is.character(ffDesc))
		{
			cat("--->fitFunc could also not be converted to character...\n")
			ffDesc<-"unknown function (?)"
		}
	}
	paste(ffDesc, " with formula pattern ", formulaPattern,
		", and ", expat, sep="")
}

createCVResult<-function(predOut, dfr, outcol, evaluatePredictions,
	includePreds=FALSE, includeDesc=FALSE,
	fitFunc, ffDesc, formulaPattern, xparlist)
{

	retval<-evaluatePredictions(predOut, dfr[,outcol])
	if(includePreds | includeDesc)
	{
		retval<-list(evaluated=retval)
		if(includePreds)
		{
			retval$preds<-predOut
		}
		if(includeDesc)
		{
			retval$desc<-getFormulaCVDescription(fitFunc, ffDesc, formulaPattern,
				xparlist)
			class(retval)<-"CVRes"
		}
	}
	return(retval)
}

print.CVRes<-function(x,...)
{
	cat(x$desc, "\n")
	cat("\tEvaluated:", x$evaluated, "\n")
}

#in contrast with the binary version, I immediately return the evaluation
#here.
doFormulaContCV<-function(dfr, outcol, fitFunc, fitAndPredictContinuous,
	formulaPattern="X", evaluatePredictions=evaluatePredictions.lms,
	includePreds=FALSE, includeDesc=FALSE, verbosity=0, ffDesc,...)
{
	if((missing(ffDesc)) || (is.null(ffDesc)))
	{
		ffDesc<-as.character(match.call()$fitFunc)
	}
	#Solution for often occurring variants ->fitAndPredictContinuous.General
	#instead of passing a function, pass a a list w at most 3 members (see above)
	#alternatively, just pass the type argument of predict (as a string)
	if(is.list(fitAndPredictContinuous))
	{
		if(verbosity > 0) cat("fitAndPredictContinuous was a list\n")
		if(!("fitFunc" %in% names(fitAndPredictContinuous)))
		{
			fitAndPredictContinuous$fitFunc<-fitFunc
		}
	}
	else if(is.character(fitAndPredictContinuous))
	{
		if(verbosity > 0) cat("fitAndPredictContinuous was a character\n")
		#assume it was the predictType that was really passed along
		fitAndPredictContinuous<-list(fitFunc=fitFunc,
			predictType=fitAndPredictContinuous)
	}
	#do tenfold crossvalidation
	if(verbosity > 0) cat("Will start crossvalidating now\n")
	
	predOut<-crossPredictContinuous(dfr=dfr, outcol=outcol,
		fitAndPredictContinuous=fitAndPredictContinuous, verbosity=verbosity-1,
		formulaPattern=formulaPattern,...)

	retval<-createCVResult(predOut, dfr, outcol, evaluatePredictions,
		includePreds, includeDesc, fitFunc, ffDesc, formulaPattern,
			xparlist=list(...))
	return(retval)
}


displayPreds<-function(res, dfr, outcol, ...) UseMethod("displayPreds")
displayPreds.default<-function(res, dfr, outcol, item, ...)
{
	if(is.list(res))
	{
		if("preds" %in% names(res))
		{
			class(res)<-"CVRes"
			displayPreds(res, dfr, outcol,...)
		}
		else
		{
			displayPreds(res[[item]], dfr, outcol,...)
		}
	}
	else
	{
		res<-list(preds=res)
		class(res)<-"CVRes"
		displayPreds(res, dfr, outcol,...)
	}
}
displayPreds.CVRes<-function(res, dfr, outcol, ...)
{
	resdfr<-data.frame(trueVal=dfr[,outcol], predicted=res$preds)
	resdfr$ssq<-(resdfr$predicted-resdfr$trueVal)^2
	invisible(edit(resdfr))
}

#Clean up an existing data.frame: make factor-like columns truly factors,
#and move all factor columns to the beginning of the data.frame
makeFactorsAndSetFrontal<-function(dfr, treatAsFactorWhenDiffValRate=0.05, verbosity=0)
{
	#Make specific columns factor
	#We look for columns that are character / have very few different values
	#and convert these to factors
	numObs<-dim(dfr)[1]
	numVars<-dim(dfr)[2]
	facts<-colsOfType(dfr, "factor")
	orgfacts<-which( facts)#    apply(dfr, 2, is.factor))
	orgchar<-which( colsOfType(dfr, "char"))#    apply(dfr, 2, is.character))
	rateDiffVals<-sapply(seq(numVars), function(vari){
			if(facts[vari])
			{
				return(length(levels(dfr[1, vari])))
			}
			else
			{
				fr<-factor(dfr[, vari])
				return(length(levels(fr[1])))
			}
		}) / numObs
	orgfewvals<-which(rateDiffVals < treatAsFactorWhenDiffValRate)
	if(verbosity > 0) orgfewnofact<-orgfewvals[!(orgfewvals %in% orgfacts)]#only for tracing
	newfactors<-unique(c(orgchar, orgfewvals))
	newfactors<-newfactors[!(newfactors %in% orgfacts)]
	if(verbosity > 0) cat("makeFactorsAndSetFrontal\n")
	if(verbosity > 0) cat("factors up front:", orgfacts, "(", length(orgfacts), ")", "\n")
	if(verbosity > 0) cat("charcols up front:", orgchar, "(", length(orgchar), ")", "\n")
	if(verbosity > 0) cat("columns with few diff values up front:", orgfewvals, "(", length(orgfewvals), ")", "\n")
	if(verbosity > 0) cat("columns with few diff values non-fact:", orgfewnofact, "(", length(orgfewnofact), ")", "\n")
	if(verbosity > 0) catt("new factors:", newfactors)
	for(i in newfactors)
	{
		dfr[,i]<-factor(dfr[,i])
	}
	#reorder columns if necessary
	allfacts<-sort(c(newfactors, orgfacts))
	restCols<-((1:numVars)[-allfacts])
	return(dfr[,c(allfacts,restCols)])
}

randomFillDS<-function(ds)
{
	#for all the categorical cols
	fakeData<-ds
	nrnas<-unlist(apply(fakeData, 2, function(curcol){sum(is.na(curcol))}))
	hasnas<-which(nrnas>0)
	for(i in hasnas)
	{
		nna<-nrnas[i]
		wherenas<-which(is.na(fakeData[,i]))
		wasChar<-FALSE
		if(is.character(fakeData[,i]))
		{
			fakeData[,i]<-factor(fakeData[,i])
			wasChar<-TRUE
		}
		if(is.factor(fakeData[,i]))
		{
			nlev<-length(levels(fakeData[,i]))
			fakeData[wherenas,i]<-levels(fakeData[,i])[sample.int(nlev, nna, replace=TRUE)]
			if(wasChar)
			{
				fakeData[,i]<-as.character(fakeData[,i])
			}
		}
		else
		{
			#for now: just use the mean for the missing values in this fake data
			colm<-mean(fakeData[,i], na.rm=TRUE)
			fakeData[wherenas,i]<-colm
		}
	}
	return(fakeData)
}

marginalProbPerCat<-function(dfr)
{
	catCols<-which(sapply(dfr[1,], is.factor))
	lapply(catCols, function(curcol){
			tbl<-table(dfr[,curcol])
			return(tbl/sum(tbl))
		})
}
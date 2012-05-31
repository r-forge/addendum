\name{run.parallel}
\alias{run.parallel}
\alias{do.parallel}
\alias{sfInitEx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Divide taks over multiple processors in \code{snowfall} setting and gather results
}
\description{
Set of functions to run a function over a set of parameters, where the work is
divided over processors (or even computers). Results of every subtask are saved
(so they can be recovered regardless of the other ones), and logging is sent to
a separate file.
}
\usage{
run.parallel(..., paramcreationname, functionname, paramname, logdir,	savedir=logdir, logorsavename= paste(functionname, "parallel", sep="_"),	postprocessname=NULL, loadLibsIfSfNotRunning=c("Matrix", "glmnet", "addendum"))
do.parallel(i, functionname, paramname, logdir, savedir=logdir, logorsavename= paste(functionname, "parallel", sep="_"), postprocessname=NULL, verbosity)
sfInitEx(parallel = NULL, cpus = NULL, type = NULL, socketHosts = NULL, 
					restore = NULL, slaveOutfile = NULL, nostart = FALSE, useRscript = FALSE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
passed on to the function matched by \code{paramcreationname}
}
  \item{paramcreationname}{
function that creates the set of objects (parameters) that is shared between all
processes
}
  \item{functionname}{
function that needs to be \code{\link{do.call}}ed with the subsetted parameters
}
  \item{paramname}{
name that can be safely given to the parameter object in each process, so it can
be safely passed along (without overwriting other objects)
}
  \item{logdir}{
directory where the output of each process is sent to (if \code{NULL}, output is
not rerouted, so is typically lost).
}
  \item{savedir}{
directory where the result of each process is saved in (if \code{NULL}, results
are not saved). Default is the same as the  \code{logdir}.
}
  \item{logorsavename}{
Pattern for the logname and the savename (if either is relevant). These are
followed by "_", then the task number (i.e. ordinal in \code{1:length(param)}),
and then, respectively ".txt" and ".saved".
}
  \item{postprocessname}{
name of function that is applied to the result after saving it (useful for saving
a big result but returning a smaller version of it). If it is \code{NULL} (the
default) or the function is not found, the original result is returned.
}
  \item{loadLibsIfSfNotRunning}{
If there wasn't a snowfall cluster running up front, a single CPU one will be 
started. After that, each of the libraries in this character vector will be
\code{\link{sfLibrary}}d. A warning is issued, as this is not the intended use.
}
  \item{i}{
subtask number - see details.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{parallel, cpus, type, socketHosts, restore, slaveOutfile, nostart, useRscript}{
see \code{\link{sfInit}}
}
}
\details{
The key to success here, is providing a proper parameter creation function, and
optionally a \code{\link{length}} function for its result, and a subset operator
(\code{"["}) for its result.

The parameter creation function should collect all the data that is passed along
in \code{\dots} in a list somehow. This is typically all the data that is needed
across all subtasks. The advantage of working this way is that the data needs to
be copied only once to every process (and this is done by \code{rub.parallel})!

Typically, the parameter creation function will return a list object of some
custom class. In what follows, I shall assume this object is of class "testing".

The number of subtasks will be obtained by calling \code{length} on the parameter
object. So it is typically best to write a custom \code{length.testing} for this
class (parameterless function).

Next, the parameter that actually applies for the current subtask is required.
This is obtained by \code{do.parallel} by passing the subtask number to the
subset operator applied to the common parameter object. For this to work, one
typically needs to write a custom "[.testing" for this class (2 parameters,
\code{x} and \code{i})

For simple cases (see examples), these customisations are not necessary

Note: sfInitEx is provided as an extension to sfInit that automatically connects
to an existing MPI cluster if one is already running.
}
\value{
Depends on \code{functionname} and others, but typically a list of the results
of the subtasks (after postprocessing)
}
\author{
Nick Sabbe
}
\note{
wrt logging/saving: it is required that the folder exists.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{originalColumnNamesFromDummyVars}}
}
\examples{
\dontrun{	defparms<-function(...)
	{
		#+/- assumes the first parameter is a vector or list
		retval<-list(...)
		catw("List of parameters passed (structure):")
		str(retval, max.level=2)
		return(retval[[1]])
	}

	defprocess<-function(namethiswhatever)
	{
		catw("I'm definitely hoping this will work.")
		catw("Will now attempt to print my parameter.")
		print(namethiswhatever)
		catw("There, that's done. Let's return it as well.")
		return(namethiswhatever)
	}
	sfInit(parallel = TRUE, cpus = 2)
	sfExport(list=c("defparms", "defprocess"))
	sfLibrary(addendum)
	tst<-run.parallel(letters[1:5], verbosity=10, paramcreationname="defparms",
		functionname="defprocess", paramname="tst_param",
		logdir="./")
	sfStop()}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ parallel }
\keyword{ snowfall }% __ONLY ONE__ keyword per line

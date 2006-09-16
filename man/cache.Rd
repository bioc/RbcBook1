\name{cache}
\alias{cache}
\title{ cache an R expression to use the result without
recomputation in later
Sweave events }
\description{ cache an R expression to use the result without
recomputation in later
Sweave events }
\usage{
cache(name, expr)
}
\arguments{
  \item{name}{ character variable}
  \item{expr}{ R expression}
}
\details{
If the object \code{tmp-[name].Rdata} does not exist,
\code{expr} is evaluated and written to \code{tmp-[name].Rdata}.
Otherwise, \code{tmp-[name].Rdata} is loaded and assigned
to \code{[name]}.

Note that if a graph is produced as a side effect of
the cached command, it will not be produced when the
cached object is encountered in future weavings.
To avoid this problem, explicitly create the graphics
output so that it persists and can be reused on future
weavings.  In general this process should be avoided
by making all graphical activities explicit, not byproducts
of expensive calculations that are likely to be cached.
In other words, all expensive calculations for which visualizations
are of interest should return an object for which a plotting
method is defined.  The object calculation can be cached and
the plotting method can be applied to the cached object.
}
\value{
The value of \code{expr}.
}
\author{Wolfgang Huber <w.huber@dkfz.heidelberg.de>}
\examples{
library(graph)
unix.time(cache("bigran", randomGraph(paste("a", 1:100), 1:20, .3)))
dir()
unix.time(cache("bigran", randomGraph(paste("a", 1:100), 1:20, .3)))
}
\keyword{ models }

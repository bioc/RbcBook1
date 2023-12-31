\name{rpart2gNEL}
\alias{rpart2gNEL}
\alias{rpart2gNEL}
\alias{grabSplitV}
\alias{remapAff}
\title{rpart to graphNEL Converter}
\description{Creates an object of class \code{graphNEL} from one of class \code{rpart}.}
\usage{
rpart2gNEL(tr, remap=function(x) x, nsep="\n")
}
\arguments{
  \item{tr}{instance of \code{rpart} class from \code{\link[rpart]{rpart}} package.}
  \item{remap}{function that maps node names in tree to node names in graph.}
  \item{nsep}{token that separates node name and vote tally in the rendering.}
}
\details{
Not explicitly used in the book, but the basis for a certain graph
illustrating application of rpart.  The newer coin/party packages
have more interesting rendering approaches based on graphviz.

\code{grabSplitV} is a utility function for acquiring the variable
names, \code{remapAff} will remap affymetrix probe names to gene
symbols for use in the \code{remap} parameter.
}
\value{
a \code{\link[graph]{graphNEL-class}} instance with nodes
constructed to convey information on the data tree fit
}
\author{Vince Carey <stvjc@channing.harvard.edu>}
\seealso{\code{\link[rpart]{rpart}}, \code{\link[graph]{graphNEL-class}}}
\examples{
library(rpart)
example(rpart)
ff = rpart2gNEL(fit2)
ff
nodes(ff)
}
\keyword{models}

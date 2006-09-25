\name{rpart2gNEL}
\alias{rpart2gNEL}
\alias{rpart2gNEL}
\alias{grabSplitV}
\alias{remapAff}
\title{convert an rpart object to a graphNEL instance   }
\description{
}
\usage{
rpart2gNEL(tr, remap=function(x) x, nsep="\n")
}
\arguments{
  \item{tr}{ instance of \code{rpart} class from \code{\link{rpart}} package}
  \item{remap}{ function that maps node names in tree to node names in graph}
  \item{nsep}{ token that separates node name and vote tally in the rendering}
}
\details{
Not explicitly used in the book, but the basis for a certain graph
illustrating application of rpart.  The newer coin/party packages
have more interesting rendering approaches based on graphviz.

\code{grabSplitV} is a utility function for acquiring the variable
names, \code{remapAff} will remap affymetrix probe names to gene
symbols for use in the \code{remap} parameter
}
\value{
a \code{\link[graph]{graphNEL-class}} instance with nodes
constructed to convey information on the data tree fit
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
library(rpart)
example(rpart)
ff = rpart2gNEL(fit2)
ff
nodes(ff)
}
\keyword{ models }

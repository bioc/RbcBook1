\name{bcr.cor}
\alias{bcr.cor}
\alias{bcr.kldist}
\alias{bcr.euc}
\alias{bcr.man}
\alias{bcr.mi}
\alias{bcr.spear}
\alias{bcr.tau}
\title{illustrative datasets for distance measures  }
\description{illustrative datasets for distance measures
}
\usage{
data(ALL.dist) # all the components live in here
objects()
}
\details{
These datasets are used in conjunction with the bioDist package
for the distance chapter of the monograph
}
\value{
these are \code{\link[stats]{dist}} objects
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }
%
%
%\seealso{  }
%
\examples{
data(ALL.dist)
dim(as.matrix(bcr.cor))
}
\keyword{ models }

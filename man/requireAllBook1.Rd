\name{require.RbcBook1}
\alias{require.RbcBook1}
\title{Load a lengthy list of packages that are used in the Book}
\description{Load a lengthy list of packages that are used in the Book}
\usage{
require.RbcBook1()
.RbcBook1.pkgs()
}
\arguments{
}
\details{\code{require.RbcBook1}
  can be used to see at once whether all packages required
  for the book can be loaded.

  To do for \code{.RbcBook1.pkgs}: version numbers?
}
\value{
  \code{.RbcBook1.pkgs} returns a character vector with the names of
  all packages used in the book.
}
\author{Wolfgang Huber <huber@ebi.ac.uk>}
\examples{
\dontrun{
library(reposTools)
install.packages2(.RbcBook1.pkgs(), develOK=TRUE)
require.RbcBook1()
}}
\keyword{misc}

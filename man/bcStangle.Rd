\name{bcStangle}
\alias{bcStangle}
\alias{tangleToSingleFiles}
\title{A function to extract the code chunks from the book for the
  online supplement}
\description{A function to extract the code chunks from the book for the
  online supplement.}
\usage{
 bcStangle(files = .RbcBook1Files(), outfile= "bioCSpringer.R")
 tangleToSingleFiles()
}
\arguments{
  \item{files}{character vector with filenames of book chapters.
    Must have extension \code{.Rnw}}
  \item{outfile}{character of length 1 with the name of the output file.}
}
\details{
  It should suffice to change to the book's top level directory
  (e.g. .../Springer1/trunk/Book1)
  and run \code{bcStangle()}.

  This function is specialized to its one purpose given in the title.
  It is not envisaged that it could be useful beyond, or even replace Stangle.

  \code{tangleToSingleFiles} creates a directory named Rfiles and tangles 
  each chapter separately into this directory. You must have the current
  working directory set to the top level directory of the book 
  (Springer/Book1).
}

\seealso{.RbcBook1Files, Stangle}

\value{
  Function is called for its side effects.
}

\author{Wolfgang Huber <huber@ebi.ac.uk>}
\examples{
  ## bcStangle()
}

\keyword{misc}

\name{imageSize}
\alias{imageSize}
\title{File info for all image files in the book}
\description{File info for all image files in the book}
\usage{imageSize(dirs= c("Preproc","Analysis","Metadata","Graphs","CaseStudies"), ext=c("pdf", "png"))}
\arguments{
  \item{dirs}{character vector with directories.}
  \item{ext}{character vector with file name extensions.}
}
\details{
  No details.
}

\value{
  Data frame.
}

\author{Wolfgang Huber <huber@ebi.ac.uk>}
\examples{
  # a = imageSize()
  # print(a[1:10,c(1,4)])
}

\keyword{misc}

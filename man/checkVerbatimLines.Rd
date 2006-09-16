\name{checkingBookSources}
\alias{checkVerbatim}
\alias{checkRnw}
\alias{checkPackage}
\alias{.RbcBook1Files}
\title{Formatting and standardization checks on book chapters}
\description{Formatting and standardization checks on book chapters}
\usage{

checkVerbatim(files = .RbcBook1Files(ext=".tex"),
               maxc = 70,
              which = "both",
            verbose = TRUE)

checkRnw(files = .RbcBook1Files(),
       verbose = TRUE, stopOnError=FALSE)

checkPackage(files = .RbcBook1Files(ext=".Rnw"),
           verbose = TRUE)

.RbcBook1Files(ext=".Rnw")
}

\arguments{
  \item{files}{character vector with names (and path) of chapter source
    files}
  \item{ext}{character of length 1. File name extension. \code{.Rnw}
    (the default) and \code{.tex} should be the most important cases.}
  \item{maxc}{integer of length 1. Maximal number of characters in a
    verbatim line. All lines exceeding this limit will be reported in
    the return value of this function.}
  \item{which}{character of length 1. 'Sinput' will look at \code{Sinput}
    environments, 'Soutput' at \code{Soutput} environments, 'both' at
    both.}
  \item{verbose}{Logical.}
  \item{stopOnError}{Logical. If FALSE, first error found will lead to
    stop. If TRUE, try to continue checking.}
}

\details{
  \code{checkVerbatimLines} sees whether all verbatim lines have length
  at most \code{maxc}.

  \code{checkPackage} finds all the occurences of \\Rpackage{...}
  in the text, checks whether the package is known, and returns a named
  list will all occurences of the packages.
  
}

\value{
  For \code{checkVerbatimLines} and \code{checkSetup}, a data frame
  with one row for each offending line and various columns
  describing it.

  For \code{.RbcBook1Files}, a character vector.
}

\author{Wolfgang Huber <huber@ebi.ac.uk>}
\examples{
  f = tempfile()
  zap = function(n) paste(formatC(1:n, width=2), collapse=" ")
  writeLines(c("*begin{Sinput}", zap(20), zap(30), "*end{Sinput}"), con=f)
  checkVerbatim(files=f)
}

\keyword{misc}

bcSweave <- function(f) {
  out  <- sub('.Rnw','.tex',f)
  if(out==f)
    stop("Expecting filename with '.Rnw' extension.")
  Sweave(f, eps=FALSE, output=out)

  ## This is for the sessionInfo stuff
  tmp  <- tempfile()
  Sweave("../sessionInfo.Rnw", output=tmp)
  con <- file(out, open="at")
  writeLines(readLines(tmp), con)
  close(con)
}

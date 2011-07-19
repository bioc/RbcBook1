bcStangle =
function(files = .RbcBook1Files(), outfile= "bioCSpringer.R") {
  con = file(outfile, open="w") 

  chunkfun = function(name, start, end, eva) {
    c("\n", name,
      paste(c("##", "")[1+eva], txt[(start+1):(end-1)]))
  }
  seplin = "##################################################"
  
  for(f in files) {
    ## find chunks
    txt = readLines (f)
    i1 = grep("^<<.*>>=", txt)
    i2 = grep("^@", txt)
    if(length(i1)!=length(i2) || any(i2<=i1)) {
      print(i1)
      print(i2)
      stop(paste(f, "seems to have invalid code chunk syntax."))
    }

    ## which chunks are evaluated?
    args=gsub("^<<|>>=$", "", gsub(" ", "", txt[i1]))
    args=strsplit(args, ",")
    is.evaluated=sapply(args, function(v) {
       i1 = ("eval=FALSE" == v)
       i2 = ("eval=TRUE"  == v)
       i3 = grep("eval", v)
       stopifnot( !(any(i1) && any(i2)),
                 length(setdiff(i3, which(i1|i2)))==0 )
       return(!any(i1))
     }) ## sapply

    ## chunk names
    chunkname=sapply(args, function(v) {
      res = v[-grep("=", v)]
      if(length(res)==0) res=""
      return(res)
    }) ## sapply
    chunkname = paste("## chunk ", 1:length(chunkname), ": ", chunkname, sep="")
      
    ## write
    writeLines(c(seplin, paste("##", txt[grep("chapter{", txt)]), # VJC 2011, extended=FALSE)]),
               seplin), con)
    chks = mapply(chunkfun, chunkname, i1, i2, is.evaluated)
    lapply(chks, writeLines, con=con)
    writeLines(character(2), con)
    
  } ## for f
  close(con)
}

tangleToSingleFiles = function() {
  ifiles = .RbcBook1Files()
  ofiles = sapply(strsplit(ifiles, "/"), function(x) x[2])
  of2 = gsub(".Rnw", ".R", ofiles)
  of3 = paste("Rfiles", of2, sep="/")
  dir.create("Rfiles")
  for( i in 1:length(ifiles))
     bcStangle(ifiles[i], of3[i])
}

## find lines that are longer than 70 characters within Sinput and Soutput
## environments

.RbcBook1Files = function(ext=".Rnw") paste(c(
  "Preproc/overview", "Preproc/AffyPreprocess", "Preproc/AffyQuality",
  "Preproc/TwoColorPre", "Preproc/cellAssays", "Preproc/massSpectra",
  "Metadata/metaOverview", "Metadata/metaQuery", "Metadata/metaOutput", 
  "Metadata/metaVisualize",
  "Analysis/anovv", "Analysis/dist", "Analysis/AnalClust", 
  "Analysis/AvHDS", "Analysis/multtest", 
  "Analysis/MLchap", "Analysis/Computational_Inference",
  "Analysis/bbanal",
  "Graphs/Intro", "Graphs/Graphs", "Graphs/bioCGraph",
  "Graphs/modelingGraphs",
  "CaseStudies/limma", "CaseStudies/class", "CaseStudies/fromcels"), ext, sep="")



checkVerbatim =
  function(files = .RbcBook1Files(ext=".tex"),
            maxc = 70,
            which = "both",
            verbose=TRUE)
{
  rv = NULL
  for (f in files) {
    if(!file.exists(f)) {
      cat(f, "NOT FOUND.\n")
    } else {
      if(verbose)
        cat(f, "")
      txt = readLines (f)
      pat = switch(which,
        Sinput  = c("^.begin.Sinput}",  "^.end.Sinput."), 
        Soutput = c("^.begin.Soutput}", "^.end.Soutput."), 
        both    = c("^.begin.Sinput}|^.begin.Soutput.",
                    "^.end.Sinput}|^.end.Soutput."),
        stop(paste("Unknown value for 'which':", which)))
    
      i1 = grep(pat[1], txt)
      i2 = grep(pat[2], txt)
      if(length(i1)!=length(i2) || any(i2<=i1))
        stop(paste(f, "seems to have unbalanced environment keywords."))
      
      if(length(i1)>=1) {
        for(j in 1:length(i1)) {
          rg = (i1[j]+1):(i2[j]-1)
          nc = nchar(txt[rg])
          wh = (nc > maxc)
          stopifnot(!any(is.na(wh)))
          if(any(wh))
            rv = rbind(rv, data.frame(
              file   = I(rep(f, sum(wh))),
              line   = as.integer(rg[wh]),
              length = as.integer(nc[wh]),
              text   = I(txt[rg[wh]])))
        } ## for j
      } ## if
    } ## else
  } ## for f
  if(verbose) cat("\nFinished.\n\n")
  return(rv)
}

checkRnw =
function(files = .RbcBook1Files(), verbose=TRUE, stopOnError=FALSE) {
  rv = NULL
  for(f in files) {
    if(verbose) cat(f, "")
    txt = readLines (f)
    i1 = grep("^<<.*>>=", txt)
    i2 = grep("^@", txt)
    if(length(i1)!=length(i2) || any(i2<=i1)) {
      if(verbose) cat("\n")
      print(i1)
      print(i2)
      stop(paste(f, "seems to have invalid code chunk syntax."))
    }

    ## are there any lines with '@' at the start that aren't empty otherwise?
    z = nchar(gsub(" ", "", txt[i2]))
    if(!all(z==1))
      rv = rbind(rv, data.frame(
        file = I(f),
        line = as.integer(i2[z>1]),
        text = I("'@' followed by text.")))
    
    ## are there any lines with '\caption' and '%' and more text thereafter?
    i = grep("caption.*%.*[a-zA-Z]", txt, extended=FALSE)
    if(length(i)>0)
      rv = rbind(rv, data.frame(
        file = I(f),
        line = as.integer(i),
        text = I(txt[i])))
    
    if(length(i1)>=1) {
      ## the line numbers of all R code
      codeChunks = unlist(apply(cbind(i1+1, i2-1), 1, function(v) seq(v[1], v[2])))

      ## Check for occurences of keyword 'options'
      wh = grep("options", txt[codeChunks])
      if(length(wh)>0)
        rv = rbind(rv, data.frame(
            file = I(rep(f, length(wh))),
            line = as.integer(codeChunks[wh]),
            text = I(txt[codeChunks[wh]])))

      ## Check for occurences of keyword 'library("RbcBook1")'
      firstChunk = (i1[1]+1):(i2[1]-1)
      lib1 = grep("library..RbcBook1..", txt[firstChunk])
      if (length(lib1)<1) { 
        rv = rbind(rv, data.frame(
          file = I(f),
          line = as.integer(NA),
          text = I('library("RbcBook1") is missing from first code chunk!')))
        if(stopOnError)stop()
      }      
      wh = grep("library..RbcBook1..", txt[setdiff(codeChunks, firstChunk)])
      if (length(wh)>=1) {
        rv = rbind(rv, data.frame(
          file = I(f),
          line = as.integer(codeChunks[wh[1]]),
          text = I('library("RbcBook1") occurs multiple times!')))
        if(stopOnError)stop()
      }

      ## Check for occurences of "=" as assignment
      ex = parse(text=txt[codeChunks])
      cl = sapply(ex, class)
      wh = which(cl=="=")
      if(length(wh>0)) {
        rv = rbind(rv, data.frame(
          file = I(rep(f, length(wh))),
          line = as.integer(NA),
          text = I(as.character(ex[wh]))))
        if(stopOnError)stop()
      }      
    } ## if
  } ## for
  if(verbose) cat("\nFinished.\n\n")
  return(rv)
}

checkPackage = function(files = .RbcBook1Files(ext=".Rnw"), verbose=TRUE) {
  pkgNames <-  library()$results[,"Package"]

  ## additional ones:
  pkgNames <-  c(pkgNames, "iSPlot", "aCGH", "daMA", 
                 "Rggobi", "RMAGEML",  ## Omegahat
                 "gclus", "vcd", "scatterplot3d", "RSvgDevice", ## CRAN
                 "logspline", "mgcv", "cclust", "flexmix", "fpc",
                 "ade4", "ape", "Matrix")

  
  pkg <- vector(mode="list", length=length(pkgNames))
  names(pkg) <- pkgNames
  
  for (i in seq(along=files)) {
    f = files[i]
    if(!file.exists(f)) {
      cat(f, "NOT FOUND.\n")
    } else {
      txt = readLines (f)
      ## the following is to break up into invidual words - otherwise we
      ## would only find first occurence in each line
      txt = unlist(strsplit(txt, "[ ,.]"))
      ## re  = regexpr("Rpackage{.*}", txt, extended=FALSE) 
      ## this is more specific, but somehow I didn't get it to work:
      re  = regexpr("Rpackage.[a-zA-Z0-9]+.", txt)
      hit = (re>0)
      p = substr(txt[hit], start=re[hit], stop=re[hit]+attr(re, "match.length")[hit]-1)
      p = sub("Rpackage{", "", sub("}", "", unique(p), extended=FALSE), extended=FALSE)
      mt   = match(p, names(pkg))
       if(any(is.na(mt))) {
        cat(paste(f, ": invalid package name:", p[is.na(mt)], "\n"))
        ## browser()
     } else {
        if(verbose)
          cat(f, "OK.\n")
      }
      for(j in which(!is.na(mt)))
        pkg[[mt[j]]] = append( pkg[[mt[j]]], f)
    }
  }
  pkg <- pkg[sapply(pkg, length) > 0]
  pkg <- pkg[order(names(pkg))]
  return(pkg)
}

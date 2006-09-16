imageSize <- function(dirs= c("Preproc", "Analysis", "Metadata", "Graphs", "CaseStudies"),
                      ext = c("pdf", "png")) {
  f = lapply(dirs, dir, pattern=paste("", paste(ext, collapse="|"), "$", sep=""), recursive=TRUE)
  for(i in seq(along=f))
    f[[i]] = file.path(dirs[i], f[[i]])
  f = unlist(f)
  fi = file.info(f)
  fi = fi[order(fi$size, decreasing=TRUE), ]
  data.frame(name=I(rownames(fi)), size=fi$size)
}
  

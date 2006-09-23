
.RbcBook1.pkgs = function()
  c("factDesign", "estrogen", "ALL", "ALLMLL", "prada", "facsDorit", "RBGL", "GOstats",
    "vsn", "graph", "Rgraphviz",
     "yeastExpData", "KEGG", "KEGGSOAP", "hgu133a", "hgu95av2", "hgu95av2cdf",
     "CoCiteStats", "arrayMagic", "hgu133acdf", "hgu133bcdf",
     "cMAP", "affydata", "PROcess", "e1071", "MLInterfaces", "kidpack", "hopach", "xtable",
     "SpikeInSubset", "exactRankTests", "LogitBoost", "statmod", "sma", 
      "limma", "randomForest",
      "ProData", "ipred", "gbm", "gpls", "pamr", "mlbench", "edd", 
      "golubEsets", "YEAST", "hgu95av2probe", "hu6800cdf", "mclust",
      "matchprobes", "hu6800probe", "hexbin", "XML", "annaffy", "humanLLMappings",
      "affyPLM", "multtest", "affypdnn", "gcrma", "affycomp", "arrayQuality",
    "convert", "beta7", "bioDist")

## install.from.svn = function(madman="../../../madman/Rpacks") {
##  pkgs <- .RbcBook1.pkgs 
##  ... bla...bla
## }


require.RbcBook1 = function() {
  pkgs = .RbcBook1.pkgs()
  res = sapply(pkgs, require, character.only=TRUE)
  if(any(!res))
    stop(paste("There were error(s) loading the following package(s):\n", pkgs[!res]))
  return(search())
}

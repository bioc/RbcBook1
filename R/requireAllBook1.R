
.RbcBook1.pkgs = function() c(
  "affycomp", "affydata", "affypdnn", "affyPLM", "ALL", "ALLMLL",
"AnnBuilder", "AmpAffyExample", "annaffy", "arrayQuality",
"beta7", "bioDist", "cMAP", "CoCiteStats", "convert",
"e1071", "edd", "estrogen", "exactRankTests", "facsDorit",
"factDesign", "gbm", "gcrma", "golubEsets", "GOstats", "gpls",
"graph", "hexbin", "hgu133a", "hgu133acdf", "hgu133bcdf", "hgu95av2",
"hgu95av2cdf", "hgu95av2probe", "hopach", "hu6800cdf", "hu6800probe",
"humanLLMappings", "ipred", "KEGG", "KEGGSOAP", "kidpack", "limma", "locfit", 
"LogitBoost", "matchprobes", "mclust", "mlbench", "MLInterfaces",
"multtest", "pamr", "prada", "PROcess", "ProData", "randomForest",
"RBGL", "Rgraphviz", "rrcov", "simpleaffy", "sma", "SpikeInSubset", "statmod",
"vsn", "XML", "xtable", "YEAST", "yeastExpData")

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

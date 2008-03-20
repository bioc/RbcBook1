
 .fixdu <- function(x,suff=" ") {
#
# makes char tokens unique by adding whitespace or another suffix as needed
#
  du <- duplicated(x)
  if (sum(du) == 0) return(x)
  x[du] <- paste(x[du], suff, sep="")
  Recall(x,suff)
 }

rpart2gNEL <- function(tr, remap=function(x)x, nsep="\n") {
#
# tr is an rpart output.  Because rpart may fiddle with
# variable names, remap allows mapping from rpart labels
# to other symbols.  nsep is the separator between vote and (a:b)
# vote count tally token
#
# node takes the form  [voteToken][nsep](a:b), it is a string
# such strings might recur in the translation of an rpart and
# we would then have redundant node names.  fixdu pads with spaces
# so that we have unique strings
#
# how do we derive graph topology from rpart?  see below
#
 require(graph)
 fixdu <- function(x,suff=" ") {
  du <- duplicated(x)
  if (sum(du) == 0) return(x)
  x[du] <- paste(x[du], suff, sep="")
  Recall(x,suff)
 }
 f <- tr$frame
 ylev <- attr(tr,"ylevels")
 yval <- f[["yval"]]
 vote <- ylev[yval]
#
# here we construct the nodes based on split results
#
 r0 <- function(x) round(as.numeric(x),0)
 an <- function(x) as.numeric(x)
 #nds <- fixdu(paste("N=",as.character(f$n),
 nds <- fixdu(paste( "(", as.character(r0(an(f$yval2[,2]))),
         ":", as.character(r0(an(f$yval2[,3]))), ")",sep=""))
 nds <- paste(vote, nds, sep=nsep) 
 G <- new("graphNEL", nodes=nds)
 edgemode(G) <- "directed"
#
# now we start to derive graph topology
# a binary enumeration is used in rpart
# if node has number n, its children are numbered 2n, 2n+1
#
 pos <- as.numeric(row.names(f))
 parent <- floor(pos/2)
#
 names(nds) <- pos
#
# so now nds is a named vector with binary enumeration as names
# we will have children pointing to parents
#
 G <- addEdge(
    to <- nds[as.character(parent)[parent>0]], 
    fr <- nds[parent>0], 
    G, 1)
 nl <- remap(labels(tr)[-1])
 names(nl) <- paste(to, fr, sep="~")
 attr(G@edgeData@data,"edgeLabels") <- as.list(nl)
 G
}
 
.plotAsGraph <- function(x,remap=function(x)x,...) {
# defunct -- it would be nice to use Rgraphviz at high level
# but not ready yet
 tmp <- rpart2gNEL(x,remap=remap)
 plot(tmp, edgeAttrs=list(label=attr(tmp@edgeData@data,"edgeLabels")),...)
}

grabSplitV <- function(g,use="%") {
 gl <- labels(g)
 fx <- gsub(">|<", use, gl)
 fxs <- strsplit(fx, use)
 sapply(fxs, function(x)x[1])
}

remapAff <- function(x, env=hgu95av2SYMBOL, use="%" ) {
 # suppose rpart has taken affy 1071_at to X1071_at >= 4.4
 # this replaces X1071_at with it symbol for 1071_at
 fx <- gsub(">|<", use, x)
 fxs <- strsplit(fx, use)
 tags <- unlist(mget(substr(sapply(fxs, function(x)x[1]), 2, 1000), env))
 tags
}

 

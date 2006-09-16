library("RbcBook1")
library("Biobase")
data("sessi")

for(p in sessi$otherPkgs){
  pkgname = p$Package
  d = packageDescription(pkgname)
  if(all(is.na(d))) {
    cat(paste("'", pkgname, "' cannot be found.\n", sep=""))
  } else {
    vHave = package.version(pkgname)
    vWant = package_version(p$Version)
    if(vHave != vWant) {
      what = ifelse(vHave > vWant, "GREATER", "LESS")
      cat(paste("Your version of '", pkgname, "': ", vHave,
                " is ", what, " than the one in 'sessi': ", vWant, 
                ".\n", sep=""))
    }
  }
}

for(p in sessi$otherPkgs){
  if(!require(p$Package, character.only=TRUE))
    stop(paste(p$Package, "cannot be loaded."))
}

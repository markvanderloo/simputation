#!/usr/bin/Rscript

suppressPackageStartupMessages({
  if (!require("drat")) stop("drat not installed")
  if (!require("docopt")) stop("docopt not installed")
})

 
"Usage: drat.sh [nocommit] [nobadge] [--pkg FILE] [--dratrepo FOLDER] 

nocommit commit after insert? 
nobadge update drat badge?
--pkg FILE The tarball to insert in the drat repo (by default the tarball in ./output)
--dratrepo FOLDER path to root of drat repo [default: ../drat]
" -> doc

opt <- docopt(doc)

stopifnot(file.exists(opt$dratrepo))

pkg <- opt$pkg
if ( is.null(pkg) ){
  pkg <- dir("output/",pattern = ".*tar\\.gz",full.names = TRUE)
} 

if (!file.exists(pkg)){
  stop(sprintf("%s not found",pkg))
}

drat::insertPackage(pkg, repodir=opt$dratrepo, pullfirst=TRUE, commit=!opt$nocommit)

if (!opt$nobadge){
  readme <- paste(readLines("README.md"),collapse="\n")
  ver <- read.dcf('pkg/DESCRIPTION')[1,"Version"]
  readme <- sub("drat-.*?-green",paste0("drat-",ver,"-green"),readme)
  write(readme, "README.md")
}

cat(sprintf("Inserted %s into %s %s\n"
  , pkg
  , opt$dratrepo
  , if(!opt$nocommit) "and committed" else ""
))

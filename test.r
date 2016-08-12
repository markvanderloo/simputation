#!/usr/bin/Rscript

suppressPackageStartupMessages({
  if (!require("docopt")) stop("docopt not installed")
})

"Usage: test.r [nocovr]

nocovr Skip measuring test coverage.
" -> doc

opt <- docopt(doc)

if(!require(devtools)) stop('devtools not installed first')
devtools::test('pkg')

if (!opt$nocovr){
  if(require(covr)){ 
    covr::package_coverage('pkg')
  } else {
    stop("covr not installed")
  }
}

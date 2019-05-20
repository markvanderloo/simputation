
[![Build Status](https://travis-ci.org/markvanderloo/simputation.svg?branch=master)](https://travis-ci.org/markvanderloo/simputation)
[![Coverage Status](https://coveralls.io/repos/github/markvanderloo/simputation/badge.svg?branch=master)](https://coveralls.io/github/markvanderloo/simputation?branch=master)
[![drat version](https://img.shields.io/badge/drat-0.2.2-green.svg)]()
[![CRAN](http://www.r-pkg.org/badges/version/simputation)](https://CRAN.R-project.org/package=simputation)[![status](https://tinyverse.netlify.com/badge/simputation)](https://CRAN.R-project.org/package=simputation)
[![Downloads](http://cranlogs.r-pkg.org/badges/simputation)](https://CRAN.R-project.org/package=simputation)[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)


# simputation
An R package to make imputation simple. Currently supported methods include

- Model based (optionally add [non-]parametric random residual)
    - linear regression 
    - robust linear regression (M-estimation)
    - ridge/elasticnet/lasso regression (from version >= 0.2.1)
    - CART models
    - Random forest
- Model based, multivariate
    - Imputation based on EM-estimated parameters (from version >= 0.2.1)
    - [missForest](https://CRAN.R-project.org/package=missForest) (from version >= 0.2.1)
- Donor imputation (including various donor pool specifications)
  - k-nearest neigbour (based on [gower](https://cran.r-project.org/package=gower)'s distance)
  - sequential hotdeck (LOCF, NOCB)
  - random hotdeck
  - Predictive mean matching
- Other
  - (groupwise) median imputation (optional random residual)
  - Proxy imputation (copy from other variable) 


### Installation

To install simputation and all packages needed to support various imputation
models do the following.
```r
install.packages("simputation", dependencies=TRUE)
```


### Example usage

Create some data suffering from missings
```r
library(simputation) # current package
library(magrittr)    # for the %>% not-a-pipe operator
dat <- iris
# empty a few fields
dat[1:3,1] <- dat[3:7,2] <- dat[8:10,5] <- NA
head(dat,10)
```
Now impute `Sepal.Length` and `Sepal.Width` by regression on `Petal.Length` and `Species`, and impute `Species` using a CART model, that uses all other variables (including the imputed variables in this case).
```r
dat %>% 
  impute_lm(Sepal.Length + Sepal.Width ~ Petal.Length + Species) %>%
  impute_cart(Species ~ .) %>% # use all variables except 'Species' as predictor
  head(10)
```

### Materials

- A [blogpost](http://www.markvanderloo.eu/yaRb/2016/09/13/announcing-the-simputation-package-make-imputation-simple/) introducing the package
- The introductory [vignette](https://cran.r-project.org/web/packages/simputation/vignettes/intro.html)


Beta versions can be installed from my [drat](http://cran.r-project.org/package=drat) repo. If you use the OS whose name shall not be spoken, first install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
```r
if(!require(drat)) install.packages("drat")
drat::addRepo("markvanderloo")
install.packages("simputation",type="source")
```


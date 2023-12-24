

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

To install the development version.

```{bash}
git clone https://github.com/markvanderloo/simputation
make install
```


### Example usage

Create some data suffering from missings
```r
library(simputation) # current package

dat <- iris
# empty a few fields
dat[1:3,1] <- dat[3:7,2] <- dat[8:10,5] <- NA
head(dat,10)
```
Now impute `Sepal.Length` and `Sepal.Width` by regression on `Petal.Length` and `Species`, and impute `Species` using a CART model, that uses all other variables (including the imputed variables in this case).
```r
dat |>
  impute_lm(Sepal.Length + Sepal.Width ~ Petal.Length + Species) |>
  impute_cart(Species ~ .) |> # use all variables except 'Species' as predictor
  head(10)
```

### Materials

- The introductory [vignette](https://cran.r-project.org/web/packages/simputation/vignettes/intro.html)
- [slides](https://markvanderloo.eu/files/share/loo2017easy.pdf) from my [useR2017](https://user2017.brussels/) talk.


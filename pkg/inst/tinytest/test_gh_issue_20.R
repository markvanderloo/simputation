dat <- iris[1:15,]
dat[8,1] <- NA
out <- impute_pmm(dat, Sepal.Length ~ Sepal.Width)
expect_equal(out[8,1],4.6)

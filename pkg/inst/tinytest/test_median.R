
## Median imputation

x <- sample(1:150) 

data(iris)
iris2 <- iris[x, ]
iris2[1,"Sepal.Length"] <- iris2[1:3, "Sepal.Width"] <- NA

out <- simputation::impute_median(iris2, . ~ Species)

m.Sepal.Length <- tapply(iris2$Sepal.Length, iris2$Species, median, na.rm=TRUE)

expect_equivalent(out[1,"Sepal.Length"], m.Sepal.Length[out[1,"Species"]])

m.Sepal.Width <- tapply(iris2$Sepal.Width, iris2$Species, median, na.rm=TRUE)

expect_equivalent(out[1:3,"Sepal.Width"], as.vector(m.Sepal.Width[out[1:3,"Species"]]))

  

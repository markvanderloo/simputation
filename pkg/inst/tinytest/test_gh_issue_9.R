d <- data.frame(x=c(NA,'a','b'),stringsAsFactors = FALSE)
expect_silent(out <- impute_proxy(d,x ~ "w"))
expect_equal(out[1,1],"w")


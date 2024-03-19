df <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6), d = c(7, 8))

# Check group extraction from formula.
expect_equal(simputation:::groups(df, a ~ b | c), c("c"))
expect_equal(simputation:::groups(df, a ~ b | c + d), c("c", "d"))

# Check dplyr group extraction.
grouped_df <- dplyr::group_by(df, d)
expect_equal(simputation:::groups(grouped_df, a ~ b), c("d"))
expect_equal(simputation:::groups(grouped_df, a ~ b | c), c("d", "c"))

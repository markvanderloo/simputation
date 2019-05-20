
## Median imputation

# sample(1:150). Put in literally b/c of RNG change in R 3.60
x <- c(77L, 131L, 15L, 125L, 116L, 132L, 85L, 46L, 55L, 127L, 21L, 
3L, 90L, 87L, 49L, 47L, 139L, 112L, 56L, 4L, 58L, 52L, 101L, 
65L, 111L, 128L, 86L, 144L, 121L, 91L, 75L, 44L, 146L, 11L, 6L, 
48L, 100L, 57L, 118L, 84L, 104L, 54L, 97L, 110L, 117L, 120L, 
109L, 123L, 103L, 81L, 9L, 107L, 18L, 31L, 72L, 137L, 133L, 106L, 
51L, 25L, 2L, 39L, 32L, 40L, 59L, 35L, 93L, 150L, 138L, 70L, 
148L, 142L, 99L, 41L, 61L, 60L, 141L, 82L, 114L, 136L, 69L, 66L, 
108L, 115L, 63L, 122L, 17L, 62L, 33L, 130L, 95L, 19L, 64L, 20L, 
53L, 23L, 113L, 67L, 71L, 68L, 5L, 27L, 147L, 1L, 29L, 34L, 36L, 
78L, 140L, 38L, 24L, 45L, 43L, 134L, 83L, 89L, 80L, 30L, 10L, 
13L, 28L, 16L, 126L, 149L, 143L, 102L, 92L, 124L, 26L, 7L, 98L, 
135L, 129L, 50L, 14L, 22L, 94L, 119L, 73L, 88L, 79L, 37L, 145L, 
96L, 76L, 12L, 42L, 105L, 74L, 8L)

data(iris)
iris2 <- iris[x, ]
iris2[1,1] <- iris2[1:3, 2] <- NA

out <- simputation::impute_median(iris2, . ~ Species)
cat(out[1:3,2],"\n")
#expect_equal(out[1:3,2], c(2.8,2.8,4.0))
  

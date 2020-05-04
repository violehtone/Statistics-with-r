library(stats)

#5 random draws from normal dist. with mean of 7 & sd 0.6

a <- rnorm(n=5, mean=7, sd=0.6)
hist(a)

b <- rnorm(10000, mean=7, sd=0.6)
hist(b)

hist(rnorm(10000, mean=7, sd=0.6), col='grey', nclass=50, freq=FALSE)
curve(dnorm(x, mean=7, sd=0.6), from=3, to=11, n=500, col='red', lwd=2, add=TRUE)

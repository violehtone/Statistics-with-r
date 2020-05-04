#just set some seed
set.seed(23)
sample(x = 1:10, size = 5)


set.seed(726)
a <- sample(1:100,2)
set.seed(726)
b <- sample(1:100,2)
identical(a,b)

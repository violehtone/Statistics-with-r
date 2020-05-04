#Exercise 1
fac_loop <- function(n) {
  result <- 1
  for (i in 1:n) {
    result <- result * i
  }
  return(result)
}

fac_rec <- function(n) {
  if(n <= 0) {
    return(1)
  }else {
    return(n*fac_rec(n-1))
  }
}

fac_vec <- function(n) {
  return(prod(seq(n)))
}

fac_loop(50)
fac_rec(50)
fac_vec(50)

N <- 10000
sapply(c('fac_loop', 'fac_rec', 'fac_vec'),
       function(x) {
         system.time(for (i in 1:N) {
           do.call(x, list(50))
         })
       })

# Exercise2

greece <- function(n) {
  numbers <- c(1:n)
  x <- 2
  for (i in x:trunc(sqrt(n))) {
    
  }
  
  numbers[!numbers %% x]
  
}

























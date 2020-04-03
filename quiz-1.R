#1. Create vector a
a <- runif(n=runif(n=1,min=20,max=100), min = 1, max = 10)

#2. Length of a
length(a)

#3. Difference between consecutive elements
diff(a)

#4.
b <- a[a>5]

#5
c <- a[seq(1,length(a),2)]

#6
every_nth_element <- function(a,n) {
  # a = any vector, n = nth element
  result = a[seq(1,length(a),n)]
  return(result)
}

every_nth_element(a,5)


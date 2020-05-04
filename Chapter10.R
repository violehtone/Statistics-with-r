#sapply
mylist <- list(
  'animals' = c('dogs','cats'),
  'food' = c('tomatoes','cheese','eggs','cucumbers')
)

lengths <- sapply(mylist, length)
lengths


#apply
d <- data.frame(name=c('John','Jack','Jill','Jason','Julia'), 
                exam1=c(7,6,8,9,5), exam2=c(6,7,7,7,4))

result1 <- apply(X = d[c('exam1', 'exam2')],
      MARGIN = 1, #rows
      FUN = mean)

result2 <- apply(X = d[c('exam1', 'exam2')],
      MARGIN = 2, #columns
      FUN = mean)

class(result1)

passfunction <- function(scores) {
  #scores is an numerical vector of scores
  if(mean(scores) >= 5.5) {
    return("pass")
  }else{
    return("fail")
  }
}

apply(X =d[c('exam1','exam2')],
      MARGIN=1,
      FUN = passfunction)


passfunction <- function(scores, limit = 5.5) {
  #scores is an numerical vector of scores
  if(mean(scores) >= limit) {
    return("pass")
  }else{
    return("fail")
  }
}

apply(X =d[c('exam1','exam2')],
      MARGIN=1,
      FUN = passfunction,
      limit = 7.0)

d[d$name == 'Jill', 'exam1'] <- NA


apply(d[,2:3],1,mean)
apply(X = d[c('exam1','exam2')],
      MARGIN = 1,
      FUN = mean,
      na.rm=TRUE)



## tapply()
d <- data.frame(name=c('John','Jack','Jill','Jason','Julia'),
                gender=c('male','male','female','male','female'),
                exam1=c(7,6,8,9,5), exam2=c(4,5,5,7,3))
d

tapply(X = d$exam1,
       INDEX = d$gender,
       FUN = mean)


## lapply() and sapply()
mylist <- list(v1=runif(12),v2=runif(100),v3=runif(23))
mylist

lapply(mylist, mean)
sapply(mylist, mean)



## 10.3 - purrr package
library(tidyverse)



## Exercises
#1
a <- matrix(1:100,ncol=10)
colavg <- apply(X = a,
                MARGIN = 2,
                FUN = mean)
colavg                

#2
d <- data.frame(name=c('John','Jack','Jill','Jason','Julia'),
                gender=c('male','male','female','male','female'),
                exam1=c(7,6,8,9,5), exam2=c(4,5,5,7,3))

d$hair <- factor(c('brunette','blonde','blonde','brunette','blonde'))

tapply(X = d$exam1,
       INDEX = list(d$gender, d$hair),
       FUN = mean)


#3 -4 
a <- c(0:10, NA, 11:20)
mean(a, na.rm = TRUE)

u <- data.frame(var1=rnorm(100,0,1), var2=rnorm(100,10,1))
u$var1[runif(length(u$var1))>0.90] <- NA
u$var2[runif(length(u$var2))>0.90] <- NA

apply(X = u[c("var1", "var2")],
      MARGIN = 1, #row
      FUN = mean,
      na.rm = TRUE)

apply(X = u[c("var1", "var2")],
      MARGIN = 2, #row
      FUN = mean,
      na.rm = TRUE)

apply(X = u[c("var1", "var2")],
      MARGIN = 2, #row
      FUN = sd,
      na.rm = TRUE)


#6
#avg. scores over both exams for males and females
tapply(X = apply(d[c("exam1", "exam2")],
                 MARGIN = 1,
                 FUN = mean),
       INDEX = d$gender,
       FUN = mean)

#7
flintstones <- list(person1=
                      list(name='Fred', kids=c('Pebbles'), married=TRUE,
                           pets=c('Dino','Baby Puss','Doozy')),
                    person2=
                      list(name='Barney', kids='Bamm-Bamm', married=TRUE,
                           pets='Hoppy'))


lapply(X = flintstones,
       FUN = "[[",
       ... = "name")

sapply(X = flintstones,
       FUN = "[[",
       ... ="name")

#8
result <- lapply(X = flintstones,
                 FUN = "[[",
                 ... = "name")

class(result)
unlist(result)

#9
#avg. # of pets in flintstones
mean(sapply(X = lapply(X = flintstones,
                       FUN = "[[",
                       ... = "pets"),
            FUN = length))


#First extract the “pets” elements in a new list,
pets <- lapply(flintstones, "[[", "pets")

#Then apply the length()

pets_lengths <- sapply(pets, length)
#calculate mean
mean(pets_lengths)


#10
v <- runif(10)

helper <- function(i) {
  v_new = v[-i]
  return(c(mean(v_new), sd(v_new)))
}


t(sapply(X = 1:10,
       FUN = helper))


#11
Rho_W <- function(T=20) {
  if (T >= -5 && T <= 50) {
    return(999.842594 + 0.06793952 * T - 0.00909529 * T^2 + 0.0001001685 * T^3 - 
             1.120083e-06 * T^4 + 6.536332e-09 * T^5)
  } else {
    return(NA)
  }
}

# A permutation function
#1
permute <- function(alphabet, length) {
  #alphabet = set of symbols
  #length = single integer
  output <- matrix(nrow = length(alphabet) * length,
                   ncol = length)
  
  
  i <- length - 1
  r <- matrix()
  whike(i>0) {
    i <- i-1
    r <- cbind()
  }
  
  
  
  for (symbol in alphabet) {
    for (i in 1:length) {
      
    }
  }
}

alphabet <- c('a', 'b', 'c')
permute(alphabet, 2)



#2
#3













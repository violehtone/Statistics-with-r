## 9.1
Circlesurface <- function(radius) {
  return(pi*radius^2)
}

Circlesurface(10)

Sphere <- function(radius) {
  v <- 4/3*pi*radius^3
  s <- 4*pi*radius^2
  return(list(volume=v, surface=s))
}

Sphere(6371)$volume

Rho_W <- function(T=20) {
  999.842594 + 0.06793952 * T - 0.00909529 * T^2 +  0.0001001685 * T^3 - 
    1.120083e-06 * T^4 +  6.536332e-09 * T^5
}

Rho_W()

## 9.2

Rho_W <- function(T=20) {
  if (T >= -5 && T <= 50) {
    return(999.842594 + 0.06793952 * T - 0.00909529 * T^2 + 0.0001001685 * T^3 - 
      1.120083e-06 * T^4 + 6.536332e-09 * T^5)
  }else{
    return(NA)
  }
}

a <- c(0,2,5,2,3)
ifelse(a<3, 0,1)


for (i in c('one', 'two', 'three')) {
  print(paste('Loop number', i))
}

i <- 0
cube <- 0
while (cube < 100) {
  print(paste("The cube of", i, "is smaller than 100"))
  i <- i+1
  cube <- i^3
}

####### EXERCISES 9.4 ###############
#1
Sphere <- function(radius) {
  c <- 2*pi*radius
  v <- 4/3*pi*radius^3
  s <- 4*pi*radius^2
  return(list(volume=v, surface=s, circumference = c))
}

Sphere(6371)$circumference

#2
temp <- 20.0
san <- 35.0

SatOx <- function(T=20, S=35) {
  T <- T + 273.15
  A <- -173.9894 + (25559.07/T) + 146.4813*log(T/100)-22.204*(T/100) + S*(-0.037362 + 0.016504*(T/100)-0.0020564*(T/100)^2)
  return(exp(A))
}

SatOx(temp,san)
SatOx(seq(0,30, by=5))



### LOOPS ###
#1 column avg.
a <- matrix(1:100, ncol=10)
avgs <- rep(x = 0, times = 10)

for(i in c(1:ncol(a))) {
  colavg <- mean(a[,i])
  avgs[i] <- colavg
}

avgs

#2. fibonacci
fibovec <- as.vector(rep(x = 0, times = 50))

fibovec

for(i in c(1:50)) {
  if(i == 1) {
    fibovec[i] = 1
  }else if(i == 2) {
    fibovec[i] = 1
  }else{
    fibovec[i] = fibovec[i-1] + fibovec[i-2]
  }
}

fibovec[50] / fibovec[49]
(1+sqrt(5))/2

ratio <- fibovec[2:50] / fibovec[1:49]
which(abs(ratio - (1+sqrt(5))/2) < 1e-6)






























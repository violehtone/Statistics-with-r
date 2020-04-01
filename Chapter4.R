#Title: worm species densities in deep-sea
#Purpose:
#Name: Ville Lehtonen
#R-version: 3.6

setwd('/home/villelehtonen/Desktop/statw-r')
Nemaspec <- read.csv("data/nemaspec.csv", header=TRUE, row.names = 1)
head(Nemaspec)

#1
dens <- Nemaspec$M160b
#2
dens <- dens[dens != 0] #remove zeros
#3
N <- sum(dens)
#4
p <- dens/N
sum(p)
#5
S <- length(p)
#6
N1 <- -sum(p * log(p))
N2 <- 1/sum(p^2)
N3 <- 1/max(p)
#7
n = 100
N = 699
ES <- sum(1-choose(N-dens,100)/choose(N,n))
#8
diversity <- c(N,S,N1,N2,N3,ES)
name_v <- c('N','S','N1','N2','N3','ES') 
names(diversity) <- name_v
diversity

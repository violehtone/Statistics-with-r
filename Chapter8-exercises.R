library(tidyverse)

### Exercises
#1
curve(expr = x^3 * (sin(3*pi*x))^2,
      from = -2,
      to = 2,
      col = "blue",
      xlab = "x",
      ylab = "f(x)",
      main = "curve",
      n = 200)
      
#2
f <- function(x) {
  1/(cos(1+x^2))
  }

curve(expr = f,
      from = -5,
      to = 5,
      xlab = "x",
      ylab = "f(x)",
      n = 200)

### Human population growth
#1
a <- 0.02
K <- 450
N0 <- 76.1
x0 <- 1900

f <- function(x) {
  K / (1 + (K-N0 / N0) * exp(-a*(x - x0)))
}

curve(expr = f,
      from = 1900,
      to = 1980,
      lwd = 2)

#2
df <- data.frame(year=c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980),
                 population=c(76.1, 92.4, 106.5, 123.1, 132.6, 152.3, 180.7, 204.9, 226.5))

plot(population ~ year,
     data = df)

curve(expr = f,
      add = TRUE,
      from = 1900,
      to = 1980,
      xlab = "x",
      ylab = "f(x)",
      main = "curve",
      lwd = 2)

### Toxic ammonia
#1
Kn <- 8*10^-10

f <- function(x) {
  Kn / (Kn + 10^(-x))
}

curve(expr = f,
      from = 5,
      to = 11)

#2
KN0 <- 8*10^-11
f0 <- function(x) {
  KN0 / (KN0 + 10^(-x))
}

curve(expr = f0,
      add = TRUE)

### Iris data set
#1
head(iris)
class(iris)

#2
dim(iris)

#3
ggplot(data = iris,
       mapping = aes(x = Petal.Length,
                     y = Petal.Width)) +
  geom_point() +
  labs(x = "Petal length",
       y = "Petal Width") +
  ggtitle("Petal length vs. width")
  

# 4-5
ggplot(data = iris,
       mapping = aes(x = Petal.Length,
                     y = Petal.Width,
                     fill = Species)) +
  geom_point(mapping = aes(color = Species,
                           shape = Species)) +
  labs(x = "Petal length",
       y = "Petal Width") +
  ggtitle("Petal length vs. width")

# 6
#boxplot
boxplot(Sepal.Length ~ Species,
        data = iris,
        col = 'grey')

# 7
par(mfrow=c(2,2))
boxplot(Sepal.Length ~ Species, data = iris, col = 'grey')
boxplot(Sepal.Width ~ Species, data = iris, col = 'grey')
boxplot(Petal.Length ~ Species, data = iris, col = 'grey')
boxplot(Petal.Width ~ Species, data = iris, col = 'grey')




### Automatic coercien by plot()
d <- data.frame(nr = factor(c('1',2:20)),
                value = c(1:20)^2)

head(d)
dim(d)

#1
print(d)

#2
par(mfrow=c(1,1))
plot(d$nr, d$value)

#3
d$nr <- as.character(d$nr)
d$nr <- as.numeric(d$nr)
plot(d$nr, d$value)














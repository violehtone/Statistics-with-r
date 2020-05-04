plot(iris$Petal.Width ~ iris$Petal.Length)

plot(data = iris,
     Petal.Width ~ Petal.Length)

plot(data = iris,
     log(Petal.Width) ~ log(Petal.Length))


baseurl = 'http://www.few.vu.nl/~molenaar/courses/statR/'
f <- file.path(baseurl, 'data', 'formulas', 'scatter.txt')
d <- read.table(f,
                sep = "\t",
                header=T)

plot(data = d,
     x ~ y,
     xlab = 'Explanatory variable',
     ylab = 'Response')


#linear model
linfit <- lm(y~x, data =d)
linfit

lines(d$x, predict(linfit), col = 'red')

par(mfrow=c(4,1))
plot(linfit)


## second order
linfit2 <- lm(y~x+I(x^2),
              data=d)

par(mfrow=c(1,1))
plot(data = d,
     x~y)
lines(d$x, predict(linfit2), col = 'blue')

linfit2


## Data with two discrete explanatory variables (factors)
f <- file.path(baseurl, 'data', 'formulas', 'generegulation.txt')
d <- read.table(f,
                sep = "\t",
                header=T)
head(d)

levels(d$mutation)
levels(d$temp)

d$mutation <- relevel(d$mutation, ref='wt')
d$temp <- relevel(d$temp, ref="low")
levels(d$mutation)

d <- unite(d, mut_temp, c(mutation, temp), remove=F)

#boxplot mut_temp vs. response
ggplot(data = d) +
  geom_boxplot(mapping = aes(x = reorder(mut_temp, desc(mut_temp)),
                             y = response),
               color = 'black',
               fill = 'grey') +
  labs(x = "Mutation : Temp")

#Exercises
#1 - model with only mutation as explanatory variable
model1 <- lm(response ~ mutation, data = d)
#2 - model with only temperature as explanatory variable
model2 <- lm(response ~ temp, data = d)
#3 - model with both mutation and temperature as explanatory variables
model3 <- lm(response ~ temp + mutation, data = d)
#4 - model with both mutation and temperature and their “interaction” as explanatory variables
model4 <- lm(response ~ temp + mutation + mutation:temp, data = d)

#diagnostic plot
par(mfrow=c(4,1))
plot(model1)
plot(model2)
plot(model3)
plot(model4)

#coefficients
coef(model1)
coef(model2)

coef(model3)
coef(model4)

#predictions
predict(model1, newdata = d)

head(iris)

#create x-y plot
plot(x=iris$Petal.Length,
     y=iris$Sepal.Length,
     pch=(15:17)[iris$Species],
     xlab="petal length (cm)",
     ylab="sepal length (cm)",
     asp = 1,
     col=c('red','green','blue')[iris$Species]
     )
#add lines to the figure
lines(x=c(1,7),
      y=c(4.31+0.41*1, 4.31+0.41*7),
      lwd=2,
      col='red')
#Add legend
legend(1.2,
       8,
       pch = 15:17,
       col=c('red','green','blue'),
       legend = levels(iris$Species))


###Histograms

#Basic histogram
hist(iris$Petal.Length, xlab="Petal length (cm)")

#two histograms
layout(matrix(1:2,nrow=1))
hist(iris$Petal.Length, xlab="Petal length (cm)",
     col="lightblue",
     main="",
     probability = TRUE)
hist(iris$Petal.Length,
     xlab="Petal length (cm)", 
     density=10, 
     main="",
     probability=FALSE)

### Boxplots
par(mfrow=c(1,1))

#Plot the petal length as a function of the variable Species
#-> Petal.Length~Species
boxplot(Petal.Length ~ Species,
        data = iris,
        col='grey',
        xlab = 'Iris species',
        ylab = 'Petal lenght')

#make logarithmic transformation
boxplot(log(Petal.Length) ~ Species,
        data = iris,
        col='grey',
        xlab = 'Iris species',
        ylab = 'log(Petal lenght)')


### Plotting mathematical functions

par(mfrow=c(1,1))
#plot curve y = sin(3*pi*x)
curve(sin(3*pi*x))

#Create a nicer looking plot for the curve
curve(sin(3*pi*x),
      from = 0,
      to = 2,
      col = "blue",
      xlab = "x",
      ylab = "f(x)",
      main = "curve")

#Add another curve to the plot
curve(cos(3*pi*x), 
      add = TRUE, 
      col="red",
      lty=2)

#Add the x-axis, line at y=0, and a legend
abline(h=0, lty=2)
legend("bottomleft", c("sin","cos"), text.col = c("blue","red"),lty=1:2)


library(tidyverse)
### GGPLOT2
p <- ggplot(data=iris,
            mapping=aes(x = Petal.Length,
                        y = Sepal.Length,
                        colour = Species,
                        shape = Species)) +
  #add geometric element
  geom_point(size = 3) +
  #add axis labels
  labs(x = 'Petal length (cm)', y = 'Sepal length (cm)') +
  #add fitted line through data
  stat_smooth(method=lm,
              se=FALSE,
              colour = 'red',
              inherit.aes = FALSE,
              mapping=aes(x = Petal.Length,
                          y = Sepal.Length))

print(p)

# method 2
p <- ggplot(data=iris,
            mapping=aes(x = Petal.Length,
                        y = Sepal.Length)) +
  #add geometric element
  geom_point(size = 3, mapping=aes(color = Species,
                                   shape = Species)) +
  #add axis labels
  labs(x = 'Petal length (cm)', y = 'Sepal length (cm)') +
  #add fitted line through data
  stat_smooth(method=lm,
              se=FALSE,
              colour = 'red')

print(p)


ggplot(data=iris,
       mapping=aes(x = Petal.Length,
                   y = Sepal.Length)) +
  #add geometric element
  geom_point(size = 3, 
             mapping=aes(color = Species,
                         shape = Species)) +
  #add axis labels
  labs(x = 'Petal length (cm)', y = 'Sepal length (cm)') +
  #add fitted line through data
  stat_smooth(method=lm,
              se=FALSE,
              colour = 'red') +
  #add legends
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0,1))

print(p)


## Histograms

p <- ggplot(data = iris,
            aes(x = Petal.Length)) +
  geom_histogram() +
  labs(x = 'Petal length (cm)')

print(p)

# plot the histograms for the individual species in one graph
ggplot(data = iris,
       aes(x = Petal.Length,
           fill = Species)) +
  #alpha = transparency factor, position = overlapping values
  geom_histogram(position = 'identity',
                 alpha = 0.7) +
  labs(x = 'Petal length (cm)')


ggplot(data = iris,
       aes(x = Petal.Length,
           color = Species)) +
  geom_freqpoly() +
  labs(x = 'Petal length (cm)')



### Box plots
ggplot(data = iris,
       aes(x = Species,
           y = Petal.Length)) +
  #add points
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3)
  labs(x = 'Iris species',
       y = 'log(Petal length, cm)') +
  scale_y_log10()


## Violin plots
ggplot(data = iris,
       aes(x = Species,
           y = Petal.Length,
           fill = Species)) +
  geom_violin() +
  scale_y_log10() +
  labs(x = 'Iris species',
       y = 'log(Petal length, cm)')




## Bar plots
ggplot(data = mtcars,
       mapping = aes(x = cyl)) +
  geom_bar() +
  labs(x="Number of cylinders", 
       y = "Number of car brands")


#Relation between # of cylinders and # of forward gears
ggplot(data = mtcars,
       mapping = aes(x = cyl,
                     fill = as.factor(gear))) +
  #Add position argument to make the plots side by side
  geom_bar(position = 'dodge') +
  labs(x="Number of cylinders", 
       y = "Number of car brands",
       fill = "Forward gears")


##Mathematical function

ggplot(data = data.frame(x = c(0,2)),
       mapping = aes(x=x)) +
  stat_function(fun = function(x) {
    sin(3*pi*x)
  }) +
  #Change theme to dark
  theme_dark()


## Saving to a file

ggplot(data=iris,
       mapping=aes(x = Petal.Length,
                   y = Sepal.Length)) +
  geom_point() +
  #save as pdf
  pdf()

  
head(iris)

model <- lm(Sepal.Length ~ Species,
            data = iris)

coefficients(model)
anova(model)
summary(model)


# 1-way ANOVA
model <- lm(Petal.Width ~ Petal.Length, data = iris)
anova(model)

# 2-way ANOVA
iris$community <- c("high", "low")
model2 <- lm(Sepal.Width ~ Species * community, data = iris)
anova(model2)
model3 <- lm(Sepal.Width ~ Species + community, data = iris)
anova(model2, model3)


library(dplyr)
mini_iris <- iris %>% 
  group_by(Species) %>% 
  slice(1)

mini_iris %>%
  gather(key = 'flower_att',
         value = 'measurement',
         -Species)


require(gridExtra)

point_plot <- ggplot(data = iris,
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width,
                     color = Species)) +
  geom_point()

jitter_plot <- ggplot(data = iris,
                      mapping = aes(x = Sepal.Length,
                                    y = Sepal.Width,
                                    color = Species)) +
  geom_jitter()


grid.arrange(point_plot, jitter_plot, ncol=2)

ggplot(iris, aes(x = Petal.Length, xend = Petal.Width,
                 y = JitterSpecies, yend = JitterSpecies)) +
  geom_segment()+

# Research design
# - 3 factors: strain (BY, RM),
#              condition (glucose, ethanol),
#              dye (Cy3, Cy5)

# Model
# log-ratio = (b0) + (b1 * dye) + (b2 * strain) + (b3 * condition) + (b4 * strain * condition)

# Exercise 1
library(dplyr)
library(tidyr)
library(ggplot2)

baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
logRatio <- file.path(baseurl, 'data', 'microarray', 'logRatio.csv') 
samples <- file.path(baseurl, 'data', 'microarray', 'samples.csv') 

df_logRatio <- read.table(logRatio, sep = '\t', header = T)
df_samples <- read.table(samples, sep = '\t', header = T)

head(df_logRatio)
head(df_samples)

# Make the logRatio table 
df_genes <- gather(data = df_logRatio,
                   key = SampleID,
                   value = Logratio,
                   -GeneID)

d <- merge(df_genes, df_samples, by = "SampleID")



# Plot data
genedata <- d %>% filter(GeneID == 'YDR518W')

p <- ggplot(data = genedata,
            mapping = aes(x = Condition,
                          y = Logratio,
                          colour = Strain)) +
  geom_jitter(width = 0.05,
              size = 2,
              mapping = aes(shape =))


# Fit a linear model
model <- lm(Logratio ~ Dye + Strain * Condition,
            data = genedata)
summary(model)

# plot coefficients
seg <- data.frame(x1 = c(1,1),
                  x2 = c(2,2),
                  y1 = c(model$coefficients[1],
                         model$coefficients[1] + model$coefficients[3]),
                  y2 = c(model$coefficients[1] + model$coefficients[4],
                         model$coefficients[1] + model$coefficients[3] +
                           model$coefficients[4] + model$coefficients[5]),
                  Strain = c('BY', 'RM'))


p <- p +
  geom_segment(data = seg,
               mapping = aes(x = x1,
                             y = y1,
                             xend = x2,
                             yend = y2))

print(p)


library(broom)
model %>% tidy()
model %>% glance()
model %>% anova() %>% tidy()

varexplained <- model %>% anova() %>% tidy() %>% 
  mutate(fracExplained = sumsq / sum(sumsq))


# Exercise 2
nesteddata <- d %>% 
  group_nest(GeneID)


# Exercise 3
library(purrr)

nesteddata <- nesteddata %>% 
  mutate(model = map(data,
                     function(x) lm(Logratio ~ Dye + Strain * Condition,
                                    data = x)))

# Exercise 4
modelparameters <- nesteddata %>% 
  mutate(summary = map(model, tidy)) %>% 
  unnest(summary) %>% 
  select(-data, -model)

modelparameters

# Exercise 5
modelparameters %>% 
  group_by(term) %>% 
  filter(p.value <= 0.01) %>% 
  summarise(significant = n())

# Exercise 6
# 1. create the anovasum and summary variables
varexplained <- nesteddata %>% 
  mutate(anovasum = map(model, anova),
         summary = map(anovasum, tidy))

#2. Calculate the frac explained
varexplained <- varexplained %>% 
  unnest(summary) %>% 
  select(-c(data,model,anovasum)) %>% 
  group_by(GeneID) %>% 
  mutate(fracExplained = sumsq/sum(sumsq)) %>% 
  ungroup()

#3. Filter and produce final table
varexplained <- varexplained %>% 
  filter(term %in% c('Strain', 'Condition', 'Strain:Condition')) %>% 
  select(GeneID, term, fracExplained) %>% 
  spread(key = term, value = fracExplained)


# Exercise 7
install.packages('ggtern')
library('ggtern')

ggtern(data = varexplained,
      mapping = aes(x = Strain,
                    z = Condition,
                    y = `Strain:Condition`)) +
  geom_point(size = 0.5)

library(tidyverse)

baseurl = 'http://www.few.vu.nl/~molenaar/courses/statR/'
file <- file.path(baseurl, 'data', 'wrangle', 'example_2.txt')

d <- read.table(file, header=TRUE)
boxplot(x = d[-1],
        col = 'grey',
        las = 2)

head(d)

## Gather makes a new dataframe where the "key" = column 1 and "value" = column 2
# Then it gathers the data from old df and inputs the old column names to the
# "key" variable and all the values of that column under the "value" variable
d.new <- gather(data = d,
                key = experiment,
                value = expression,
                -gene)
head(d.new)


#Separate data from 'experiment' column into two columns and cut the column value after '.'
d.new <- separate(data = d.new,
                  col = experiment,
                  into = c('treatment', 'replicate_nr'),
                  sep = '[.]')

head(d.new)

boxplot(expression ~ treatment, data =d.new, col='grey')

### Make all above using pipe
d.new <- d %>% 
  gather(key = experiment,
         value = expression,
         -gene) %>% 
  separate(col = experiment,
           into = c('treatment', 'replicate_nr'),
           sep = "[.]")

#t-test
t.test(expression ~ treatment, data =d.new)
head(d.new)

rows <- sample(nrow(d.new))
d.shuffle <- d.new[rows,]
rownames(d.shuffle) <- c(1:60)
head(d.shuffle)

##Dplyr
file <- file.path(baseurl, 'data', 'wrangle', 'example_2_annotations.txt')
annot <- read.table(file, header=TRUE,sep="\t")

head(d.new)
head(annot)

full.table <- inner_join(d.new, annot)
head(full.table)


## avg. expression vs. treatment
summary.table <- full.table %>% 
  group_by(treatment, biofunction) %>% 
  summarise(meanexpr = mean(expression))

summary.table


### 12.3 - Exercises
tiris <- as_tibble(iris)

library(dplyr)

#distinct
tiris %>% 
  distinct(Species)

#Mutate
mutate(tiris, double_PW = 2*Petal.Width)

#summarise
tiris %>% 
  group_by(Species) %>% 
  summarise(mean_PW = mean(Petal.Width))

#tapply
tapply(X = iris$Petal.Width,
       INDEX = iris$Species,
       FUN = mean)

#summarise
tiris %>% 
  group_by(Species) %>% 
  summarise(mean_PW = mean(Petal.Width),
            sd_PW = sd(Petal.Width))


#mutate
tiris %>% 
  group_by(Species) %>% 
  mutate(mean_PW = mean(Petal.Width),
         sd_PW = sd(Petal.Width)) %>% 
  head()




## Grouping over multiple columns
tminn38 <- tibble::as_tibble(MASS::minn38)

#1
tminn38 %>% distinct(sex, hs)

#2
tminn38 %>% 
  group_by(sex,hs) %>% 
  summarise(frequency = sum(f))

#3
male <- tminn38 %>%
  filter(sex == 'M') %>% 
  nrow()

female <- tminn38 %>%
  filter(sex == 'F') %>% 
  nrow


result <- tminn38 %>% 
  group_by(sex) %>%
  mutate(fraction_by_sex = f/sum(f)) %>% 
  group_by(hs, sex) %>% 
  summarise(percentage = 100*sum(fraction_by_sex))

#spread
result %>% 
  spread(key = sex,
         value = percentage)


ggplot(data = result) +
  geom_bar(mapping = aes(x=hs, y=percentage, fill=sex),
           stat = 'identity',
           position = position_dodge()) +
  labs(x= 'highschool rank', y = 'percentage')



##12.4 - exercise
file <- file.path(baseurl, 'data', 'wrangle', 'exc_1.txt')
mices <- read.table(file, header=T, sep="\t")
head(mices)

#factors and factor levels
# -> sex: M/F
# -> diet: 1,2,3
mices.new <- gather(data = mices,
                    key = diet,
                    value = weight)
head(mices.new)

mices.new <- separate(data = mices.new,
                      col = diet,
                      into = c('diet', 'sex'),
                      sep = '[.]')
head(mices.new)

boxplot(formula = weight~sex*diet,
        data=mices.new,
        col='grey',
        las=2)























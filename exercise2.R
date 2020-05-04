d <- read.csv("http://www.few.vu.nl/~molenaar/courses/data/Zeller2014/zeller2014_phenodata.csv")

head(d)

#mQuestion 1
d %>% 
  group_by(gender) %>% 
  summarise(amount = n())

#Question 2
round(mean(d$age))

#Question 3
d %>%
  filter(gender == 'male',
         group == 'control') %>% 
  nrow()

#Question 4
d %>% 
  filter(bmi >= 30, !is.na(bmi)) %>% 
  nrow()

#Question 5
obese %>% 
  group_by(group) %>% 
  summarise(amount = n())


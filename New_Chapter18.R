# Practicing chapter 18 exercises again (5 in total)


# -----------------------------------------------------------------------------------------------#
# 18.2.2
# Exercise 1

baseurl <- "http://www.few.vu.nl/~molenaar/courses/statR"
filename <- "plasmidtransformation.tab"
f <- file.path(baseurl, "data", "linearmodels", filename)
d <- read.table(f, sep = '\t', header = TRUE)

head(d)

# Which model fits the data?


# Relevel so that 'normal' is the reference level for temperature
d$t <- relevel(d$t, 'normal')
d$log.transforms <- log(d$transforms, 10)

# Compare a model where plasmidprep is additive term vs. interaction term
model.total.sum <- lm(log.transforms ~ plasmidprep + t * h, data = d)
model.total.interact <- lm(log.transforms ~ plasmidprep * t * h, data = d)

# Perform ANOVA
anova(model.total.sum, model.total.interact)

# Fit the data sets for different plasmid preparations individually
transforms.s <- subset(d, plasmidprep == 'syngeneic')
model.s.sum <- lm(log.transforms ~ t + h, data = transforms.s)
model.s.interact <- lm(log.transforms ~ t * h, data = transforms.s)

anova(model.s.sum, model.s.interact)

# -----------------------------------------------------------------------------------------------#
# 18.3.1

# Exercise 1
# Exercise 2
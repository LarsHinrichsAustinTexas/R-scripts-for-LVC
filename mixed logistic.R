# Load packages with functions for model fitting and utilities
library(lme4)
library(mlmRev)
m1 <- lmer(use ~ age + (1|district), Contraception, family=binomial)
fitted <- fitted(m1)
predicted <- ifelse(fitted  >= .5, 1,0)
a <- data.frame(Contraception, predicted)

# Make a crosstab of observed by predicted outcomes, including statistics
library(gmodels)
CrossTable(a$use, a$predicted)

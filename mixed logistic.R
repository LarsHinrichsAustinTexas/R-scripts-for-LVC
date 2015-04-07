library(mlmRev)
m1<-lmer(use~age + (1|district), Contraception, family=binomial)
fitted <- fitted(m1)
predicted <- ifelse(fitted  >= .5, 1,0)
a <- data.frame(Contraception, predicted)

library(gmodels)
CrossTable(a$use, a$predicted)
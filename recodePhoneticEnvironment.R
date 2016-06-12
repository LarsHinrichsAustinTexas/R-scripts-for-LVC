## Convert "speaker" into a factor (because R reads it as being in "integer" format)
oo$speaker <- as.factor(oo$speaker)


## Create as a new factor: taskcomp (whether task comprehension was "good" or "bad")
goodones <- c(2, 3, 8, 10, 14, 16)
goodones <- as.factor(goodones)
oo$taskcomp <- ifelse(is.element(oo$speaker, goodones), "good", "bad")
oo$taskcomp <- as.factor(oo$taskcomp)   # convert our new column into a factor
rm(goodones)


## RE-CODING FOLLOWING PHONETIC ENVIRONMENT
# first, make three vectors that contain the sounds for each of the three groups
labial <- c("B", "M", "P", "V")
coronal <- c("D", "N", "TH", "S", "JH", "Z", "T")
coda <- "None"

# now, use a for-loop to put the correct value for "post_sound_label_2" into each row
for (i in 1:nrow(oo)) {
    if (oo$post_sound[i] %in% labial) { 
      oo$post_sound_label_2[i] <- "labial"
    } else if (oo$post_sound[i] %in% coronal) {
      oo$post_sound_label_2[i] <- "coronal"
    } else if (oo$post_sound[i] == "None"){ 
      oo$post_sound_label_2[i] <- "coda"
    } else {
      oo$post_sound_label_2[i] <- "ERROR"
    }
}
# now we can delete our three environment-vectors from memory
rm(c(labial, coronal, coda))

# finally, convert our new column to factor format
oo$post_sound_label_2 <- as.factor(oo$post_sound_label_2)


## create subsets of the data according to perf. v. normal (or: "non-performed")
ooPerf <- subset(oo, normOrPerf == "perform")
ooNorm <- subset(oo, normOrPerf == "normal")


## subset according to task comprehension
ooGood <- subset(oo, taskcomp == "good")
ooBad <- subset(oo, taskcomp == "bad")


## Subset the "good" and "bad" by perform/norm
ooGoodPerf <- subset(ooGood, normOrPerf == "perform")
ooGoodNorm <- subset(ooGood, normOrPerf == "normal")
ooBadPerf <- subset(ooBad, normOrPerf == "perform")
ooBadNorm <- subset(ooBad, normOrPerf == "normal")


## 4-way plot of F2delta by following environment
par(mfrow=c(2,2))
plot(ooGoodPerf$post_sound_label_2, ooGoodPerf$F2delta, main = "Good, performance")
plot(ooGoodNorm$post_sound_label_2, ooGoodNorm$F2delta, main = "Good, non-performance")
plot(ooBadPerf$post_sound_label_2, ooBadPerf$F2delta, main = "Average, performance")
plot(ooBadNorm$post_sound_label_2, ooBadNorm$F2delta, main = "Average, non-performance")
par(mfrow=c(1,1))


## Linear modeling
## These are a couple of different linear models
m1 <- lm(F2delta ~ post_sound_label_2 + taskcomp + where, data = oo)
m2 <- lm(F2delta ~ post_sound_label_2 + taskcomp + gender, data = oo)
m3 <- lm(F2delta ~ post_sound_label_2 + normOrPerf + taskcomp + gender, data = oo)
m4 <- lm(F2delta ~ post_sound_label_2 + normOrPerf + gender, data = ooGood)
m5 <- lm(F2delta ~ post_sound_label_2 + normOrPerf*gender + normOrPerf + gender , data = ooGood)
# model checking
require(car)
Anova(m5)  # this command gives you a coefficient and one p-value per factor
summary(m5)  # this command gives you more detailed output about the model, including one p-value per level of each factor
drop1(m5)  # another good one for getting relative factor importance



# This script was used to prepare a dataset containing normalized vowel formant measurements for the vowel UW (or: GOOSE).
# Participants were asked to read prepared English sentences, first in an unmarked condition, then in "your 
# best Texas accent". Research question: is the F2 delta affected by the performance condition?
# Script by Lars Hinrichs, 2016.  Contact: lars@texasenglish.org


## Open dataset as oo
oo <- read.csv("normedOOs.csv")


## This dataset initially has the following columns: 
# > names(oo)
# [1] "speaker"           "file"              "context"           "cmu_transcription" "vowel"             "pre_sound"        
# [7] "pre_sound_label"   "post_sound"        "post_sound_label"  "F1."               "F2."               "F3."              
# [13] "F1..gl"            "F2..gl"            "F3..gl"            "F2delta"           "where"             "gender"           
# [19] "ethnicity"         "normOrPerf"


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
# check our work by seeing if any labels of "ERROR" were assigned
which(oo$post_sound_label_2 == "ERROR")


# finally, convert our new column to factor format
oo$post_sound_label_2 <- as.factor(oo$post_sound_label_2)


## create subsets of the data according to performance v. normal (or: "non-performance")
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
## These are a couple of different linear models - our goal is to build the one that has only significant factors in it
m1 <- lm(F2delta ~ post_sound_label_2 + taskcomp + where, data = oo)
m2 <- lm(F2delta ~ post_sound_label_2 + taskcomp + gender, data = oo)
m3 <- lm(F2delta ~ post_sound_label_2 + normOrPerf + taskcomp + gender, data = oo)
m4 <- lm(F2delta ~ post_sound_label_2 + normOrPerf + gender, data = ooGood)
m5 <- lm(F2delta ~ post_sound_label_2 + normOrPerf*gender + normOrPerf + gender , data = ooGood)
# model checking: 
require(car) # you only need to load the car package once
Anova(m5)  # this command gives you a coefficient and one p-value per factor
summary(m5)  # this command gives you more detailed output about the model, including one p-value per level of each factor




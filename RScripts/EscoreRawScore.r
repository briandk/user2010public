# Required Libraries
library(plyr)

# Bringing in the data
source("../RScripts/DataReshape.r", chdir = TRUE)

x <- loadInstructorCSV("../ExamCSV/instructor1MC.csv")
y <- loadInstructorCSV("../ExamCSV/instructor2MC.csv")
z <- loadInstructorCSV("../ExamCSV/instructor3MC.csv")

mcdata <- rbind(x, y, z)
p161mc <- join(mcdata, phys161data, by = "name")
str(p161mc)

# Naming epistemologically relevant items
eitems <- c(
  "mc1",
  "mc3",
  "mc4",
  "mc6",
  "mc8"
)
rawitems <- c(
  "mc1",
  "mc2",
  "mc3",
  "mc4",
  "mc5",
  "mc6",
  "mc7",
  "mc8"
)

# Computing escores and rawscores
dd <- appendEscoresAndRawScores(data = p161mc)
summary(dd)
# Analyzing Escore data by instructor
x <- dd
x <- x[ , c("name", "escore", "rawscore", "rawratio", "eratio", "instructor")]

x1 <- subset(x, instructor == 1)
x2 <- subset(x, instructor == 2)
x3 <- subset(x, instructor == 3)

summary(x1)
summary(x2)
summary(x3)

sd(x1)
sd(x2)
sd(x3)

# Fitting models to the data
lm1 <- lm(rawscore ~ instructor, data = dd)
lm2 <- lm(escore ~ instructor, data = dd)
lm3 <- lm(rawratio ~ instructor, data = dd)
lm4 <- lm(eratio ~ instructor, data = dd)

anova(lm1)
anova(lm2)
anova(lm3)
anova(lm4)

# Running MCPs
eratio.tukey <- TukeyHSD(aov(eratio ~ instructor, data = dd))
eratio.tukey

rawratio.tukey <- TukeyHSD(aov(rawratio ~ instructor, data = dd))
rawratio.tukey
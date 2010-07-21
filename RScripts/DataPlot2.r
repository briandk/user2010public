## Required Libraries
library(ggplot2)

## Custom Functions


## Bringing in the reshaped data
source("DataReshape.r", chdir = TRUE)
phys161dataMelt <- removeBlankUncodeableAndMissing(phys161dataMelt, "q09d")



x <- phys161dataMelt
x <- collapseq09dReasonLevels(phys161dataMelt)

# Exploring all the codes
p <- qplot(
  instructor,
  data = x,
  geom = "bar",
  main = "Code Breakdown\n for the Two Blocks Problem",
  fill = valueCondensed
)

p + scale_fill_brewer()

# Looking at the breakdown of rule-quoters on q09d
x <- returnq09dRuleQuotersAsDataFrame()
x <- subset(x, (value%in%(1)) & (variable%in%(5)))
summary(x)
p <- qplot(
  instructor,
  data = x,
  fill = variable,
  geom = "bar",
  main = "Students Who Quoted ``Conservation of Energy'' as their \nExplanation for q09d"
)

p + scale_fill_brewer()

# Exploring just the people who asserted ME stays the same
staysTheSame <- subset(x, q09d == "b")
summary(staysTheSame)

p <- qplot(
  instructor,
  data = staysTheSame,
  geom = "bar",
  main = "Mechanical Energy Stays the Same",
  fill = valueCondensed
)

p + scale_fill_brewer()

# Exploring the people who correctly asserted that ME increases
increases <- subset(relevelFactors(x), q09d == "a")
summary(increases)

p <- qplot(
  instructor,
  data = increases,
  geom = "bar",
  main = "Mechanical Energy Increases",
  fill = valueCondensed
)

p + scale_fill_brewer()

# Exploring the people who got it wrong and said ME doesn't increase)
gotItWrong <- subset(x, q09d != "a")
summary(gotItWrong)

p <- qplot(
  instructor,
  data = gotItWrong,
  geom = "bar",
  main = "Mechanical Energy Doesn't Increase",
  fill = valueCondensed
)

p + scale_fill_brewer()
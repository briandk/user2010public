source("DataReshape.r", chdir = TRUE)
library(ggplot2)

# Analyzing data from question 09 D
countTotalNumberOfCodesPerColumn(phys161dataMelt)
q09d <- countCodeTypes(phys161dataMelt)
q09d
orderCastAggregatedData(q09d)

# Breaking the data down by instructor
summary(phys161dataMelt$q09d)
q09dByInstructor <- cast(phys161dataMelt, q09d ~ instructor, length)
q09dByInstructor
q09dOrderedByInstructor2 <- orderCastAggregatedData(q09dByInstructor, columnToOrderBy = "2")
q09dOrderedByInstructor2

# How many students correctly said mechanical energy increases?
count(phys161data$q09d == "a" & phys161data$instructor == "1")
count(phys161data$q09d == "a" & phys161data$instructor == "2")
count(phys161data$q09d == "a" & phys161data$instructor == "3")

# How many students in each class asserted that Mechanical Energy stays the
# same?
count(phys161data$q09d == "b" & phys161data$instructor == "1")
count(phys161data$q09d == "b" & phys161data$instructor == "2")
count(phys161data$q09d == "b" & phys161data$instructor == "3")



# Removing the blank, missing, and uncodeable values from the melted data
q09dCodeableReasons <- removeBlankUncodeableAndMissing(phys161dataMelt)
summary(q09dCodeableReasons)

# Looking at the breakdown of rule-quoters by instructor
x <- cast(phys161dataMelt, name + instructor + q09d ~ value)
x <- subset(x, q09d == "b")
str(x)
str(x)
qplot(
  y = 5,
  x = instructor,
  data = x
)

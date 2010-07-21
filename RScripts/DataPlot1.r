## Required Libraries
library(ggplot2)

## Custom Functions


## Bringing in the reshaped data
source("DataReshape.r", chdir = TRUE)

# How many students asserted in q09d that Mechanical Energy stayed the same?
# First, we reorder the factors to make a nice stacked bar chart

p <- qplot(
  instructor,
  data  = relabelq09dFactorLevels(phys161data),
  geom  = "bar",
  fill  = q09d,
  main  = "What Happens to the Mechanical Energy \nof the Two Blocks?",
) + scale_fill_brewer()

p

# Next, we look at facetting by all responses to q09d, including missing,
# uncodeable, and blank
p <- qplot(
  instructor,
  data = relabelq09dFactorLevels(phys161dataMelt),
  geom = "bar",
  main = "What Happens to the Mechanical Energy\n of the Two Blocks?",
  fill = value
)
p + facet_grid(~ q09d)

# Then, we look at facetting by just the valid responses: increases, stays the
# same, decreases
p <- qplot(
  instructor,
  data = relabelq09dFactorLevels(Codeableq09dMelted),
  geom = "bar",
  main = "What Happens to the Mechanical Energy\n of the Two Blocks?",
  fill = value
) 
p + facet_grid(~ q09d)



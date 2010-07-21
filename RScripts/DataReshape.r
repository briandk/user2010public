## Required libraries
library(reshape)

## Preliminary Scripts
source(
  "DataLoad.r", 
  chdir         = TRUE, 
  echo          = TRUE,
  print.eval    = TRUE,
  continue.echo = TRUE
)

## Custom Functions
count <- function (conditional) {
  length(which(conditional))
}

countTotalNumberOfCodesPerColumn <- function (moltenData) {
  cast(moltenData, . ~ variable, length)
}

countCodeTypes <- function (moltenData) {
  cast(moltenData, value ~ . , function(x) length(x))
}

orderCastAggregatedData <- function(castAggregatedData, columnToOrderBy = "(all)") {
  xdf <- as.data.frame(castAggregatedData)
  sortedIndices <- order(xdf[ ,columnToOrderBy], decreasing = TRUE)
  xdf[sortedIndices, ]
}

removeBlankUncodeableAndMissing <- function(data, columnToClean = "value") {
  x  <- data[ ,columnToClean]
  x1 <- (x != "blank") & (x != "uncodeable") & (x != "missingValue")
  return(data[x1, ])
}

relabelq09dFactorLevels <- function(phys161data) {
  newLevels <- c(
    "increases",
    "stays the same",
    "blank",
    "decreases",
    "missing value",
    "uncodeable"
  )
  
  levels(phys161data$q09d) <- newLevels
  # Releveling allows us to place "stays the same" on the bottom of the stack
  phys161data$q09d <- relevel(phys161data$q09d, ref = "stays the same")
  return(phys161data)
}

collapseq09dReasonLevels <- function (data = phys161dataMelt) {
  x <- data$value
  x
  phys161dataMelt1 <- data.frame(phys161dataMelt, valueCondensed = x)
  levels(phys161dataMelt1$valueCondensed) = c(
      "compensation argument",
      "explicit calculation",
      "explicit calculation",
      "explosion effect",
      "only considered 1 block",
      "quoted rule",
      "quoted rule",
      "mechanistic reasoning",
      "mechanistic reasoning",
      "mechanistic reasoning",
      "mechanistic reasoning",
      "mechanistic reasoning",
      "mechanistic reasoning",
      "functional dependency",
      "functional dependency",
      "blank/missing/uncodeable",
      "blank/missing/uncodeable",
      "blank/missing/uncodeable"
  )
  
  phys161dataMelt1$valueCondensed <- relevel(
    phys161dataMelt1$valueCondensed,
    ref = "quoted rule"
  )
  
  return(phys161dataMelt1)
}

relevelFactors <- function(
  data      = x,
  col       = "valueCondensed", 
  reference = "mechanistic reasoning"
  ) 
  {
      x <- data[ , col]
      x <- relevel(x, reference)
      data[ , col] <- x
      return(data)
}

pmelt <- function (dataframe = phys161data) {
  x <- melt(
        data    = dataframe,
        id.vars = c("name", "instructor", "q09d"),
        na.rm   = TRUE # removes the implicit structural missings
  )
  
  return(x)  
}

returnq09dRuleQuotersAsDataFrame <- function (dataframe = phys161data) {
  x <- pmelt(dataframe)
  x <- cast(x, name + q09d + instructor ~ value, margins = FALSE)
  q09dRuleQuoterColumns <- c(
    "name",
    "q09d",
    "instructor",
    "5",
    "5x"
  )

  x <- x[ , q09dRuleQuoterColumns]
  x <- colwise(as.factor)(x)
  x <- removeBlankUncodeableAndMissing(data = x, columnToClean = "q09d")
  x <- pmelt(x)
  return(x)
}

appendEscoresAndRawScores <- function(data = p161mc) {
  escore   <- apply(data[ , eitems], MARGIN = 1, sum)
  rawscore <- apply(data[ , rawitems], MARGIN = 1, sum)
  rawratio  <- rawscore / max (rawscore)
  eratio    <- escore / max (escore)  
  x <- cbind(data, escore, rawscore, eratio, rawratio)
  return(x)
}

# Melting the data
phys161dataMelt <- melt(
  phys161data, 
  id.vars = c("name", "instructor", "q09d"),
  na.rm   = TRUE # removes the implicit structural missings
)

summary(phys161dataMelt)
head(phys161dataMelt)
str(phys161dataMelt)

# Looking at just the codeable answers for q09d
Codeableq09dMelted <- subset(
  phys161dataMelt, q09d == "a" | q09d == "b" | q09d == "c"
)

# Condensing the rule quoters and velocity-changers



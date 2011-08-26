# This is a comment

### CONFIDENTIAL DATA ###

## Custom Functions ##
loadInstructorCSV <- function (CSVfilepath) {
  return(
    read.csv(
      file             = CSVfilepath,
      header           = TRUE,
      stringsAsFactors = TRUE,
      na.strings       = ""
    )
  )
}

bindInstructorData <- function () {
  inst1 <- loadInstructorCSV("../ExamCSV/PHYS161Spring2009Instructor1.csv")
  inst2 <- loadInstructorCSV("../ExamCSV/PHYS161Spring2009Instructor2.csv")
  inst3 <- loadInstructorCSV("../ExamCSV/PHYS161Spring2009Instructor3.csv")
  return(rbind(inst1, inst2, inst3))
}

checkStructureOfData <- function (dataToCheck) {  
  h  <- head(dataToCheck)
  t  <- tail(dataToCheck)
  st <- str(dataToCheck)
  su <- summary(dataToCheck)
  
  print(h)
  print(t)
  print(st)
  print(su)
}

assignq09dCodeLabels <- function (data) {
  levels(data$q09d)["a"] <- "increases"
}

makeInstructorsFactors <- function(data = phys161data) {
  data$instructor <- as.factor(data$instructor)
  return(data)
}

# Loading the Data
phys161data <- bindInstructorData()
phys161data <- makeInstructorsFactors()
checkStructureOfData(phys161data)


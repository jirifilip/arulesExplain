setClass("intervalReader",

  representation(
    decimalSeparator = "character",
    numberSeparator = "character",
    leftClosedBracket = "character",
    leftOpenBracket = "character",
    rightClosedBracket = "character",
    rightOpenBracket = "character",
    negativeInfinity = "character",
    positiveInfinity = "character",
    bracketLen = "numeric"
  ),

  prototype(
    decimalSeparator = ".",
    numberSeparator = ";",
    leftClosedBracket = "[",
    leftOpenBracket = "(",
    rightClosedBracket = "]",
    rightOpenBracket = ")",
    negativeInfinity = "-Inf",
    positiveInfinity = "Inf",
    bracketLen = 1
  ),


)



createIntervalReader <- function(numberSeparator = ";",
                                 closedBrackets = c("(", ")"),
                                 openBrackets = c("[", "]"),
                                 infinities = c("-Inf", "Inf"),
                                 bracketLen = 1,
                                 decimalSeparator = ".") {

  if (length(closedBrackets) != 2) {
    stop("closed brackets must be only 2")
  }
  if (length(openBrackets) != 2) {
    stop("open brackets must be only 2")
  }
  if (length(infinities) != 2) {
    stop("infinities must be only 2")
  }

  intervalReader <- new("intervalReader",
      numberSeparator = numberSeparator,
      negativeInfinity = infinities[1],
      positiveInfinity = infinities[2],
      leftClosedBracket = closedBrackets[1],
      leftOpenBracket = openBrackets[1],
      rightClosedBracket = closedBrackets[2],
      rightOpenBracket = openBrackets[2],
      bracketLen = bracketLen,
      decimalSeparator = decimalSeparator
  )

  intervalReader
}


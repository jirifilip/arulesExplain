#' intervalReader
#'
#' This class serves s data structure for storing different configurations
#' for interval reading.
#'
#' @slot numberSeparator string for separating numbers in an interval
#' @slot leftClosedBracket left closed bracket string
#' @slot rightClosedBracket right closed bracket string
#' @slot leftOpenBracket left open bracket string
#' @slot rightOpenBracket right open bracket string
#' @slot negativeInfinity negative infinity string
#' @slot positiveInfinity positive infinity string
#' @slot bracketLen how long in chars a bracket is
#' @slot decimalSeparator string used as a decimal separator in an interval
#'
#'
#'
#'
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



#' Helper function for creating an intervalReader object.
#'
#'
#' @param numberSeparator string for separating numbers in an interval
#' @param closedBrackets vector of length 2 for parsing closed brackets
#' @param openBrackets vector of length 2 for parsing open brackets
#' @param infinities vector of length 2 for parsing positive and negative infinities
#' @param bracketLen how long in chars a bracket is
#' @param decimalSeparator string used as a decimal separator in an interval
#'
#'
#' @return new intervalReader object
#'
#' @export
#'
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


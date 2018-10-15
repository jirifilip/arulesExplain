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

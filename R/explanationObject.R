#' explanationObject
#'
#' @exportClass explanationObject
#'
explanationObject <- setClass("explanationObject",

 representation(
  ruleDataFrame = "data.frame",
  dataCount = "numeric"
 ),

 prototype(
  ruleDataFrame = data.frame(),
  dataCount = 0
 )
)


setGeneric(
  name = "initializeExplanation",
  def = function (theObject, ruleModel, trainingData) {
    standardGeneric("initializeExplanation")
  }
)

#' @export
#'
setMethod(
  f = "initializeExplanation",
  signature = c("explanationObject", "CBARuleModel", "data.frame"),
  definition = function (theObject, ruleModel, trainingData) {
    theObject@dataCount <- nrow(trainingData)
    theObject@ruleDataFrame <- as.qcba.rules(ruleModel@rules)

    return(theObject)
})


#' @export
#'
setMethod(
  f = "initializeExplanation",
  signature = c("explanationObject", "qCBARuleModel", "data.frame"),
  definition = function (theObject, ruleModel, trainingData) {
    theObject@dataCount <- nrow(trainingData)
    theObject@ruleDataFrame <- ruleModel@rules

    return(theObject)
  })

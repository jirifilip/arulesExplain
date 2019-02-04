#' explanationObject
#'
#' @export explanationObject
#'
explanationObject <- setClass("explanationObject",

 representation(
  ruleDataFrame = "data.frame",
  dataCount = "numeric",
  intervalReader = "intervalReader",
  ruleModel = "CBARuleModel"
 ),

 prototype(
  ruleDataFrame = data.frame(),
  dataCount = 0,
  intervalReader = NULL
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
    theObject@ruleModel <- ruleModel
    theObject@ruleDataFrame <- as.qcba.rules(ruleModel@rules)
    theObject@intervalReader <- createIntervalReader()

    return(theObject)
})

#'
#' @export
#'
setGeneric(
  name = "explainInstances",
  def = function (theObject, ruleModel, dataToExplain) {
    standardGeneric("explainInstances")
  }
)

#'
#'
#' @export
setMethod(
  f = "explainInstances",
  signature = c("explanationObject", "CBARuleModel", "data.frame"),
  definition = function (theObject, ruleModel, dataToExplain) {


    cbaFiringRules <- theObject@ruleDataFrame

    expl <- getExplanationsDataframe(theObject, dataToExplain, ruleModel)

    return(expl)
  }
)


#'
#'
#' @export
#'
setGeneric(
  name = "explainRuleModel",
  def = function(theObject) {
    standardGeneric("explainRuleModel")
  }
)



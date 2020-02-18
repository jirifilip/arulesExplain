#' @title
#' Explanation object
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


#' @export
#'
setGeneric(
  name = "initializeExplanation",
  def = function (theObject, ruleModel, trainingData, ruleModelQCBA) {
    standardGeneric("initializeExplanation")
  }
)


setMethod(
  f = "initializeExplanation",
  signature = c("explanationObject", "CBARuleModel", "data.frame", "qCBARuleModel"),
  definition = function (theObject, ruleModel, trainingData, ruleModelQCBA) {

    theObject@dataCount <- nrow(trainingData)
    theObject@ruleModel <- CBARuleModel()
    theObject@ruleModel@rules <- as.item.matrix(ruleModelQCBA, trainingData)
    theObject@ruleModel@classAtt <- ruleModelQCBA@classAtt
    theObject@ruleModel@cutp <- ruleModel@cutp

    theObject@ruleDataFrame <- as.qcba.rules(theObject@ruleModel@rules)
    theObject@intervalReader <- createIntervalReader()



    return(theObject)
  })


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





#' @export
#'
setGeneric(
  name = "explainInstances",
  def = function (theObject, dataToExplain) {
    standardGeneric("explainInstances")
  }
)


setMethod(
  f = "explainInstances",
  signature = c("explanationObject", "data.frame"),
  definition = function (theObject, dataToExplain) {


    cbaFiringRules <- theObject@ruleDataFrame

    expl <- getExplanationsDataframe(theObject, dataToExplain, theObject@ruleModel)

    return(expl)
  }
)




#'
#'
#' @export
#'
setGeneric(
  name = "explainRuleModel",
  def = function(theObject, data) {
    standardGeneric("explainRuleModel")
  }
)


#'
#'
#' @export
setMethod(
  f = "explainRuleModel",
  signature = c("explanationObject", "data.frame"),
  definition = function (theObject, data) {

    expl <- getClassExplanationsDataframe(theObject, data)

    return(expl)
  }
)


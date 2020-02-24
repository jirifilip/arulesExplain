#' @title Explanation object
#' @export explanationObject
#' @slot ruleDataFrame data frame of mined rules
#' @slot dataCount number of training data
#' @slot intervalReader an intervalReader object
#' @slot ruleModel a cba rule model
#' @include intervalReader.R
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
 ),

 contains = "intervalReader"
)


#' Generic method for initializing explanations
#'
#' @param theObject an explanation objectg
#' @param ruleModel a rule model of CBA
#' @param trainingData training data of the model
#' @param ruleModelQCBA a rule model of QCBA
#'
#' @export
setGeneric(
  name = "initializeExplanation",
  def = function (theObject, ruleModel, trainingData, ruleModelQCBA) {
    standardGeneric("initializeExplanation")
  }
)

#' Method for initializing explanations when the model is trainined by qcba
#'
#' @param theObject an explanation objectg
#' @param ruleModel a rule model of CBA
#' @param trainingData training data of the model
#' @param ruleModelQCBA a rule model of QCBA
#'
#' @rdname initializeExplanationQCBA
#' @export
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


#' Method for initializing explanations when the model is
#'  trained by cba
#'
#'
#' @rdname initializeExplanation
#' @export
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





#' A generic method for explaining instances
#'
#' @param theObject an explanation object
#' @param dataToExplain data to be explained
#'
#' @export
setGeneric(
  name = "explainInstances",
  def = function (theObject, dataToExplain) {
    standardGeneric("explainInstances")
  }
)

#' A method for exlaining instances
#'
#' @param theObject an explanation object
#' @param dataToExplain data to be explained
#'
#' @export
setMethod(
  f = "explainInstances",
  signature = c("explanationObject", "data.frame"),
  definition = function (theObject, dataToExplain) {


    cbaFiringRules <- theObject@ruleDataFrame

    expl <- getExplanationsDataframe(theObject, dataToExplain, theObject@ruleModel)

    return(expl)
  }
)




#' Generic method for explaining a rule model
#'
#' @param theObject an explanation objectg
#' @param data data to be explained
#'
#' @export
setGeneric(
  name = "explainRuleModel",
  def = function(theObject, data) {
    standardGeneric("explainRuleModel")
  }
)


#' Method for explaining a rule model
#'
#' @param theObject an explanation objectg
#' @param data data to be explained
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


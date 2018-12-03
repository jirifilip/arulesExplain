#' Function for parsing an item like 'eye_color=blue' or 'height=<166;180)' and
#' outputting a string explaining the item in natural language like 'eye_color is blue'
#' or 'height is between 166 to 180'.
#'
#' @param item A string containing key and value separated by '=', e.g. 'eye_color=(1;5]'
#' @param intervalReader An object of type intervalReader. It allows different types of brackets,
#'    commas and infinity types to be defined and read.
#'
#'
#' @examples
#'   parseItem("eye_color=white", new("intervalReader"))
#'
#'   parseItem("eye_color=(1;3)", new("intervalReader"))
#'
#'   ir <- new("intervalReader",
#'         numberSeparator = "_to_",
#'         negativeInfinity = "-inf",
#'         positiveInfinity = "inf",
#'         leftClosedBracket = "(",
#'         leftOpenBracket = "",
#'         rightClosedBracket = "",
#'         rightOpenBracket = ")",
#'         bracketLen = 0)
#'   parsedItem <- parseItem("eye_color=-inf_to_inf", new("intervalReader"))
#'
#' @return string explaning an item in natural language
#'
#'
parseItem <- function (item, intervalReader) {

  itemVector <- unlist(strsplit(item, "="))
  attributeName <- itemVector[1]

  # for ordinal attributes that are numeric
  if (!grepl(intervalReader@numberSeparator, itemVector[2])) {
    result <- paste(attributeName, "is", itemVector[2])
    return(result)
  }

  intervalVector <- unlist(strsplit(itemVector[2], intervalReader@numberSeparator))



  leftString <- intervalVector[1]
  rightString <- intervalVector[2]




  leftBound <- substr(leftString, intervalReader@bracketLen + 1, nchar(leftString))
  rightBound <- substr(rightString, 1, nchar(rightString) - intervalReader@bracketLen)

  leftBracket <- substr(leftString, 1, intervalReader@bracketLen)
  rightBracket <- substr(rightString, nchar(rightString) - intervalReader@bracketLen, nchar(rightString))


  result <- NULL

  leftBracketString <- if (leftBracket == intervalReader@leftClosedBracket) "or equal to " else ""
  rightBracketString <- if (leftBracket == intervalReader@rightClosedBracket) "or equal to " else ""

  leftBoundInc <- if (leftBracket == intervalReader@leftOpenBracket) " (excl)" else ""
  rightBoundInc <- if (rightBracket == intervalReader@rightOpenBracket) " (excl)" else ""

  if (leftBound == intervalReader@negativeInfinity) {
    result <- paste(attributeName, " is lower than ", rightBracketString, rightBound, rightBoundInc, sep = "")
  }
  #else if (rightBound == " Inf" || rightBound == "Inf") {
  #  result <- paste(attributeName, " is greater than ", leftBracketString, leftBound, leftBoundInc, sep = "")
  #}
  else if (rightBound == intervalReader@positiveInfinity) {
    result <- paste(attributeName, " is greater than ", leftBracketString, leftBound, leftBoundInc, sep = "")
  }
  else {
    result <- paste(attributeName, " is between ", leftBound, leftBoundInc, " to ", rightBound, rightBoundInc, sep = "")
  }


  result
}

#' Calculate rules statistics to use in the natural language explanation.
#'
#' @param index Index of the rule we want to calculate statistics for.
#' @param allRules Dataframe containing all of the rules.
#' @param consequentStringTrimmed Consequent string trimmed of the surrounding curly brackets.
#' @param data Data to be used to calculate rule statistics.
#'
#' @return string explaining rule quality.
#'
explainRuleStatistics <- function (index, allRules, consequentStringTrimmed, data) {
  relSupport <- allRules[index, 2]
  absAupport <- floor(nrow(data) * relSupport)
  confidence <-  allRules[index, 3]
  incorrectlyPredicted <- floor((absAupport - absAupport * confidence) / confidence)
  numOfCoveredInstances <- floor(incorrectlyPredicted + absAupport)

  qualityText <- paste(
    "There were",
    numOfCoveredInstances,
    "clients which match the conditions of this rule in the training dataset. Out of these",
    absAupport,
    "are predicted correctly as having",
    consequentStringTrimmed,
    "by this rule. The confidence of the rule is thus",
    round(confidence*100),
    "%."
  )

  qualityText
}

#' Function for explaning QCBA.
#'
#' @param rulesText Text of the rules.
#' @param rules Rule dataframe.
#' @param allData Data from which the rules were mined.
#' @param defaultRuleList default rules object from qcba model.
#' @param intervalReader an intervalReader object used for reading intervals in rules
#'
#' @return dataframe containing explanations for rules
#'
#'
explainQCBA <- function (rulesText, rules, allData, defaultRuleList, intervalReader) {
  defaultRule <- defaultRuleList[nrow(defaultRuleList),]
  defaultRuleSupport <- defaultRule[, 2]
  defaultRuleConfidence <- defaultRule[, 3]

  defaultRuleAbsSupport <- floor(defaultRuleSupport * nrow(allData))
  defaultRuleAbsConfidence <- floor(defaultRuleConfidence * defaultRuleAbsSupport)


  antecedentConsequentList <- unname(sapply(rulesText, function (x) { strsplit(x, " => ") }))
  antecedentConsequentArray <- t(simplify2array(antecedentConsequentList))

  explanationTextVector <- c()

  for (i in 1:nrow(antecedentConsequentArray)) {
    antecedentString <- antecedentConsequentArray[i, 1]
    consequentString <- antecedentConsequentArray[i, 2]

    # trim the curly braces from consequent text
    consequentStringTrimmed <- substr(consequentString, 2, nchar(consequentString) - 1)
    # split the consequent text into vector
    consequentTextVector <- unlist(strsplit(consequentStringTrimmed, "="))
    # transform the vector into natural language text
    consequentText <- paste(consequentTextVector[1], "is", consequentTextVector[2])


    ruleText <- ""
    qualityText <- ""

    if (antecedentString != "{}") {
      # trim the curly braces
      antecedentStringTrimmed <- substr(antecedentString, 2, nchar(antecedentString) - 1)
      # split into individual items
      antecedentStringSplit <- unlist(strsplit(antecedentStringTrimmed, ","))
      # parse the items into vectors
      antecedentItemsParsed <- sapply(antecedentStringSplit, function (x) {
        parseItem(x, intervalReader)
      })
      # transform into natural language text
      antecedentText <- paste(antecedentItemsParsed, collapse = " and ")

      ruleText <- paste("IF", antecedentText, "THEN", consequentText)

      # calculate rules statistics to use in the natural language explanation
      qualityText <- explainRuleStatistics(i, rules, consequentStringTrimmed, allData)

    } else {

      default_rule <- consequentString
      default_rule_str <- paste(
        "Among instances not covered by any of the rules, the majority ",
        "(",
        defaultRuleAbsConfidence,
        " out of ",
        defaultRuleAbsSupport,
        ") ",
        "have ",
        consequentStringTrimmed,
        sep = ""
      )

      ruleText <- default_rule_str

    }


    explanationTextVector <- c(explanationTextVector, ruleText, qualityText)
  }

  explanationTextArray <- matrix(explanationTextVector, ncol = 2, byrow = TRUE)
  instance_index <- 1:nrow(explanationTextArray)

  explanation_dataframe <- data.frame(instance_index = instance_index,
                                      explanation = explanationTextArray[,1],
                                      justification = explanationTextArray[,2],
                                      stringsAsFactors = FALSE)


  explanation_dataframe
}

#' Function for getting the explanation dataframe.
#'
#' @param rules rules dataframe from the model
#' @param firingRulesID IDs of the rules that classified the instance
#' @param allData instances that the user wants to get explained
#' @param includeJustifications switch determining if rule justifications
#'     should also be included in the final dataframe
#' @param intervalReader intervalReader that should be used for reading
#'     intervals in the rules
#'
#'
#' @return explanation dataframe
#'
#'
#' @examples
#'   library(arc)
#'
#'   data <- iris
#'
#'   rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])
#'
#'
#'   cbaFiringRuleIDs <- explainPrediction.CBARuleModel(rmCBA, data, discretize=TRUE)
#'   cbaFiringRules <- as.qcba.rules(rmCBA@rules)[cbaFiringRuleIDs,]
#'
#'
#'
#'   explanation_dataframe <- getExplanationsDataframe(rmCBA@rules, cbaFiringRuleIDs, train,
#'       includeJustifications = TRUE, ir)
#'
#'
#'
#' @export
#'
getExplanationsDataframe <- function(rules, firingRulesID, allData, includeJustifications = TRUE, intervalReader = new("intervalReader")) {
  firingRules <- rules[firingRulesID,]
  firingRulesText <- firingRules[,1]

  explanationDataframe <- explainQCBA(firingRulesText, firingRules, allData, rules, intervalReader)

  if (!includeJustifications) {
    explanationDataframeWithoutJustifications <- explanationDataframe
    explanationDataframeWithoutJustifications$justification <- NULL

    return(explanationDataframeWithoutJustifications)
  }

  explanationDataframe[["predicted class"]] <- sapply(explanationDataframe$explanation, function (x) {
    clazzSplit <- unlist(strsplit(x, " "))
    clazz <- clazzSplit[length(clazzSplit)]


    if (grepl("=", clazz)) {
      clazz <- unlist(strsplit(x, "="))[2]
    }

    clazz
  })

  textVector <- c()
  for (i in firingRulesID) {
    text <- getQCBAConflictingRuleText(rules, i, allData)

    textVector <- c(textVector, text)
  }
  explanationDataframe[["strongest rule supporting alternative class"]] <- textVector

  explanationDataframe
}

#' Function for getting the class explanation dataframe.
#'
#' @param rm rule model, either qcba or arc
#' @param allData data on which the rule model was trained
#' @param intervalReader intervalReader that should be used for reading intervals in
#'     the rules
#'
#'
#' @return list (where classes are keys) of class explanation dataframes
#'
#'
#' @examples
#'   library(arc)
#'
#'   train <- iris
#'
#'   rmCBA <- cba(train, classAtt=colnames(train)[length(colnames(train))])
#'
#'   cbaFiringRuleIDs <- explainPrediction.CBARuleModel(rmCBA, train)
#'   cbaFiringRules <- as.qcba.rules(rmCBA@rules)[cbaFiringRuleIDs,]
#'
#'   explanation_dataframe <- getClassExplanationsDataframe(rmCBA, train, createIntervalReader())
#'
#'
#' @export
#'
getClassExplanationsDataframe <- function(rm, allData, intervalReader) {
  rules <- rm@rules

  if (class(rules) != "data.frame") {
    if (class(rules) != "rules") {
      stop("class of rules must be data.frame or rules")
    }

    rules <- as.qcba.rules(rules)
  }


  explanationDataframe <- getExplanationsDataframe(rules, 1:(nrow(rules) - 1), allData, includeJustifications = TRUE, intervalReader)

  classAtt <- rm@classAtt
  classNames <- names(table(allData[classAtt]))


  classExplRulesLength <- nrow(rules)
  classExplRules <- rules[-classExplRulesLength,]
  classExplRulesText <- classExplRules[-classExplRulesLength,1]

  # class explanations
  class_explanation_df <- explanationDataframe
  class_explanation_df$priority <- class_explanation_df$instance_index
  class_explanation_df$instance_index <- NULL
  class_explanation_df$class_expl <- sapply(class_explanation_df$explanation, function (row) {
    split <- unlist(strsplit(row, " THEN "))

    split[1]
  })
  class_explanation_df$class_val <- sapply(class_explanation_df$explanation, function (row) {
    split <- unlist(strsplit(row, " THEN "))

    class_val <- unlist(strsplit(split[2], " is "))[2]

    class_val
  })

  class_explanation_df["Confidence"] <- classExplRules[,3]
  class_explanation_df["Support (rel)"] <- classExplRules[,2]
  class_explanation_df["Support (abs)"] <- floor(nrow(allData) * classExplRules[,2])
  class_explanation_df$Lift <- sapply(class_explanation_df$priority, function (ruleid) {
    confidence <- class_explanation_df$Confidence[ruleid]
    class_val <- class_explanation_df$class_val[ruleid]

    class_support <- sum(allData[classAtt] == class_val) / nrow(allData)

    lift <- confidence / class_support

    lift
  })

  class_explanation_df <- class_explanation_df[,c(3, 1, 6, 7, 8, 9, 5)]

  resultList <- list()

  for (className in classNames) {
    mask <- class_explanation_df$class_val == className

    maskExplanations <- class_explanation_df[mask,]

    maskExplanations$class_val <- NULL

    colnames(maskExplanations)[2] <- paste("Explanation (", classAtt, "=", className, ")", sep = "")

    resultList[[className]] <- maskExplanations
  }

  resultList
}


#' Function for getting information about the next best rule which has different
#' class from the current rule.
#'
#' @param rules dataframe containing rules information
#' @param ruleIndex index of the rule we want to get conflicting text for
#' @param data data on which the model was trained
#'
#' @return conflicting rule text
#'
getQCBAConflictingRuleText <- function(rules, ruleIndex, data) {
  if (ruleIndex == nrow(rules)) {
    return("No specific conflicting rule found.")
  }


  matchesText <- rules
  matchesTextRule <- matchesText[,1]
  matchesTextRuleSplit <- strsplit(matchesTextRule, " => ")

  conflictIndex <- ruleIndex

  ruleClass <- matchesTextRuleSplit[[ruleIndex]][2]
  for (i in (ruleIndex + 1):length(matchesTextRuleSplit)) {

    currentClass <- matchesTextRuleSplit[[i]][2]

    if (currentClass != ruleClass) {
      conflictIndex <- i

      if (i == nrow(rules)) {
        return("No specific conflicting rule found.")
      }

      break()
    }
    if (i == length(matchesTextRuleSplit)) {
      return("No specific conflicting rule found.")
    }
  }

  conflictRuleText <- matchesText[conflictIndex, 1]
  conflictRuleSupport <- matchesText[conflictIndex, 2]
  conflictRuleConfidence <- matchesText[conflictIndex, 3]

  classText <- unlist(strsplit(conflictRuleText, " => "))[[2]]
  classText <- unlist(strsplit(classText, "="))[[2]]
  classText <- substr(classText, 1, nchar(classText) - 1)

  currentRuleSupport <- matchesText[ruleIndex, 2]
  currentRuleConfidence <- matchesText[ruleIndex, 3]

  supportRatio <- (conflictRuleSupport - currentRuleSupport) * 100
  confidenceRatio <- (round(conflictRuleConfidence * 100, 2) - round(currentRuleConfidence * 100, 2))



  #print(supportRatio)
  #print(confidenceRatio)

  #confidenceNumberText <- round((1 - confidenceRatio) * 100, 2)
  #supportNumberText <- round((1 - supportRatio) * 100, 2)
  #supportWeightText <- if (supportNumberText < 0) "higher" else "lower"
  #supportConclusionText <- if (supportNumberText < 0) "less" else "more"

  confidenceNumberText <- round(confidenceRatio, 2)
  supportNumberText <- round(supportRatio, 2)
  supportWeightText <- if (supportNumberText < 0) "smaller" else "higher"
  supportConclusionText <- if (supportNumberText < 0) "less" else "more"

  supportNumberText <- abs(supportNumberText)
  confidenceNumberText <- abs(confidenceNumberText)


  text <- paste(
    "The strongest applicable conflicting rule predicting the alternative class",
    classText,
    "is",
    conflictRuleText,
    "\nThis conflicting rule has lower strength of evidence with confidence",
    confidenceNumberText, "%",
    "lower than the selected rule.",
    paste("(", round(currentRuleConfidence * 100, 2), sep=""), "%", "vs", round(conflictRuleConfidence * 100, 2), paste("%", ").", sep=""),
    "The weight of evidence of the conflicting rule is",
    supportNumberText, "%", supportWeightText,
    "compared to the selected rule",
    paste("(", round(currentRuleSupport * nrow(data)), sep=""), "cases", "vs", round(conflictRuleSupport * nrow(data)), paste("cases", ").", sep="")#,
    #"The conflicting rule captures",  supportConclusionText, "specific group of past cases."
  )

  if (currentRuleSupport > conflictRuleSupport) {
    text <- paste(text, "Overall, the evidence behind the strongest conflicting rule has both lower strength and weight.", sep = "\n")
  }
  if (currentRuleSupport < conflictRuleSupport) {
    text <- paste(text, "Overall, the evidence behind the strongest conflicting rule has lower strength, but higher weight. This suggests that additional statistical analysis of the historical data applicable to this case may be required.", sep = "\n")
  }
  if (currentRuleConfidence == conflictRuleConfidence && currentRuleSupport == conflictRuleText) {
    text <- paste(text, "The evidence behind the strongest conflicting rule and the selected rule has the same strength and the same weight. The assigned class was chosen based on higher specificity of the selected rule.  This suggests that additional statistical analysis of the historical data applicable to this case is mandatory.", sep = "\n")
  }



  text
}






#' Function for investigating which rule classified an instance.
#'
#' @param object CBARuleModel object
#' @param data testing data for prediction
#' @param discretize determines if data should be discretized or not
#' @param ... other arguments (currently not used)
#'
#' @return vector of rule IDs
#'
#' @examples
#'   library(arc)
#'
#'   data <- iris
#'
#'   rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])
#'
#'   cbaFiringRuleIDs <- explainPrediction.CBARuleModel(rmCBA, data, discretize=TRUE)
#'
#' @export
#'
explainPrediction.CBARuleModel  <- function (object, data, discretize = TRUE, ...) {
  if (discretize && length(object@cutp)>0) {
    data <- applyCuts(data, object@cutp, infinite_bounds=TRUE, labels=TRUE)
  }
  test_txns <- as(data, "transactions")

  t <- unname(is.subset(object@rules@lhs, test_txns))

  matches <- suppressWarnings(apply(t, 2, function(x) min(which(x==TRUE))))

  # check if all instances are classified
  first_unclassified_instance <- match(Inf,matches)
  if (!is.na(first_unclassified_instance))
  {
    # the is.subset function does not mark default (with empty lhs) rule as applicable for all instances,
    # we need to do this manually.

    first_rules_with_empty_lhs <- min(which(apply(object@rules@lhs@data, 2, function(x) sum(x))==0))
    if (!is.na(first_rules_with_empty_lhs))
    {
      # the default rule will be used only for instances unclassified by any of the other rules
      matches[matches==Inf] <- first_rules_with_empty_lhs
    }
    else
    {
      stop(paste("There were unclassified instances, the first one has index: ", first_unclassified_instance, " and there is no default rule in the classifier"))
    }

  }

  matches
}





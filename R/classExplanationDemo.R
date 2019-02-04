firingRulesID <- explainPrediction.CBARuleModel(ruleModel, dataToExplain, discretize=TRUE)
firingRules <- expl@ruleDataFrame[firingRulesID,]

firingRulesText <- firingRules[,1]


explanationDataframe <- explainQCBA(expl, firingRulesText, firingRules, firingRulesID)

explanationDataframe[["predicted class"]] <- sapply(explanationDataframe$explanation, function (x) {
  clazzSplit <- unlist(strsplit(x, " "))
  clazz <- clazzSplit[length(clazzSplit)]


  if (grepl("=", clazz)) {
    clazz <- unlist(strsplit(x, "="))[2]
  }

  clazz
})







allData <- iris
rmCBA <- cba(iris, classAtt = "Species")

eo <- explanationObject()
eo <- initializeExplanation(eo, rmCBA, iris)
expl <- eo

rm <- rmCBA


rules <- rm@rules

if (class(rules) != "data.frame") {
  if (class(rules) != "rules") {
    stop("class of rules must be data.frame or rules")
  }

  rules <- as.qcba.rules(rules)
}


firingRulesID <- 1:(nrow(rules) - 1)
firingRules <- expl@ruleDataFrame[firingRulesID,]

firingRulesText <- firingRules[,1]


explanationDataframe <- explainQCBA(expl, firingRulesText, firingRules, firingRulesID)

explanationDataframe[["predicted class"]] <- sapply(explanationDataframe$explanation, function (x) {
  clazzSplit <- unlist(strsplit(x, " "))
  clazz <- clazzSplit[length(clazzSplit)]


  if (grepl("=", clazz)) {
    clazz <- unlist(strsplit(x, "="))[2]
  }

  clazz
})




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

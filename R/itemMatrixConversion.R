#' @importFrom("methods", "as", "new")


#' Function for converting qcba rules dataframe structure to arules
#' itemMatrix structure.
#'
#'
#' @param qcbaRuleModel qcba rule model
#' @param trainingData data which was used for training the rule model
#'
#' @examples
#'   library(arc)
#'   library(qCBA)
#'
#'   data <- cars
#'
#'   rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])
#'   rmqCBA <- qcba(cbaRuleModel=rmCBA,datadf=data)
#'
#'   itemMatrixRules <- as.item.matrix(rmqCBA, data)
#'
#'  @export
#'
as.item.matrix <- function (qcbaRuleModel, trainingData) {
  lhs_itemsets <- c()
  rhs_itemsets <- c()

  row_number <- nrow(qcbaRuleModel@rules)

  for (i in 1:row_number) {
    row <- qcbaRuleModel@rules[i, 1]

    rule_split <- strsplit(row, " => ")[[1]]

    antecedent_str <- substr(rule_split[1], 2, nchar(rule_split[1]) - 1)
    consequent_str <- substr(rule_split[2], 2, nchar(rule_split[2]) - 1)

    antecedent_split <- strsplit(antecedent_str, ",")[[1]]

    lhs_itemsets <- c(lhs_itemsets, antecedent_split)
    rhs_itemsets <- c(rhs_itemsets, consequent_str)

  }

  lhs_itemsets_processed <- sort(unique(lhs_itemsets))
  rhs_itemsets_processed <- sort(unique(rhs_itemsets))

  lhs_rhs_len <- length(lhs_itemsets_processed) + length(rhs_itemsets_processed)

  qcba_matrix_lhs <- matrix(logical(row_number * lhs_rhs_len), nrow = row_number)
  qcba_matrix_rhs <- matrix(logical(row_number * lhs_rhs_len), nrow = row_number)

  dimnames_processed <- c(lhs_itemsets_processed, rhs_itemsets_processed)

  dimnames(qcba_matrix_lhs) <- list(NULL, dimnames_processed)
  dimnames(qcba_matrix_rhs) <- list(NULL, dimnames_processed)

  for (i in 1:row_number) {
    row <- qcbaRuleModel@rules[i, 1]

    rule_split <- strsplit(row, " => ")[[1]]

    antecedent_str <- substr(rule_split[1], 2, nchar(rule_split[1]) - 1)
    consequent_str <- substr(rule_split[2], 2, nchar(rule_split[2]) - 1)

    antecedent_split <- strsplit(antecedent_str, ",")[[1]]
    antecedent_processed <- sort(unique(antecedent_split))

    qcba_matrix_lhs[i, antecedent_processed] <- !logical(length(antecedent_processed))
    qcba_matrix_rhs[i, c(consequent_str)] <- TRUE
  }

  class_vals <- sapply(qcbaRuleModel@rules[,1], function (rule_string) {
    consequent_string <- strsplit(rule_string, " => ")[[1]][2]
    consequent_class_val <- strsplit(consequent_string, "=")[[1]][2]

    substr_class_val <- substr(consequent_class_val, 1, nchar(consequent_class_val) - 1)

    substr_class_val
  })

  quality_preprocessed <- qcbaRuleModel@rules[,2:3]
  quality_preprocessed$lift <- sapply(1:row_number, function (ruleid) {
    confidence <- quality_preprocessed$confidence[ruleid]
    class_val <- class_vals[ruleid]

    class_support <- sum(trainingData[qcbaRuleModel@classAtt] == class_val) / nrow(trainingData)

    lift <- confidence / class_support

    lift
  })
  quality_preprocessed$count <- floor(quality_preprocessed$support * nrow(trainingData))
  quality_preprocessed$lhs_length <- apply(qcba_matrix_lhs, MARGIN = 1, FUN = sum)


  qcba_matrix_quality <- quality_preprocessed

  newRules <- new("rules",  lhs = as(qcba_matrix_lhs, "itemMatrix"),
                  rhs = as(qcba_matrix_rhs, "itemMatrix"),
                  quality = qcba_matrix_quality,
                  info = list(data = "txns",
                              ntransactions = nrow(trainingData)))

  newRules
}


#' Function for converting arules rules data structure to qcba dataframe.
#'
#'
#' @param rules rules to be converted to qcba data structure
#'
#' @examples
#'   library(arc)
#'   library(qCBA)
#'
#'   data <- cars
#'
#'   rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])
#'   rmqCBA <- qcba(cbaRuleModel=rmCBA,datadf=data)
#'
#'   itemMatrixRules <- as.item.matrix(rmqCBA, data)
#'   qcbaRules <- as.qcba.rules(itemMatrixRules)
#'
#'
#'  @export
#'
as.qcba.rules <- function (rules) {

  lhsMatrix <- as(rules@lhs, "matrix")
  rhsMatrix <- as(rules@rhs, "matrix")


  ruleCount <- nrow(lhsMatrix)

  ruleStrings <- c()

  for (i in 1:ruleCount) {
    currentLHSRow <- lhsMatrix[i,]
    currentRHSRow <- rhsMatrix[i,]


    # subset only true values
    # we'll get only the items that the rule contains
    # then we'll take the names of those items
    lhsSubset <- names(currentLHSRow[currentLHSRow])
    rhsSubset <- names(currentRHSRow[currentRHSRow])

    lhsString <- paste(lhsSubset, collapse = ",")
    rhsString <- paste(rhsSubset, collapse = ",")

    # add curly braces to antecedent and consequent
    lhsString <- paste("{", lhsString, "}", sep = "")
    rhsString <- paste("{", rhsString, "}", sep = "")

    ruleStr <- paste(lhsString, "=>", rhsString)

    ruleStrings <- c(ruleStrings, ruleStr)
  }

  rulesQuality <- rules@quality

  newRules <- data.frame(rules = ruleStrings,
                        support = rulesQuality$support,
                        confidence = rulesQuality$confidence,
                        stringsAsFactors = FALSE)
  newRules
}



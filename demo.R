library(qCBA)
library(stringr)
#data <- read.csv("data/breast-w0.csv")
data <- cars

smp_size <- floor(1 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[train_ind, ]

rmCBA <- cba(train, classAtt=colnames(data)[length(colnames(data))])


rmqCBA <- qcba(cbaRuleModel=rmCBA,datadf=train)


# conversion to arules data structure - itemMatrix
itemMatrixRules <- as.item.matrix(rmqCBA, train)

# conversion to qcba data structure
qcbaRules <- as.qcba.rules(itemMatrixRules)

# overwrite the object slot with new rules
rmqCBA@rules <- qcbaRules

# convert back to arules itemMatrix
itemMatrixRules2 <- as.item.matrix(rmqCBA, train)


inspect(itemMatrixRules)
inspect(itemMatrixRules2)
qcbaRules


cbaFiringRuleIDs <- explainPrediction.CBARuleModel(rmCBA, train)
cbaFiringRules <- as.qcba.rules(rmCBA@rules)[cbaFiringRuleIDs,]

# explanation demo
firingRuleIDs <- predict(rmqCBA,test,outputFiringRuleIDs=TRUE)
firingRules <- rmqCBA@rules[firingRuleIDs,]

ir <- new("intervalReader",
          numberSeparator = "_to_",
          negativeInfinity = "-inf",
          positiveInfinity = "inf",
          leftClosedBracket = "<",
          leftOpenBracket = "",
          rightClosedBracket = "",
          rightOpenBracket = ")",
          bracketLen = 0)

explanation_dataframe <- getExplanationsDataframe(rmqCBA@rules, firingRuleIDs, train, includeJustifications = TRUE, ir)
View(explanation_dataframe)


explanation_dataframe <- getClassExplanationsDataframe(rmqCBA, data, ir)
View(explanation_dataframe[["benign"]])


cba_explanation_dataframe <- getExplanationsDataframe(as.qcba.rules(rmCBA@rules), cbaFiringRuleIDs, train, includeJustifications = TRUE, ir)
View(cba_explanation_dataframe)

cba_explanation_dataframe <- getClassExplanationsDataframe(rmCBA, train, ir)
View(cba_explanation_dataframe[["benign"]])

library(qCBA)
library(stringr)
data <- read.csv("data/breast-w0.csv")


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

explanation_dataframe <- getExplanationsDataframe(rmqCBA, firingRuleIDs, train, includeJustifications = FALSE, ir)
View(explanation_dataframe)

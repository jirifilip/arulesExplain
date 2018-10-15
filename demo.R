library(qCBA)
library(stringr)
data <- read.csv("data/heloc_dataset_v1.csv")


smp_size <- floor(1 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[train_ind, ]

rmCBA <- cba(train, classAtt="RiskPerformance", rulelearning_options = list(target_rule_count=50,find_conf_supp_thresholds=TRUE))


rmqCBA <- qcba(cbaRuleModel=rmCBA,datadf=train)


itemMatrixRules <- as.item.matrix(rmqCBA, train)

inspect(itemMatrixRules)

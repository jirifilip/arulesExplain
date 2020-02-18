# arulesExplanation

arulesExplanation provides a way to convert arules data structures to
    explanations which can be easily understood by non-experts. This means explaining all the rules
    in a classifier or simply getting a human-readable version of a rule that classified certain instance.
    Additionally, it provides several convenience functions for converting data structures
    between the qCBA and arules packages.


## Examples

### Data preparation
```R
library(qCBA)

train <- iris

rmCBA <- cba(train, classAtt=colnames(train)[length(colnames(train))])
rmqCBA <- qcba(cbaRuleModel=rmCBA,datadf=train)
```

### Data structures conversion
```R
# conversion to arules data structure - itemMatrix
itemMatrixRules <- as.item.matrix(rmqCBA, train)

# conversion to qcba data structure
qcbaRules <- as.qcba.rules(itemMatrixRules)

# overwrite the object slot with new rules
rmqCBA@rules <- qcbaRules

# convert back to arules itemMatrix
itemMatrixRules2 <- as.item.matrix(rmqCBA, train)
```

### Rules explanation
```R
library(arc)

data <- iris
dataSubset <- iris[sample(1:nrow(data), 15),]

rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])

eo <- explanationObject()
eo <- initializeExplanation(eo, rmCBA, data)
explanationDF <- explainInstances(eo, dataSubset)
View(explanationDF)

classExplanationDF <- explainRuleModel(eo, data)
View(classExplanationDF$virginica)
View(classExplanationDF$versicolor)
View(classExplanationDF$setosa)
```

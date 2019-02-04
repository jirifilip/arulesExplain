context("Testing explanation object")

library(qCBA)
library(arc)


test_that("text explanations are correct", {
  data <- iris

  indexSubset <- seq(1, nrow(data), by = 10)
  dataSubset <- data[indexSubset,]

  data[indexSubset,][1,]

  rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])

  rmCBA@rules %>% inspect

  eo <- explanationObject()
  eo <- initializeExplanation(eo, rmCBA, data)
  explanation_dataframe <- explainInstances(eo, rmCBA, dataSubset)


  (explainPrediction.CBARuleModel(rmCBA, data) == length(eo@ruleModel@rules)) %>%
  which %>% length



  for (i in 1:5) {
    expect_equal(
      "IF Petal.Length is lower than or equal to 2.45 THEN Species is setosa",
      explanation_dataframe$explanation[[i]])
  }


  expect_equal(
    "IF Sepal.Length is greater than 6.15 (excl) and Petal.Length is between 2.45 (excl) to 4.75 THEN Species is versicolor",
    explanation_dataframe$explanation[[6]])
  expect_equal(
    "Among instances not covered by any of the rules, the majority (16 out of 50) have Species=versicolor",
    explanation_dataframe$explanation[[7]])
  expect_equal(
    "IF Petal.Width is greater than 1.75 (excl) THEN Species is virginica",
    explanation_dataframe$explanation[[8]])

  expect_equal(
    "Among instances not covered by any of the rules, the majority (16 out of 50) have Species=versicolor",
    explanation_dataframe$explanation[[9]])
  expect_equal(
    "Among instances not covered by any of the rules, the majority (16 out of 50) have Species=versicolor",
    explanation_dataframe$explanation[[10]])
  expect_equal(
    "IF Sepal.Length is greater than 6.15 (excl) and Petal.Width is greater than 1.75 (excl) THEN Species is virginica",
    explanation_dataframe$explanation[[11]])
  expect_equal(
    "IF Sepal.Length is greater than 6.15 (excl) and Petal.Width is greater than 1.75 (excl) THEN Species is virginica",
    explanation_dataframe$explanation[[12]])
  expect_equal(
    "IF Sepal.Length is greater than 6.15 (excl) and Petal.Width is greater than 1.75 (excl) THEN Species is virginica",
    explanation_dataframe$explanation[[13]])
  expect_equal(
    "IF Sepal.Length is greater than 6.15 (excl) and Petal.Width is greater than 1.75 (excl) THEN Species is virginica",
    explanation_dataframe$explanation[[14]])
  expect_equal(
    "IF Sepal.Length is greater than 6.15 (excl) and Petal.Width is greater than 1.75 (excl) THEN Species is virginica",
    explanation_dataframe$explanation[[15]])
})


test_that("class explanations are correct", {
  data <- iris

  indexSubset <- seq(1, nrow(data), by = 10)
  dataSubset <- data[indexSubset,]

  rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])

  eo <- explanationObject()
  eo <- initializeExplanation(eo, rmCBA, data)

  classExpl <- getClassExplanationsDataframe(eo, data)


  expect_equal(
    classExpl$setosa$priority,
    1
  )
  expect_equal(
    classExpl$setosa$`Explanation (Species=setosa)`,
    "IF Petal.Length is lower than or equal to 2.45"
  )
  expect_equal(
    classExpl$setosa$justification,
    "There were 50 instances which match the conditions of this rule in the training dataset. Out of these 50 are predicted correctly as having Species=setosa by this rule. The confidence of the rule is thus 100 %."
  )



  expect_equal(
    classExpl$virginica[1,]$priority,
    2
  )
  expect_equal(
    classExpl$virginica[1,]$`Explanation (Species=virginica)`,
    "IF Sepal.Length is greater than 6.15 (excl) and Petal.Width is greater than 1.75 (excl)"
  )
  expect_equal(
    classExpl$virginica[1,]$justification,
    "There were 37 instances which match the conditions of this rule in the training dataset. Out of these 37 are predicted correctly as having Species=virginica by this rule. The confidence of the rule is thus 100 %."
  )

  expect_equal(
    classExpl$versicolor[3,]$priority,
    6
  )
  expect_equal(
    classExpl$versicolor[3,]$`Explanation (Species=versicolor)`,
    "IF Sepal.Width is between 2.95 (excl) to 3.35 and Petal.Length is between 2.45 (excl) to 4.75"
  )
  expect_equal(
    classExpl$versicolor[3,]$justification,
    "There were 12 instances which match the conditions of this rule in the training dataset. Out of these 12 are predicted correctly as having Species=versicolor by this rule. The confidence of the rule is thus 100 %."
  )
})


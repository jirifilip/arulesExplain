context("Testing explanation object")

library(qCBA)
library(arc)


test_that("text explanations are correct", {
  data <- iris

  indexSubset <- seq(1, nrow(data), by = 10)
  dataSubset <- data[indexSubset,]

  rmCBA <- cba(data, classAtt=colnames(data)[length(colnames(data))])

  eo <- explanationObject()
  eo <- initializeExplanation(eo, rmCBA, data)
  explanation_dataframe <- explainInstances(eo, rmCBA, dataSubset)

  explanation_dataframe %>% View

  classExpl <- getClassExplanationsDataframe(eo, data)

  classExpl$versicolor %>% View

  rmCBA@rules

  for (i in 1:5) {
    expect_equal(
      "IF Petal.Length is lower than or equal to 2.45 THEN Species is setosa",
      explanation_dataframe$explanation[[i]])
  }


  expect_equal(
    "IF Sepal.Length is greater than or equal to 6.15 and Petal.Length is between 2.45 to 4.75 THEN Species is versicolor",
    explanation_dataframe$explanation[[6]])
  expect_equal(
    "Among instances not covered by any of the rules, the majority (1 out of 5) have Species=versicolor",
    explanation_dataframe$explanation[[7]])
  expect_equal(
    "IF Petal.Width is greater than or equal to 1.75 THEN Species is virginica",
    explanation_dataframe$explanation[[8]])

  expect_equal(
    "Among instances not covered by any of the rules, the majority (1 out of 5) have Species=versicolor",
    explanation_dataframe$explanation[[9]])
  expect_equal(
    "Among instances not covered by any of the rules, the majority (1 out of 5) have Species=versicolor",
    explanation_dataframe$explanation[[10]])
  expect_equal(
    "IF Sepal.Length is greater than or equal to 6.15 and Petal.Width is greater than or equal to 1.75 THEN Species is virginica",
    explanation_dataframe$explanation[[11]])
  expect_equal(
    "IF Sepal.Length is greater than or equal to 6.15 and Petal.Width is greater than or equal to 1.75 THEN Species is virginica",
    explanation_dataframe$explanation[[12]])
  expect_equal(
    "IF Sepal.Length is greater than or equal to 6.15 and Petal.Width is greater than or equal to 1.75 THEN Species is virginica",
    explanation_dataframe$explanation[[13]])
  expect_equal(
    "IF Sepal.Length is greater than or equal to 6.15 and Petal.Width is greater than or equal to 1.75 THEN Species is virginica",
    explanation_dataframe$explanation[[14]])
  expect_equal(
    "IF Sepal.Length is greater than or equal to 6.15 and Petal.Width is greater than or equal to 1.75 THEN Species is virginica",
    explanation_dataframe$explanation[[15]])
})





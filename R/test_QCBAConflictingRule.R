context("Testing QCBAConflicting Rule")


library(arc)
library(qCBA)


skip()

test_that("QCBAConflictingRule works", {
  rmCBA <- cba(iris, classAtt = "Species")

  eo <- explanationObject()
  eo <- initializeExplanation(eo, rmCBA, iris)

  string <- "The strongest applicable conflicting rule predicting the alternative class virginica is {Sepal.Length=(6.15; Inf],Petal.Width=(1.75; Inf]} => {Species=virginica} \nThis conflicting rule has lower strength of evidence with confidence 0 % lower than the selected rule. (100 % vs 100 %). The weight of evidence of the conflicting rule is 8.67 % smaller compared to the selected rule (50 cases vs 37 cases).\nOverall, the evidence behind the strongest conflicting rule has both lower strength and weight."
  expect_equal(getQCBAConflictingRuleText(eo, 1), string)

  string <- "The strongest applicable conflicting rule predicting the alternative class versicolor is {Sepal.Length=(5.55;6.15],Petal.Length=(2.45;4.75]} => {Species=versicolor} \nThis conflicting rule has lower strength of evidence with confidence 0 % lower than the selected rule. (100 % vs 100 %). The weight of evidence of the conflicting rule is 10.67 % smaller compared to the selected rule (37 cases vs 21 cases).\nOverall, the evidence behind the strongest conflicting rule has both lower strength and weight."
  expect_equal(getQCBAConflictingRuleText(eo, 2), string)

  string <- "The strongest applicable conflicting rule predicting the alternative class virginica is {Petal.Width=(1.75; Inf]} => {Species=virginica} \nThis conflicting rule has lower strength of evidence with confidence 2.17 % lower than the selected rule. (100 % vs 97.83 %). The weight of evidence of the conflicting rule is 22 % higher compared to the selected rule (12 cases vs 45 cases).\nOverall, the evidence behind the strongest conflicting rule has lower strength, but higher weight. This suggests that additional statistical analysis of the historical data applicable to this case may be required."
  expect_equal(getQCBAConflictingRuleText(eo, 5), string)
})

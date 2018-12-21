context("Testing parseItem function")

library(qCBA)
library(arc)

skip()

test_that("parseItem works", {
  ir <- createIntervalReader()

  expect_equal(parseItem("eye_color=white", ir),
         "eye_color is white")

  expect_equal(parseItem("height=[-Inf;180]", ir),
               "height is lower than or equal to 180")

  expect_equal(parseItem("height=[-Inf;180)", ir),
               "height is lower than 180 (excl)")

  expect_equal(parseItem("height=[150; Inf]", ir),
               "height is greater than or equal to 150")

  expect_equal(parseItem("height=(150; Inf]", ir),
               "height is greater than 150 (excl)")

  expect_equal(parseItem("height=(150;180]", ir),
               "height is between 150 (excl) to 180")

  expect_equal(parseItem("height=[150;180)", ir),
               "height is between 150 to 180 (excl)")

  expect_equal(parseItem("height=[150;180]", ir),
               "height is between 150 to 180")

  expect_equal(parseItem("height=(150;180)", ir),
               "height is between 150 (excl) to 180 (excl)")

})

test_that("parseItem works with different intervalReader", {
  ir <- createIntervalReader(
    bracketLen = 0,
    numberSeparator = "_to_",
    infinities = c("Inf", "Inf"),
    closedBrackets = c("NULL", ""),
    openBrackets = c("", "NULL")
  )

  expect_equal(parseItem("eye_color=white", ir),
               "eye_color is white")

  expect_equal(parseItem("height=5_to_Inf", ir),
               "height is greater than 5 (excl)")

  expect_equal(parseItem("height=150_to_180", ir),
               "height is between 150 (excl) to 180")

  expect_equal(parseItem("height=150_to_180", ir),
               "height is between 150 (excl) to 180")

  expect_equal(parseItem("height=Inf_to_180", ir),
               "height is lower than 180")
})





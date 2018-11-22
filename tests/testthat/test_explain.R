context("Parsing items")

library(qCBA)

test_that("parsing items works", function () {
  ir <- new("intervalReader")

  expect_equal(parseItem("eye_color=white", ir), "eye_color is white")

})

test_that("parsing items with continuous values works", function () {
  ir <- new("intervalReader")

  expect_equal(
    parseItem("height=[3;5]", ir),
    "height is between 3 to 5")

  expect_equal(
    parseItem("height=(3;5]", ir),
    "height is between 3 (excl) to 5")

  expect_equal(
    parseItem("height=(3;Inf]", ir),
    "height is greater than 3 (excl)")

})

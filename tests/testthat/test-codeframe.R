library(testthat)

source("../../codeframe.R")

test_that("can create codeframe", {
  codeframe <- read.codeframe("../../data/example-codeframe.csv")

  expect_equal(
      codeframe$get("covid"), c("covid", "coronavirus", "mask")
  )
})

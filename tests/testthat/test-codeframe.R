library(testthat)

source("../../codeframe.R")

test_that("can load codeframe", {
  frame <- load.codeframe("../../data/example-codeframe.csv")

  expect_equal(frame$covid, c("covid", "coronavirus", "mask"))

  expect_equal(frame$jobs, c("job", "work"))
})

test_that("can create new codeframe", {
  frame2 <- codeframe(covid = c("covid19", "virus"),
                      family = c("parents"))

  expect_equal(frame2$covid, c("covid19", "virus"))

  expect_equal(frame2$family, c("parents"))
})

test_that("can merge codeframes", {
  frame <- read.codeframe("../../data/example-codeframe.csv")
  frame2 <- codeframe(covid = c("covid19", "virus"),
                      family = c("parents"))

  frame3 <- union(frame, frame2)

  expect_equal(frame3$covid,
               c("covid", "coronavirus", "mask", "covid19", "virus"))

  expect_equal(frame3$jobs, c("job", "work"))

  expect_equal(frame3$family, c("parents"))
})

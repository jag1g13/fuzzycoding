library(testthat)

source("../../fuzzycoding.R")

test_that("can match keywords", {
  words <- c("picket", "dragon", "common")
  expect_false(
    is_keywords_match(words, c("notpresent", "northis"))
  )

  expect_true(
    is_keywords_match(words, c("picket"))
  )

  expect_true(
    is_keywords_match(words, c("picket", "dragon"))
  )

  expect_true(
    is_keywords_match(words, c("pocket"))
  )

  expect_true(
    is_keywords_match(words, c("dragons"))
  )
})

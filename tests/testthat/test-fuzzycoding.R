library(testthat)

source("../../fuzzycoding.R")

words <- c("picket", "dragon", "common")

test_that("can match keywords", {
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

test_that("can match multi-word phrases", {
    expect_false(
        is_keywords_multi_match(words, c("picket common"))
    )

    expect_true(
        is_keywords_multi_match(words, c("picket dragon"))
    )

    expect_true(
        is_keywords_multi_match(words, c("dragon common"))
    )

    expect_false(
        is_keywords_multi_match(words, c("dragon common notpresent"))
    )
})

library(testthat)

source("../../util.R")
source("../../codeframe.R")
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
})

test_that("can match similar words", {
  expect_true(
    is_keywords_match(words, c("pocket"))
  )

  expect_true(
    is_keywords_match(words, c("dragons"))
  )
})

test_that("can match multi-word phrases", {
    # Multi-word phrases have to be split into word vectors
    # The `clean_responses` function does this
    expect_false(
        is_keywords_match(words,
                          clean_responses(c("picket common")))
    )

    expect_true(
        is_keywords_match(words,
                          clean_responses(c("picket dragon")))
    )

    expect_true(
        is_keywords_match(words,
                          clean_responses(c("dragon common")))
    )

    expect_false(
        is_keywords_match(words,
                          clean_responses(c("dragon common notpresent")))
    )
})

test_that("can apply coding frame", {
  responses <- read.csv("../../data/example-responses.csv")
  responses$response <- clean_responses(responses$response)

  frame <- read.codeframe("../../data/example-codeframe.csv")

  results <- apply_coding(responses, frame)

  # Example responses contain 4 on covid and 2 on jobs
  expect_equal(coded_as(results, "covid") %>% nrow, 4)
  expect_equal(coded_as(results, "jobs") %>% nrow, 2)
})

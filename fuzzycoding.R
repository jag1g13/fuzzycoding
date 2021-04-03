require(dplyr)
require(parallel)
require(stringdist)
require(stringr)

#' Do any of the patterns match any of the words?
#'
#' Patterns are fuzzy matched with an allowed error rate of 1/3 the length of
#' each word for each word in the pattern.
#' @param words Phrase being coded as list of words
#' @param patterns Topic keywords (or keyphrases) to look for
is_keywords_match <- function(words, patterns) {
  # Split each pattern into words
  patterns_words <- strsplit(patterns, " +")

  matches <- lapply(patterns_words, function(pattern_words) {
    # Match pattern words against target words
    word_matches <- lapply(pattern_words, function(pattern) {
      amatch(pattern, words, method = "dl", maxDist = nchar(pattern) / 3)
    })

    # Must find all words in the pattern for this pattern to match
    any_missing <- any(is.na(word_matches))
    if (any_missing) return(FALSE)

    # Did all found words appear consecutively?
    steps <- diff(unlist(word_matches))
    return(all(steps == 1))
  })

  # Did any pattern match?
  return(any(unlist(matches)))
}

#' Apply coding frame to the responses.
#'
#' Topic keywords are fuzzy matched to words in the responses.
#' @param responses Responses split into words
#' @param coding_frame Dictionary of topics to list of keywords
apply_coding <- function(responses, coding_frame) {
  # Setup parallelisation
  num_procs <- detectCores() - 1
  cluster <- makeCluster(num_procs)
  clusterExport(cluster, c("is_keywords_match", "amatch"))

  for (topic in coding_frame$keys()) {
    strings <- coding_frame$get(topic)

    is_match <- function(texts) {
        return(is_keywords_match(texts, strings))
    }

    responses <- responses %>%
      mutate("topic.{topic}" := parLapply(cluster, response, is_match))
  }

  stopCluster(cluster)
  return(responses)
}

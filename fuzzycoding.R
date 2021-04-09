require(dplyr)
require(parallel)
require(stringdist)
require(stringr)

#' Do any of the patterns match any of the words?
#'
#' Patterns are fuzzy matched with an allowed error rate of 1/3 the length of
#' each word for each word in the pattern.
#' @param words Phrase being coded as list of words
#' @param patterns_words Topic keywords/phrases as list of word vectors
is_keywords_match <- function(words, patterns_words) {
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
#' @param coding_frame Coding frame of topics to list of keywords
apply_coding <- function(responses, coding_frame) {
  # Setup parallelisation
  num_procs <- detectCores() - 1
  cluster <- makeCluster(num_procs)
  clusterExport(cluster, c("is_keywords_match", "amatch"))

  for (topic in names(coding_frame)) {
    strings <- clean_responses(coding_frame[[topic]])

    is_match <- function(texts) {
        return(is_keywords_match(texts, strings))
    }

    responses <- responses %>%
      mutate("topic.{topic}" := parLapply(cluster, response, is_match))
  }

  stopCluster(cluster)
  return(responses)
}

#' Select responses coded with a given topic.
#'
#' @param coded Dataframe of coded responses
#' @param topic Topic to filter by
coded_as <- function(coded, topic) {
  column <- paste("topic.", topic, sep = "")
  coded %>% filter(!!as.symbol(column) == TRUE) %>% return
}

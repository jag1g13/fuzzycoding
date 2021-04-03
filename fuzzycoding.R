require(dplyr)
require(parallel)
require(stringdist)

#' Do any of the patterns match any of the words?
#'
#' Patterns are fuzzy matched with an allowed error rate of 1/3 the length of the word.
#' @param words Phrase being coded as list of words
#' @param patterns Topic keywords to look for
is_keywords_match <- function (words, patterns) {
  matches <- lapply(patterns, function (pattern) {
    return(amatch(pattern, words, method = "dl", maxDist = nchar(pattern) / 3))
  })

  # Were any matches found? stringdist uses NA to represent no match
  return(!all(is.na(matches)))
}


#' Apply coding frame to the responses.
#'
#' Topic keywords are fuzzy matched to words in the responses.
#' @param responses Responses split into words
#' @param coding_frame Dictionary of topics to list of keywords
apply_coding <- function (responses, coding_frame) {
  # Setup parallelisation
  num_procs <- detectCores() - 1
  cluster <- makeCluster(num_procs)
  clusterExport(cluster, c("is_keywords_match", "amatch"))

  for (topic in coding_frame$keys()) {
    strings <- coding_frame$get(topic)

    is_match <- function(texts) { return(is_keywords_match(texts, strings)) }

    responses <- responses %>%
      mutate("topic.{topic}" := parLapply(cluster, response, is_match))
  }

  stopCluster(cluster)
  return(responses)
}

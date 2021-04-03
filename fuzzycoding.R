require(collections)
require(dplyr)
require(parallel)
require(stringdist)
require(stringr)


data_path <- 'data/Q2.a.1.csv'
responses <- read.csv(data_path, stringsAsFactors = FALSE)
names(responses) <- c("response")
#responses <- head(responses, n = 10)

#' Duplicate rows in a dataframe.
#'
#' Useful for testing performance with bigger datasets.
#' @param frame Dataframe to double
#' @param doubles Number of doublings
double_frame <- function (frame, doubles) {
  for (i in 1:doubles) {
    frame <- rbind(frame, frame)
  }

  return(frame)
}

#responses <- double_frame(responses, 2)

start_time <- Sys.time()

# Clean responses and split into words
responses$response <- responses$response %>%
  lapply(., tolower) %>%
  str_remove_all(., "[,.'’;:-]+") %>%
  strsplit(., " +")

# Create initial coding frame
coding_frame <- dict()
coding_frame$set('covid', c('covid', 'coronavirus', 'mask', 'corona', 'covid19'))

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

coded_texts <- apply_coding(responses, coding_frame)

print(nrow(coded_texts))
print(Sys.time() - start_time)


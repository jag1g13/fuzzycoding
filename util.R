require(dplyr)
require(stringr)

#' Clean responses for case and punctuation then split into words.
#'
#' Any punctuation is considered a word split except apostrophes.
#'
#' @examples
#' df$responses <- clean_responses(df$responses)
clean_responses <- function(responses) {
  cleaned <- responses %>%
    lapply(tolower) %>%
    str_replace_all("[,.;:-]+", " ") %>%  # Replace punctuation with spaces
    str_remove_all("['’]+") %>%  # Remove apostrophes - i.e. don't -> dont
    strsplit(" +")
  return(cleaned)
}

#' Write a list of responses to file without CSV decoration.
#'
#' @param responses List of responses
#' @param filepath Path of file to write
write_responses <- function(responses, filepath) {
  if (file.exists(filepath)) {
    file.remove(filepath)
  }
  lapply(responses, write, filepath, append = TRUE)
}

#' Duplicate rows in a dataframe.
#'
#' Useful for testing performance with bigger datasets.
#' @param frame Dataframe to double
#' @param doubles Number of doublings
double_frame <- function(frame, doubles) {
  for (i in 1:doubles) {
    frame <- rbind(frame, frame)
  }

  return(frame)
}

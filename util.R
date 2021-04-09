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

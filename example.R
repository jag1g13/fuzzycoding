require(collections)
require(stringr)

source('./fuzzycoding.R')


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

coded_texts <- apply_coding(responses, coding_frame)

print(nrow(coded_texts))
print(Sys.time() - start_time)

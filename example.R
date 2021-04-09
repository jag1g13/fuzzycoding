require(stringr)

source("./util.R")
source("./codeframe.R")
source("./fuzzycoding.R")

data_path <- 'data/Q2.a.1.csv'
responses <- as.data.frame(read.delim(data_path, stringsAsFactors = FALSE, sep = "\t"))
responses_raw <- responses

names(responses) <- c("response")
responses$response <- clean_responses(responses$response)
# responses <- double_frame(responses, 6)

start_time <- Sys.time()

# Create codeframe
code_frame <- codeframe(covid=c(
  "covid",
  "coronavirus",
  "virus",
  "covid19",
  "corona",
  "vaccine",
  "vaccination",
  "lockdown",
  "pandemic",
  "mask",
  "normal",
  "normality",
  "epidemic",
  "old routines",
  "being able to do things again",
  "interrupted"
))

# Code responses and filter for covid topic
coded_responses <- apply_coding(responses, code_frame)
covid_coded <- coded_as(coded_responses, "covid")

cat("Total rows:", nrow(coded_responses), "\n")
cat("Covid coded:", nrow(covid_coded), "\n")
cat("Time taken:", Sys.time() - start_time, "s\n")

results <- subset(responses_raw, coded_responses$topic.covid==TRUE)$response
write_responses(results, "data/coded.csv")

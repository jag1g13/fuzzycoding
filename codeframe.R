require(collections)
require(dplyr)

codeframe <- structure(list(dict = dict()), class = "codeframe")

codeframe <- function() {
    value <- list(dict = dict())
    class(value) <- "codeframe"

    return(value)
}

read.codeframe <- function(...) {
    long_frame <- read.csv(...)

    wide_frame <- long_frame %>% unstack(keyword ~ topic)
    codeframe <- dict(wide_frame)

    return(codeframe)
}

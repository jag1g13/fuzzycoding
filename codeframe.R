require(dplyr)

#' Create a new codeframe object
#'
#' @examples
#' codeframe()
#' codeframe(covid=c("coronavirus"))
#' codeframe(covid=c("coronavirus"), jobs=c("job", "work"))
codeframe <- function(...) {
    data <- list(...)
    return(as.codeframe(data))
}

#' Convert a list of vectors into a codeframe.
#'
#' @examples
#' as.codeframe(list(covid=c("coronavirus")))
as.codeframe <- function(x) {
    codeframe <- structure(as.list(x), class = "codeframe")
    return(codeframe)
}

#' Read a codeframe from CSV.
#'
#' @examples
#' read.codeframe("data/example-codeframe.csv")
read.codeframe <- function(...) {
    data <- read.csv(...) %>% unstack(keyword ~ topic)
    return(as.codeframe(data))
}

union <- function(x, y) UseMethod("union", x)

union.default <- function(x, y) base::union(x, y)

#' Merge two codeframes, combining keywords for each topic.
#'
#' @examples
#' union(frame1, frame2)
union.codeframe <- function(x, y) {
    frame <- rlang::duplicate(x)

    for (topic in union(names(x), names(y))) {
        frame[[topic]] <- union(x[[topic]], y[[topic]])
    }

    return(frame)
}

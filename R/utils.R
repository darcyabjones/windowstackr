drop_nulls <- function(x) {
  x <- unclass(x)
  x <- x[!vapply(x, FUN.VALUE = TRUE, FUN = is.null)]

  keep <- rep(TRUE, length(x))
  names(keep) <- names(x)
  for (n in names(x)) {
    v <- x[[n]]
    if (is.list(v)) {
      v <- drop_nulls(v)
      if ((is.null(v)) || (length(v) == 0)) {
        keep[n] <- FALSE
      } else {
        x[[n]] <- v
      }
    }
  }
  x[keep]
}

drop_empty_sublists <- function(li, v) {
  if (v %in% names(li)) {
    li[[v]] <- drop_nulls(li[[v]])
  }
  if ((length(li[[v]]) == 0) || is.null(li[[v]])) {
    li <- li[names(li) != v]
  }
  return(li)
}

try_sprintf <- function(template, ...) {
  tryCatch(
    suppressWarnings(sprintf(template, ...)),
    error = function(e) {
      return(template)
    }
  )
}

prepend_names <- function(options, prefix = "gs-", suffix = "") {
  opt <- options
  names(opt) <- paste0(prefix, names(opt), suffix)
  return(opt)
}

pound <- function(string) {
  paste0("#", string)
}

dot <- function(string) {
  paste0(".", string)
}

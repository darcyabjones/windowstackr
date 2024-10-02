drop_nulls <- function(x) {
  x[!vapply(unclass(x), FUN.VALUE = TRUE, FUN = is.null)]
}

drop_empty_sublists <- function(li, v) {
  if (v %in% names(li)) {
    li[[v]] <- drop_nulls(li[[v]])
  }
  if ((length(li[[v]]) == 0) | is.null(li[[v]])) {
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

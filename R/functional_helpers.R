new_some <- function(x, ..., null_as_none = NULL, class = character()) {
  structure(list(value = x, null_as_none = null_as_none), ..., class = c(class, "some", "optional"))
}

some <- function(x, ..., null_as_none = NULL) {
  new_some(x, null_as_none = null_as_none)
}

toString.some <- function(x, ...) {
  x <- x[["value"]]
  if (any(is(x, "optional"), is(x, "either"), is(x, "result"), is(x, "error_chain"))) {
    x <- toString(x)
  } else {
    x <- deparse(x)
  }
  sprintf("some(%s)", x)
}

print.some <- function(x, ..., quote = FALSE) {
  print(toString.some(x), ..., quote = quote)
}

new_none <- function(..., null_as_none = NULL, class = character()) {
  structure(list(null_as_none = null_as_none), ..., class = c(class, "none", "optional"))
}

none <- function(..., null_as_none = NULL) {
  new_none(null_as_none = null_as_none)
}

toString.none <- function(x, ...) {
  "none()"
}

print.none <- function(x, ..., quote = FALSE) {
  print(toString.none(x), ..., quote = quote)
}

optional <- function(x, ..., null_as_none = NULL) {
  if ((is.null(null_as_none) || null_as_none) && is.null(x)) {
    none(null_as_none = null_as_none)
  } else {
    some(x, null_as_none = null_as_none)
  }
}

print.optional <- function(x, ..., quote = FALSE) {
  print(sprintf("optional(%s)", deparse(x[["value"]])), ..., quote = quote)
}


new_left <- function(x, ..., class = character()) {
  structure(list(value = x), ..., class = c(class, "left", "either"))
}

left <- function(x, ...) {
  new_left(x, ...)
}

toString.left <- function(x, ...) {
  x <- x[["value"]]
  if (any(is(x, "optional"), is(x, "either"), is(x, "result"), is(x, "error_chain"))) {
    x <- toString(x)
  } else {
    x <- deparse(x)
  }
  sprintf("left(%s)", x)
}

print.left <- function(x, ..., quote = FALSE) {
  print(toString.left(x), ..., quote = quote)
}

new_right <- function(x, ..., class = character()) {
  structure(list(value = x), ..., class = c(class, "right", "either"))
}

right <- function(x, ...) {
  new_right(x, ...)
}

toString.right <- function(x, ...) {
  x <- x[["value"]]
  if (any(is(x, "optional"), is(x, "either"), is(x, "result"), is(x, "error_chain"))) {
    x <- toString(x)
  } else {
    x <- deparse(x)
  }
  sprintf("right(%s)", x)
}

print.right <- function(x, ..., quote = FALSE) {
  print(toString.right(x), ..., quote = quote)
}

either <- function(x, ...) {
  right(x, ...)
}

new_ok <- function(x, ..., class = character()) {
  structure(list(value = x), ..., class = c(class, "ok", "result"))
}

ok <- function(x, ...) {
  new_ok(x, ...)
}

toString.ok <- function(x, ...) {
  x <- x[["value"]]
  if (any(is(x, "optional"), is(x, "either"), is(x, "result"), is(x, "error_chain"))) {
    x <- toString(x)
  } else {
    x <- deparse(x)
  }
  sprintf("ok(%s)", x)
}

print.ok <- function(x, ..., quote = FALSE) {
  print(toString.ok(x), ..., quote = quote)
}

new_err <- function(x, ..., class = character()) {
  structure(list(value = x), ..., class = c(class, "err", "result"))
}

err <- function(x, ...) {
  new_err(x, ...)
}

toString.err <- function(x, ...) {
  x <- x[["value"]]
  if (any(is(x, "optional"), is(x, "either"), is(x, "result"), is(x, "error_chain"))) {
    x <- toString(x)
  } else {
    x <- deparse(x)
  }
  sprintf("err%s)", x)
}

print.err <- function(x, ..., quote = FALSE) {
  print(toString.err(x), ..., quote = quote)
}

result <- function(x, ...) {
  ok(x, ...)
}

new_error_chain <- function(x, ..., errors = NULL, class = character()) {
  if (is.null(errors)) {
    errors <- list()
  } else if (is.character(errors)) (
    errors <- as.list(errors)
  )
  structure(list(value = x, errors = errors), ..., class = c(class, "error_chain"))
}

validate_error_chain <- function(x) {
  errors <- x[["errors"]]
  if (!is.list(errors)) {
    stop("The errors must be a list of strings", call. = FALSE)
  } else if (any(!vapply(errors, FUN.VALUE = TRUE, FUN = is.character))) {
    stop("The errors must be a list of strings", call. = FALSE)
  }
  x
}

error_chain <- function(x, ..., errors = NULL) {
  new_error_chain(x, ..., errors = errors) |> validate_error_chain()
}

toString.error_chain <- function(x, ...) {
x2 <- x[["value"]]
  if (any(is(x2, "optional"), is(x2, "either"), is(x2, "result"), is(x2, "error_chain"))) {
    x2 <- toString(x2)
  } else {
    x2 <- deparse(x2)
  }
  sprintf("error_chain(%s, errors = %s)", x2, deparse(x[["errors"]]))
}

print.error_chain <- function(x, ..., quote = FALSE) {
  print(toString.error_chain(x), ..., quote = quote)
}


as_optional <- function(.x, ..., nulls_as_none = NULL) {
  UseMethod("as_optional")
}

as_optional.default <- function(.x, ..., nulls_as_none = NULL) {
  optional(.x, ...)
}

as_optional.optional <- function(.x, ..., nulls_as_none = NULL) {
  if (is.null(nulls_as_none)) {
    .x
  } else {
    if (inherits(.x, "some")) {
      some(.x, null_as_none = nulls_as_none)
    } else {
      none(nulls_as_none = nulls_as_none)
    }
  }
}

as_optional.either <- function(.x, ..., nulls_as_none = NULL) {
  if (inherits(.x, "right")) {
    some(.x[["value"]], ..., nulls_as_none = nulls_as_none)
  } else {
    none(nulls_as_none = nulls_as_none)
  }
}

as_optional.result <- function(.x, ..., nulls_as_none = NULL) {
  if (inherits(.x, "ok")) {
    some(.x[["value"]], ..., nulls_as_none = nulls_as_none)
  } else {
    none(nulls_as_none = nulls_as_none)
  }
}

as_either <- function(.x, ...) {
  UseMethod("as_either")
}

as_either.default <- function(.x, ...) {
  either(.x, ...)
}

as_either.optional <- function(.x, ...) {
  if (inherits(.x, "some")) {
    right(.x, ...)
  } else {
    left(NULL)
  }
}

as_either.either <- function(.x, ...) {
  .x
}

as_either.result <- function(.x, ...) {
  if (inherits(.x, "ok")) {
    right(.x[["value"]], ...)
  } else {
    left(.x[["value"]], ...)
  }
}

as_either.error_chain <- function(.x, ...) {
  right(.[["errors"]])
}

as_result.default <- function(.x, ...) {
  result(.x, ...)
}

as_result.optional <- function(.x, ...) {
  if (inherits(.x, "some")) {
    ok(.x, ...)
  } else {
    err(NULL)
  }
}

as_either.either <- function(.x, ...) {
  .x
}

as_either.result <- function(.x, ...) {
  if (inherits(.x, "ok")) {
    right(.x[["value"]], ...)
  } else {
    left(.x[["value"]], ...)
  }
}

as_either.error_chain <- function(.x, ...) {
  right(.[["errors"]])
}




fmap <- function(.x, .fn, ...) {
  UseMethod("fmap")
}

fmap.optional <- function(.x, .fn, ..., null_as_none = NULL) {
  if (inherits(.x, "none")) {
    return(.x)
  }
  o <- .fn(.x[["value"]], ...)

  if (is.null(null_as_none)) {
    null_as_none <- .x[["null_as_none"]]
  }

  if (!is.null(null_as_none) && null_as_none && is.null(o)) {
    none(null_as_none = null_as_none)
  } else {
    some(o, null_as_none = null_as_none)
  }
}

fmap.either <- function(.x, .fn, ...) {
  if (inherits(.x, "left")) {
    return(.x)
  } else {
    return(right(.fn(x[["value"]], ...)))
  }
}

fmap.result <- function(.x, .fn, ...) {
  if (inherits(.x, "err")) {
    return(.x)
  } else {
    return(ok(.fn(x[["value"]], ...)))
  }
}

fmap.error_chain <- function(.x, .fn, ...) {
  e <- .fn(.x[["value"]], ...)
  if (is.null(e) || (length(e) == 0)) {
    return(.x)
  } else {
    # Could add .fn call as a name here?
    e <- c(.x[["errors"]], as.character(e))
  }
  return(error_chain(.x[["value"]], errors = e))
}

fmap_other <- function(.x, .fn, ...) {
  UseMethod("fmap_other")
}

fmap_other.either <- function(.x, .fn, ...) {
  if (inherits(.x, "right")) {
    return(.x)
  } else {
    return(left(.fn(x[["value"]])))
  }
}

fmap_other.result <- function(.x, .fn, ...) {
  if (inherits(.x, "ok")) {
    return(.x)
  } else {
    return(err(.fn(x[["value"]])))
  }
}

fmap_other.error_chain <- function(.x, .fn, ...) {
  e <- .fn(.x[["errors"]], ...)
  if (is.null(e)) {
    return(.x)
  }
  return(error_chain(.x[["value"]], errors = e))
}

and_then <- function(.x, .fn, ...) {
  UseMethod("and_then")
}

and_then.default <- function(.x, .fn, ...) {
  if (is.null(.x)) {
    return(.x)
  } else {
    return(.fn(.x, ...))
  }
}

and_then.optional <- function(.x, .fn, ..., null_as_none = NULL) {
  if (is.null(null_as_none)) {
    null_as_none <- .x[["null_as_none"]]
  }

  if (inherits(.x, "none")) {
    return(none(null_as_none = null_as_none))
  }
  o <- .fn(.x[["value"]], ...)
  stopifnot("Function needs to return an 'optional' type." = inherits(o, "optional"))

  if (inherits(.x, "none")) {
    return(none(null_as_none = null_as_none))
  }

  if (!is.null(null_as_none) && null_as_none && is.null(o)) {
    none(null_as_none = null_as_none)
  } else {
    some(o, null_as_none = null_as_none)
  }
}

and_then.either <- function(.x, .fn, ...) {
  if (inherits(.x, "left")) {
    return(.x)
  } else {
    tmp <- .fn(.x)
    stopifnot("Function needs to return an 'either' type." = inherits(tmp, "either"))
    return(tmp)
  }
}

and_then.result <- function(.x, .fn, ...) {
  if (inherits(.x, "err")) {
    return(.x)
  } else {
    tmp <- .fn(.x)
    stopifnot("Function needs to return a 'result' type." = inherits(tmp, "result"))
    return(tmp)
  }
}

and_then.error_chain <- function(.x, .fn, ...) {
  e <- .fn(.x[["value"]], ...)
  stopifnot("Function needs to return an 'error_chain' type." = inherits(e, "result"))

  errors <- c(.x[["errors"]], e[["errors"]])
  if (.x[["value"]] != e[["value"]]) {
    warning(
      "Returned value from and_then in the error_chain is not the same as the input value.",
      call. = FALSE
    )
  }
  return(error_chain(.x[["value"]], errors = errors))
}

and_then_other <- function(.x, .fn, ...) {
  UseMethod("and_then_other")
}

and_then_other.either <- function(.x, .fn, ...) {
  if (inherits(.x, "right")) {
    return(.x)
  } else {
    tmp <- .fn(.x)
    stopifnot("Function needs to return an 'either' type." = inherits(tmp, "either"))
    return(tmp)
  }
}

and_then_other.result <- function(.x, .fn, ...) {
  if (inherits(.x, "ok")) {
    return(.x)
  } else {
    tmp <- .fn(.x)
    stopifnot("Function needs to return a 'result' type." = inherits(tmp, "result"))
    return(tmp)
  }
}

or_else <- function(.x, .default, ...) {
  UseMethod("or_else")
}

or_else.optional <- function(.x, .default, ..., null_as_none = NULL) {
  if (inherits(.x, "none")) {
    return(.default)
  }
  if (is.null(null_as_none)) {
    null_as_none <- .x[["null_as_none"]]
  }

  if (!is.null(null_as_none) && null_as_none && is.null(.x[["value"]])) {
    return(.default)
  } else {
    return(.x[["value"]])
  }
}

or_else.either <- function(.x, .default, ...) {
  if (inherits(.x, "left")) {
    return(.default)
  } else {
    return(.x[["value"]])
  }
}

or_else.result <- function(.x, .default, ...) {
  if (inherits(.x, "err")) {
    return(.default)
  } else {
    return(.x[["value"]])
  }
}

collect <- function(.x) {
  UseMethod("collect")
}

collect.default <- function(.x) {
  return(.x)
}

collect.optional <- function(.x) {
  if (inherits(.x, "some")) {
    .x[["value"]]
  } else {
    NULL
  }
}

collect.either <- function(.x) {
  return(.x[["value"]])
}

collect.result <- function(.x) {
  return(.x[["value"]])
}

# Don't know that i like this but hey
collect.error_chain <- function(.x) {
  return(.x[["errors"]])
}

unwrap <- function(.x) {
  UseMethod("unwrap")
}

unwrap.default <- function(.x) {
  return(.x)
}

unwrap.optional <- function(.x) {
  if (inherits(.x,  "some")) {
    .x[["value"]]
  } else {
    NULL
  }
}

unwrap.either <- function(.x) {
  if (inherits(.x, "right")) {
    .x[["value"]]
  } else {
    NULL
  }
}

unwrap.result <- function(.x) {
  if (inherits(.x, "ok")) {
    .x[["value"]]
  } else {
    NULL
  }
}

unwrap.error_chain <- function(.x) {
  .x[["value"]]
}

unwrap_other <- function(.x) {
  UseMethod("unwrap_other")
}

unwrap_other.either <- function(.x) {
  if (inherits(.x, "left")) {
    .x[["value"]]
  } else {
    NULL
  }
}

unwrap_other.result <- function(.x) {
  if (inherits(.x, "err")) {
    .x[["value"]]
  } else {
    NULL
  }
}

unwrap_other.error_chain <- function(.x) {
  .x[["errors"]]
}

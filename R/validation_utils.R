validate_it <- function(value, message, FUN, ...) {
  r <- tryCatch(
    FUN(value, ...),
    error = function(err) {
      err
    }
  )

  if (is.null(value) && ((length(r) == 0) || is.null(r))) {
    return(NULL)
  } else if (inherits(r, "error")) {
    cli::cli_abort(c("Encountered an unexpected error validating {deparse(value)}.", parent = r))
  } else if (is.null(r) || (length(r) == 0)) {
    cli::cli_abort(c("Function returned NULL for value {deparse(value)}."))
  } else if (r) {
    return(NULL)
  } else {
    bullets <- names(message)
    value <- deparse(value)

    message <- vapply(
      message,
      FUN.VALUE = "string",
      FUN = \(x) {cli::format_inline(x, .envir = parent.frame(2))},
      USE.NAMES = FALSE
    )
    if (!is.null(bullets)) {
      bullets[is.na(bullets)] <- " "
      bullets[bullets == ""] <- " "
      names(message) = bullets
    }
    return(message)
  }
}


validate_many <- function(value, fns, ...) {
  errors <- list()

  for (fn in fns) {
    fn_err <- fn(value)
    if (is.null(fn_err)) {
      next
    } else if (!is.character(fn_err)) {
      cli::cli_abort("Fn {deparse(fn)} did not return a character or NULL.")
    }

    errors[[length(errors) + 1]] <- fn_err
  }

  if (length(errors) == 0) {
    return(NULL)
  }

  errors
}

validate_all <- function(value, fns, ..., message = NULL) {
  errors <- validate_many(value, fns, ...)

  if (length(errors) == 0) {
    return(NULL)
  } else if (length(errors) == 1) {
    return(unlist(errors))
  } else {
    errors <- unlist(lapply(errors, FUN = cli::format_bullets_raw))
  }

  if (is.null(message)) {
    message <- c("Encountered the following unsatisfied requirements:")
  }

  bullets <- names(message)
  value <- deparse(value)

  message <- vapply(
    message,
    FUN.VALUE = "string",
    FUN = \(x) {cli::format_inline(x, .envir = parent.frame(2))},
    USE.NAMES = FALSE
  )
  if (!is.null(bullets)) {
    bullets[is.na(bullets)] <- " "
    bullets[bullets == ""] <- " "
    names(message) = bullets
  }

  names(errors) <- rep(" ", length(errors))
  message <- c(message, errors)
  return(message)
}


validate_any <- function(value, fns, ..., message = NULL) {
  errors <- validate_many(value, fns, ...)

  if (length(errors) == 0) {
    return(NULL)
  } else if (length(errors) < length(fns)) {
    return(NULL)
  } else if (length(errors) == 1) {
    return(unlist(errors))
  } else {
    errors <- unlist(errors)
  }

  if (is.null(message)) {
    message <- c("At least one of the following requirements must be satisfied:")
  }

  bullets <- names(message)
  value <- deparse(value)

  message <- vapply(
    message,
    FUN.VALUE = "string",
    FUN = \(x) {cli::format_inline(x, .envir = parent.frame(2))},
    USE.NAMES = FALSE
  )
  if (!is.null(bullets)) {
    bullets[is.na(bullets)] <- " "
    bullets[bullets == ""] <- " "
    names(message) = bullets
  }

  errors <- cli::format_bullets_raw(errors)
  names(errors) <- rep(" ", length(errors))
  message <- c(message, errors)
  return(message)
}


not_null <- function(v) {
  !is.null(v)
}

not_na <- function(v) {
  !any(is.na(v))
}

is_single <- function(v) {
  if (is.null(v)) {
    return(TRUE)
  }
  length(v) == 1
}

is_string <- function(v) {
  not_null(v) && is_single(v) && is.character(v)
}

is_bool <- function(b) {
  not_null(b) && is_single(b) && is.logical(b)
}

is_integer <- function(i) {
  if (is_single(i) && is.character(i) && grepl("[[:digit:]][[:digit:]]*", i)) {
    i <- as.integer(i)
  }
  suppressWarnings(not_null(i) && not_na(i) && is_single(i) && is.numeric(i) && (i == as.integer(i)))
}

is_in <- function(v, options) {
  not_null(v) && is_single(v) && (v %in% options)
}

doesnt_raise_error <- function(expr, echo = FALSE) {
  tryCatch(
    { expr; return(TRUE) },
    error = function(err) {
      if (echo) {print(err)}
      return(FALSE)
    }
  )
}

is_valid_css_unit <- function(v) {
  not_null(v) && is_single(v) && doesnt_raise_error(htmltools::validateCssUnit(v))
}


VALID_CSS_UNIT_TYPES <- c(
  "in", "cm", "mm", "ch",
  "em", "ex", "rem", "pt",
  "pc", "px", "vh", "vw",
  "vmin", "vmax"
)

is_valid_css_unit_type <- function(v) {
  not_null(v) && is_single(v) && is_in(
    v,
    VALID_CSS_UNIT_TYPES
  )
}

all_str_split <- function(v, sep, fn, ...) {
  if (!is_string(v)) {return(FALSE)}
  v2 <- strsplit(v, sep)[[1]]
  all(vapply(v2, FUN = fn, FUN.VALUE = TRUE, ...))
}

all_space_split <- function(v, fn, ...) {
  all_str_split(v, "[[:space:]][[:space:]]*", fn, ...)
}

all_comma_split <- function(v, fn, ...) {
  all_str_split(v, "[[:space:]]*,[[:space:]]*", fn, ...)
}

validate_not_null <- function(v) {
  validate_it(v, c("x" = "Expected a value but wasn't provided one."), not_null)
}

validate_is_single <- function(v) {
  validate_it(v, c("x" = "Value {value} has length > 1."), is_single)
}

validate_is_string <- function(v) {
  validate_it(v, c("x" = "Value {value} is not a string."), is_string)
}

validate_is_bool <- function(v) {
  validate_it(v, c("x" = "Value {value} is not a boolean/logical."), is_bool)
}

validate_is_integer <- function(v) {
  validate_it(v, c("x" = "Value {value} is not an integer."), is_integer)
}

validate_is_in <- function(v, options) {
  validate_it(
    v,
    c("x" = "Value {.str value} is not in {.or {.str {options}}}."),
    is_in,
    options = options
  )
}

validate_is_string_literal <- function(v, literal) {
  validate_it(
    v,
    c("x" = cli::format_inline("Value {.str v} is not {.str literal}.")),
    \(x, ...) {x == literal}
  )
}

validate_integer_less_than <- function(v, max_) {
  r <- validate_is_integer(v)
  if (!is.null(r)) {
    return(r)
  }
  validate_it(
    v,
    c("x" = "Value {value} is greater than {max_}."),
    function(x, max_) {x < max_},
    max_ = max_
  )
}

validate_less_than <- function(v, max_) {
  validate_it(
    v,
    c("x" = "Value {value} is greater than {max_}."),
    function(x, max_) {x < max_},
    max_ = max_
  )
}

validate_is_valid_css_unit <- function(v) {
  validate_it(v, "Value {value} is not a valid CSS unit.", is_valid_css_unit)
}

validate_is_valid_css_unit_type <- function(v) {
  validate_it(
    v,
    c(
      "x" = "Value {value} is not a valid CSS unit type.",
      "i" = "Valid CSS unit types are: {.str {VALID_CSS_UNIT_TYPES}}."
    ),
    is_valid_css_unit_type
  )
}

validate_all_str_split <- function(value, sep, fn, ..., message = NULL) {
  errors <- validate_is_string(value)
  if (!is.null(errors)) {
    return(errors)
  }
  value <- strsplit(value, sep)[[1]]
  errors <- lapply(value, FUN = fn, ...)
  errors <- drop_nulls(errors)
  if (length(errors) == 0) {
    return(NULL)
  }

  if (is.null(message)) {
    message <- "{first_word} of the elements {satisfy} requirements:"
  }

  first_word <- if (length(message) == 1) {
    "One"
  } else if (length(message) == length(value)) {
    "None"
  } else {
    "Some"
  }
  satisfy <- if (length(message) == length(value)) {"satisfied"} else {"didn't satisfy"}

  bullets <- names(message)
  value <- deparse(value)

  message <- vapply(
    message,
    FUN.VALUE = "string",
    FUN = \(x) {cli::format_inline(x, .envir = parent.frame(2))},
    USE.NAMES = FALSE
  )
  if (!is.null(bullets)) {
    names(message) = bullets
  }

  errors <- unlist(lapply(errors, FUN  = cli::format_bullets_raw))
  names(errors) <- rep(" ", length(errors))
  message <- c(message, errors)
  return(message)
}


validate_all_space_split <- function(value, fn, ...) {
  message <- validate_all_str_split(
    value,
    "[[:space:]][[:space:]]*",
    fn,
    ...,
    message = "{first_word} of the space separated elements {satisfy} requirements:"
  )
  if (is.null(message)) {
    return(NULL)
  }

  return(message)
}


validate_all_comma_split <- function(value, fn, ...) {
  message <- validate_all_str_split(
    value,
    "[[:space:]]*,[[:space:]]*",
    fn,
    ...,
    message = "{first_word} of the comma separated elements {satisfy} requirements:"
  )
  if (is.null(message)) {
    return(NULL)
  }

  return(message)
}


validate_option <- function(option, value, fns, ..., ignore_null = TRUE, parent = NULL, call = rlang::caller_env()) {
  if (ignore_null && is.null(value)) {
    return(NULL)
  }
  errors <- list()

  if ((length(fns) == 1) && inherits(fns, "function")) {
    fns <- list(fns)
  }
  for (fn in fns) {
    fn_err <- fn(value)
    errors[[length(errors) + 1]] <- fn_err
  }

  if (length(errors) == 0) {
    return(NULL)
  }

  errors <- unlist(lapply(errors, FUN = function(x) {
    if (not_null(names(x)) && (names(x)[1] == "")) {
      names(x)[1] <- "!"
    }
    cli::format_bullets_raw(x)
  }))

  names(errors) <- rep("  ", length(errors))
  return(c(
    "!" = cli::format_inline("Invalid gridstack option {.var {option}} provided."),
    errors
  ))
}

eval_quo <- function(value, f_quo, ...) {
  f_exp <- rlang::quo_get_expr(f_quo)
  if (rlang::is_symbol(f_exp)) {
    f_chr <- f_exp
  } else {
    f_chr <- paste0(deparse(f_exp[[1]]), collapse = "")
  }
  f_env <- rlang::quo_get_env(f_quo)

  f <- get(f_chr, envir = f_env, mode = "function")
  if (rlang::is_symbol(f_exp)) {
    return(f(value, ...))
  }

  # It's necessary to remove positional arguments to avoid them being
  # interpreted as the first value in call_match later.
  cargs <- rlang::call_args(f_exp)
  positional_args <- cargs[names(cargs) == ""]

  f_exp <- f_exp[c(TRUE, names(cargs) != "")]

  named_args <- rlang::call_match(call = f_exp, fn = f)
  named_args <- as.list(named_args)[-1]
  if (length(positional_args) > 0) {
    named_args <- c(positional_args, named_args)
  }

  return(eval(rlang::call2(f, value, !!!named_args, ...)))
}


result_err <- function(value, FUN, ...) {
  if (is(value, "error")) {
    return(value)
  } else if (is(value, "result")) {
    if (!is.null(value$error)) {
      return(simpleError(value$error))
    } else {
      value <- value$value
    }
  }

  f_quo <- rlang::enquo(FUN)
  value <- eval_quo(value, f_quo, ...)

  if (is(value, "error")) {
    return(value)
  } else if (is(value, "result")) {
    if (!is.null(value$error)) {
      return(value$error)
    } else {
      value <- value$value
    }
  }

  return(value)
}

`%?%` <- result_err


result_type <- function(value = NULL, error = NULL) {
  r <- list(value = value, error = error)
  class(r) <- c("result", class(r))

  if (is(r$error, "error")) {
    r$error <- r$error$message
  }
  return(r)
}

test_chain <- function(value, FUN, ...) {
  if (is.null(value)) {
    return(result_type())
  } else if (is(value, "error")) {
    return(result_type(error = value))
  } else if (!is(value, "result")) {
    value <- result_type(value = value)
  }

  if (is.null(value$value)) {
    return(value)
  }

  f_quo <- rlang::enquo(FUN)
  result <- eval_quo(value$value, f_quo)
  print(result)

  if (is(result, "error")) {
    if (!is.null(value$error)) {
      error <- paste(value$error, result$message)
    } else {
      error <- result$message
    }
    value <- result_type(value = value$value, error = error)
  }
  return(value)
}

`%|?%` <- test_chain

test_collect <- function(value) {
  if (is.null(value)) {
    return(NULL)
  } else if(is(value, "error")) {
    return(value)
  } else if (is(value, "result")) {
    return(maybe_null(value$error, simpleError))
  } else {
    return(NULL)
  }
}

test_raise <- function(value, message = NULL) {
  print("test_raise")
  print(message)
  raise_it(test_collect(value), message)
  return(NULL)
}

test_any <- function(v, ...) {
  #print(paste("v", v))
  errors <- c()

  if (is(v, "error")) {
    return(result_type(error = v))
  } else if (is(v, "result")) {
    if (!is.null(v$error)) {
      errors <- c(errors, v$error)
    }

    if (is(v$value, "error")) {
      errors <- c(errors, v$value)
      return(result_type(error = errors))
    }

    v <- v$value
  }

  FUNS = rlang::list2(rlang::enquos(...))

  for (fn in FUNS[[1]]) {
    result <- eval_quo(v, fn)
    if (is(result, "error")) {
      errors <- c(errors, result$message)
    }
  }

  if(length(errors) == length(FUNS[[1]])) {
    return(result_type(value = v, error = paste(errors, collapse = "\n")))
  } else {
    return(result_type(value = v))
  }
}

test_all <- function(v, ...) {
  errors <- c()

  if (is(v, "error")) {
    return(result_type(error = v))
  } else if (is(v, "result")) {
    if (!is.null(v$error)) {
      errors <- c(errors, v$error)
    }

    if (is(v$value, "error")) {
      errors <- c(errors, v$value)
      return(result_type(error = errors))
    }

    v <- v$value
  }

  FUNS = rlang::list2(rlang::enquos(...))

  for (fn in FUNS[[1]]) {
    result <- eval_quo(v, fn)
    if (is(result, "error")) {
      errors <- c(errors, result$message)
    }
  }

  if(length(errors) > 0) {
    return(result_type(value = v, error = paste(errors, collapse = "\n")))
  } else {
    return(result_type(value = v))
  }
}


wrap_result_err <- function(FUN, ...) {
  function(value) {result_err(value, FUN, ...)}
}

maybe_null <- function(value, FUN, ...) {
  if (is.null(value)) {
    return(value)
  }
  f_quo <- rlang::enquo(FUN)
  f_exp <- rlang::quo_get_expr(f_quo)

  if (is(f_exp, "name")) {
    return(FUN(value, ...))
  }

  f_chr <- deparse(f_exp[[1]])
  f_env <- rlang::quo_get_env(f_quo)

  f <- get(f_chr, envir = f_env)
  q_ex_std <- rlang::call_match(call = f_exp, fn = f)
  expr_ls <- as.list(q_ex_std)

  return(eval(rlang::call2(f, value, !!!expr_ls[-1], ...)))
}

`%maybe%` <- maybe_null

wrap_maybe_null <- function(FUN, ...) {
  function(value) {maybe_null(value, FUN, ...)}
}

raise_it <- function(value, message = NULL) {
  print("raise it")
  print(message)
  err = NULL
  if (is(value, "error")) {
    err = value$message
  } else if (is(value, "result")) {
    if (!is.null(value$error)) {
      err = value$error
    } else if (is(value$value, "error")) {
      err = value$value$message
    } else {
      return(value$value)
    }
  }

  if (is.null(err)) {
    return(value)
  } else if (is.null(message)) {
    stop(simpleError(err))
  } else {
    stop(simpleError(paste(message, err, sep = "\n")))
  }
}

test_it <- function(v, FUN, message, ...) {
  r <- tryCatch(
    FUN(v, ...),
    error = function(err) {
      NULL
    }
  )

  if (is.null(v) & ((length(r) == 0) | is.null(r))) {
    return(v)
  } else if (is.null(r) | (length(r) == 0)) {
    return(simpleError(try_sprintf(message, deparse(v))))
  } else if (r) {
    return(v)
  } else {
    return(simpleError(try_sprintf(message, deparse(v))))
  }
}

not_null <- function(v) {
  !is.null(v)
}

test_not_null <- function(v) {
  test_it(v, not_null, "Expected a value but wasn't provided one.")
}

is_single <- function(v) {
  if (is.null(v)) {
    return(TRUE)
  }
  length(v) == 1
}

test_is_single <- function(v) {
  test_it(v, is_single, "Expecting a single value, but got %s.")
}

is_string <- function(v) {
  is_single(v) & is.character(v)
}

test_is_string <- function(v) {
  test_it(v, is_string, "Expecting a string, but got %s.")
}

is_bool <- function(b) {
  is_single(b) & is.logical(b)
}

test_is_bool <- function(v) {
  test_it(v, is_bool, "Expecting a boolean, but got %s.")
}

is_integer <- function(i) {
  suppressWarnings(is_single(i) & is.numeric(i) & (i == as.integer(i)))
}

test_is_integer <- function(v) {
  test_it(v, is_integer, "Expecting an integer, but got %s.")
}

is_in <- function(v, options) {
  is_single(v) & (v %in% options)
}

test_is_in <- function(v, options) {
  test_it(
    v,
    is_in,
    sprintf("Expecting an value in %s, but got %%s.", deparse(options)),
    options = options
  )
}

test_less_than <- function(v, max_) {
  test_it(
    v,
    function(x) {x < max_},
    sprintf("Expecting a number less than %s, but got %s", deparse(max_), deparse(v))
  )
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

test_doesnt_raise_error <- function(expr, echo = FALSE) {
  tryCatch(
    {expr; return(NULL)},
    error = function(err) {
      if (echo) {print(err)}
      return(err)
    }
  )
}

is_valid_css_unit <- function(v) {
  raises_error(htmltools::validateCssUnit(v))
}

test_is_valid_css_unit <- function(v) {
  test_it(v, is_valid_css_unit, "Expecting a CSS unit, but got %s.")
}

is_valid_css_unit_type <- function(v) {
  is_in(
    v,
    c("in", "cm", "mm", "ch",
      "em", "ex", "rem", "pt",
      "pc", "px", "vh", "vw",
      "vmin", "vmax")
  )
}

test_is_valid_css_unit_type <- function(v) {
  test_it(
    v,
    is_valid_css_unit_type,
    "Expecting a CSS unit type (e.g. mm, em), but got %s."
  )
}

all_str_split <- function(v, sep, fn, ...) {
  if (!is_single(v)) {return(FALSE)}
  v2 <- strsplit(v, sep)[[1]]
  all(vapply(v2, FUN = fn, FUN.VALUE = TRUE, ...))
}

test_all_str_split <- function(v, sep, fn, ...) {
  result <- test_is_single(v)
  if (!is.null(result)) {
    return(v)
  }
  result <- strsplit(v, sep)[[1]]
  result <- lapply(result, FUN = fn, ...)
  result <- drop_nulls(result)
  if (length(result) == 0) {
    return(v)
  }
  simpleError(paste(result, collapse = "\n"))
}

all_space_split <- function(v, fn, ...) {
  all_str_split(v, "[[:space:]][[:space:]]*", fn, ...)
}

test_all_space_split <- function(v, fn, ...) {
  message <- test_all_str_split(v, "[[:space:]][[:space:]]*", fn, ...)
  if (is.null(message)) {
    return(v)
  }
  simpleError(sprintf(
    "At least one of the values in a space separated list had a problem.\n%s",
    message
  ))
}

all_comma_split <- function(v, fn, ...) {
  all_str_split(v, "[[:space:]]*,[[:space:]]*", fn, ...)
}

test_all_comma_split <- function(v, fn, ...) {
  message <- test_all_str_split(v, "[[:space:]]*,[[:space:]]*", fn, ...)
  if (is.null(message)) {
    return(v)
  }
  simpleError(sprintf(
    "At least one of the values in a comma separated list had a problem.\n%s",
    message
  ))
}

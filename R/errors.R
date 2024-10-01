#tryCatch(tryCatch(rlang::abort("test", class = "myerr"), error = function(x) {rlang::abort(c("yeah", "what"), parent = x)} ), error = function(y) {rlang::abort("nah", parent = y)})

stop_invalid_argument <- function(argument, ..., parent = NULL, call = rlang::caller_env()) {
  rlang::abort(
    cli::format_inline("Invalid gridstack option {.var {argument}} given."),
    argument = argument,
    class = "windowstackr_error_invalid_argument",
    parent = parent,
    call = call
  )
}

stop_multiple_errors <- function(errors, ..., parent = NULL, call = rlang::caller_env()) {
  if (length(errors) == 1) {
    error <- errors[[1]]
    if (is.null(error$parent) && !is.null(parent)) {
      error$parent <- parent
    }
    rlang::cnd_signal(error)
  }
  messages <- unlist(lapply(
    errors,
    FUN = function(msg) {
      messages <- rlang::cnd_message(msg, prefix = FALSE)
      names(messages)[1] <- ""
      if (length(messages) > 1) {
        names(messages)[2:length(messages)] <- rep(" ", length(messages) - 1)
      }
      messages
    }
  ))

  rlang::abort(
    c("Encountered multiple errors.", messages),
    class = "windowstackr_error_multiple_errors",
    parent = parent,
    call = call
  )
}

stop_chain_multiple_errors <- function(errors, ..., reverse = FALSE, parent = NULL) {
  if (length(errors) == 1) {
    error <- errors[[1]]
    if (is.null(error$parent) && !is.null(parent)) {
      error$parent <- parent
    }
    rlang::cnd_signal(error)
  }

  if (reverse) {
    errors <- rev(errors)
  }

  error <- Reduce(function(l, r) {r$parent <- l; r}, errors)
  if (!is.null(parent)) {
    parent$parent <- error
    error <- parent
  }

  rlang::cnd_signal(error)
}


test_field <- function(argument, value, fn, ..., call = rlang::caller_env()) {
  rlang::try_fetch(
    {
      fn(value, ...)
    },
    error = function(err) {
      stop_invalid_argument(argument, parent = err)
    }
  )
}

test_is_integer <- function(var, call = NULL) {
  is_int <- is.integer(var) || (is.numeric(var) && (as.numeric(var) == as.integer(var)))

  if (!is_int) {
    cli::cli_abort(
      c(
        "i" = "Value should be an integer.",
        "x" = "You've supplied {.code {var}}."
      ),
      call = call
    )
  }
  return(var)
}

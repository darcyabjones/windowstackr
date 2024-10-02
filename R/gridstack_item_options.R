#' See: https://github.com/gridstack/gridstack.js/blob/2ba4d412f348119e657a1d51a27fea360a2a9993/src/types.ts#L318
gridstack_item_options <- function(
  ...,
  sizeToContent = TRUE,
  w = 6,
  h = 2,
  autoPosition = NULL,
  minW = NULL,
  maxW = NULL,
  noResize = NULL,
  noMove = NULL,
  locked = NULL,
  id = NULL,
  content = NULL,
  resizeToContentParent = NULL,
  subGridOpts = NULL
) {
  if (inherits(subGridOpts, "gridstack_options")) {
    validate_gridstack_options(subGridOpts)
  } else if (!is.null(subGridOpts)) {
    subGridOpts <- do.call(gridstack_options, subGridOpts)
  }

  opt <- structure(
    list(
      sizeToContent = sizeToContent,
      w = w,
      h = h,
      autoPosition = autoPosition,
      minW = minW,
      maxW = maxW,
      noResize = noResize,
      noMove = noMove,
      locked = locked,
      id = id,
      content = content,
      resizeToContentParent = resizeToContentParent,
      subGridOpts = subGridOpts
    ),
    ...,
    class = "gridstack_item_options"
  )
  validate_gridstack_item_options(opt)
  opt
}



validate_gridstack_item_options <- function(opt, call = NULL) {
  if (is.null(opt)) {return(TRUE)}
  errors <- list(
    validate_option("autoPosition", opt[["autoPosition"]], validate_is_bool),
    validate_option("minW", opt[["minW"]], validate_is_integer),
    validate_option("maxW", opt[["maxW"]], validate_is_integer),
    validate_option("noResize", opt[["noResize"]], validate_is_bool),
    validate_option("noMove", opt[["noMove"]], validate_is_bool),
    validate_option("locked", opt[["locked"]], validate_is_bool),
    validate_option("id", opt[["id"]], validate_is_string),
    validate_option("content", opt[["content"]], validate_is_string),
    validate_option(
      "sizeToContent",
      opt[["sizeToContent"]],
      \(v) {validate_any(
        v,
        list(
          validate_is_bool,
          validate_is_string
        )
    )}),
    validate_option("resizeToContentParent", opt[["resizeToContentParent"]], validate_is_string)
  )
  errors <- drop_nulls(errors)

  if (length(errors) == 0) {
    return(NULL)
  }

  errors <- unlist(lapply(errors, FUN = function(x) {
    if (not_null(names(x)) && (names(x)[1] == "")) {
      names(x)[1] <- "!"
    }
    cli::format_bullets_raw(x)
  }))

  names(errors) <- rep(" ", length(errors))

  cli::cli_abort(
    c(
      "!" = cli::format_inline("Invalid gridstack item options provided."),
      errors
    ),
    call = call
  )
  return(NULL)
}

as.list.gridstack_item_options <- function(opt) {
  drop_nulls(opt) |>
    drop_empty_sublists("subGridOpts")
}

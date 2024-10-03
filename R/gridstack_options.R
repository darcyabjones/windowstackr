new_gridstack_options <- function(
    ...,
    animate = NULL,
    auto = NULL,
    cellHeight = NULL,
    cellHeightThrottle = NULL,
    cellHeightUnit = NULL,
    column = NULL,
    row = NULL,
    maxRow = NULL,
    minRow = NULL,
    float = NULL,
    disableDrag = NULL,
    disableResize = NULL,
    draggable = NULL,
    handleClass = NULL,
    resizable = NULL,
    alwaysShowResizeHandle = NULL,
    margin = NULL,
    marginUnit = NULL,
    removable = NULL,
    removableOptions = NULL,
    itemClass = NULL,
    placeholderClass = NULL,
    placeholderText = NULL,
    rtl = NULL,
    sizeToContent = NULL,
    staticGrid = NULL,
    styleInHead = NULL,
    subGridOpts = NULL,
    class = character()
  ) {
  structure(
    list(
      animate = animate,
      auto = auto,
      cellHeight = cellHeight,
      cellHeightThrottle = cellHeightThrottle,
      cellHeightUnit = cellHeightUnit,
      column = column,
      row = row,
      maxRow = maxRow,
      minRow = minRow,
      float = float,
      disableDrag = disableDrag,
      disableResize = disableResize,
      draggable = draggable,
      handleClass = handleClass,
      resizable = resizable,
      alwaysShowResizeHandle = alwaysShowResizeHandle,
      margin = margin,
      marginUnit = marginUnit,
      removable = removable,
      removableOptions = removableOptions,
      itemClass = itemClass,
      placeholderClass = placeholderClass,
      placeholderText = placeholderText,
      rtl = rtl,
      sizeToContent = sizeToContent,
      staticGrid = staticGrid,
      styleInHead = styleInHead,
      subGridOpts = subGridOpts
    ),
    ...,
    class = c(class, "gridstack_options")
  )
}

gridstack_options <- function(
  ...,
  animate = TRUE,
  auto = TRUE,
  float = NULL,
  cellHeight = 'initial',
  cellHeightThrottle = 100,
  cellHeightUnit = 'px',
  column = 6,
  draggable = gridstack_draggable_options(),
  handleClass = NULL,
  itemClass = 'grid-stack-item',
  margin = "5px 5px 5px 5px",
  marginUnit = 'px',
  row = NULL,
  maxRow = 0,
  minRow = 2,
  placeholderClass = 'window-stack-placeholder',
  placeholderText = '',
  removable = NULL,
  removableOptions = gridstack_removable_options(),
  resizable = gridstack_resizable_options(),
  alwaysShowResizeHandle = 'mobile',
  disableResize = FALSE,
  disableDrag = FALSE,
  rtl = TRUE,
  sizeToContent = NULL,
  staticGrid = NULL,
  styleInHead = NULL,
  subGridOpts = NULL
) {
  if (inherits(draggable, "gridstack_draggable_options")) {
    validate_gridstack_draggable_options(draggable)
  } else if (!is.null(draggable)) {
    draggable <- do.call(gridstack_draggable_options, draggable)
  }


  if (inherits(resizable, "gridstack_resizable_options")) {
    validate_gridstack_resizable_options(resizable)
  } else if (!is.null(resizable)) {
    resizable <- do.call(gridstack_resizable_options, resizable)
  }

  if (!inherits(removableOptions, "gridstack_resizable_options")) {
    validate_gridstack_removable_options(removableOptions)
  } else if (!is.null(removableOptions)) {
    removableOptions <- do.call(gridstack_resizable_options, removableOptions)
  }

  if (inherits(subGridOpts, "gridstack_options")) {
    validate_gridstack_options(subGridOpts)
  } else if (!is.null(subGridOpts)) {
    subGridOpts <- do.call(gridstack_options, subGridOpts)
  }

  opt <- new_gridstack_options(
    animate = animate,
    auto = auto,
    cellHeight = cellHeight,
    cellHeightThrottle = cellHeightThrottle,
    cellHeightUnit = cellHeightUnit,
    column = column,
    row = row,
    maxRow = maxRow,
    minRow = minRow,
    float = float,
    disableDrag = disableDrag,
    disableResize = disableResize,
    draggable = draggable,
    handleClass = handleClass,
    resizable = resizable,
    alwaysShowResizeHandle = alwaysShowResizeHandle,
    margin = margin,
    marginUnit = marginUnit,
    removable = removable,
    removableOptions = removableOptions,
    itemClass = itemClass,
    placeholderClass = placeholderClass,
    placeholderText = placeholderText,
    rtl = rtl,
    sizeToContent = sizeToContent,
    staticGrid = staticGrid,
    styleInHead = styleInHead,
    subGridOpts = subGridOpts
  )
  validate_gridstack_options(opt)
  opt
}


validate_gridstack_options <- function(opt, call = NULL) {
  errors <- list(
    validate_option("animate", opt[["animate"]], validate_is_bool),
    validate_option("auto", opt[["auto"]], validate_is_bool),
    validate_option(
      "cellHeight",
      opt[["cellHeight"]],
      \(v) {validate_any(v, list(
        validate_is_valid_css_unit,
        \(v2) {validate_is_in(v2, c("auto", "initial"))}
      ))}
    ),
    validate_option("cellHeightThrottle", opt[["cellHeightThrottle"]], validate_is_integer),
    validate_option("cellHeightUnit", opt[["cellHeightUnit"]], validate_is_valid_css_unit_type),
    validate_option(
      "column",
      opt[["column"]],
      \(v) {validate_any(v, list(
        \(v2) {validate_integer_less_than(v2, 12)},
        \(v3) {validate_is_string_literal(v3, "auto")}
      ))}
    ),
    validate_option("row", opt[["row"]], validate_is_integer),
    validate_option("maxRow", opt[["maxRow"]], validate_is_integer),
    validate_option("minRow", opt[["minRow"]], validate_is_integer),
    validate_option("float", opt[["float"]], validate_is_bool),
    validate_option("disableDrag", opt[["disableDrag"]], validate_is_bool),
    validate_option("disableresize", opt[["disableresize"]], validate_is_bool),
    validate_option("handleClass", opt[["handleClass"]], validate_is_string),
    validate_option(
      "alwaysShowResizeHandle",
      opt[["alwaysShowResizeHandle"]],
      \(v) {validate_any(v, list(
        validate_is_bool,
        \(v3) {validate_is_string_literal(v3, "mobile")}
      ))}
    ),
    validate_option(
      "margin",
      opt[["margin"]],
      \(v) {validate_all_space_split(v, validate_is_valid_css_unit)}
    ),
    validate_option("marginUnit", opt[["marginUnit"]], validate_is_valid_css_unit_type),
    validate_option(
      "removable",
      opt[["removable"]],
      \(v) {validate_any(v, list(
        validate_is_bool,
        validate_is_string
      ))}
    ),
    validate_option("itemClass", opt[["itemClass"]], validate_is_string),
    validate_option("placeholderClass", opt[["placeholderClass"]], validate_is_string),
    validate_option("placeholderText", opt[["placeholderText"]], validate_is_string),
    validate_option(
      "rtl",
      opt[["rtl"]],
      \(v) {validate_any(v, list(
        validate_is_bool,
        \(v2) {validate_is_string_literal(v2, "auto")}
      ))}
    ),
    validate_option("sizeToContent", opt[["sizeToContent"]], validate_is_bool),
    validate_option("staticGrid", opt[["staticGrid"]], validate_is_bool),
    validate_option("styleInHead", opt[["styleInHead"]], validate_is_bool)
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
      "!" = cli::format_inline("Invalid gridstack options provided."),
      errors
    ),
    call = call
  )
  return(NULL)
}

as.list.gridstack_options <- function(opt) {
  drop_nulls(opt) |>
    drop_empty_sublists("removableOptions") |>
    drop_empty_sublists("draggable") |>
    drop_empty_sublists("resizable") |>
    drop_empty_sublists("subGridOpts")
}

gridstack_draggable_options <- function(
  handle = '.windowstack-window-handle',
  appendTo = "body",
  pause = NULL,
  scroll = TRUE,
  cancel = NULL,
  ...
) {
  opt <- structure(
    list(
      handle = handle,
      appendTo = appendTo,
      pause = pause,
      scroll = scroll,
      cancel = cancel
    ),
    ...,
    class = "gridstack_draggable_options"
  )
  validate_gridstack_draggable_options(opt)
  opt
}

validate_gridstack_draggable_options <- function(opt, call = NULL) {
  if (is.null(opt)) {return(NULL)}

  errors <- list(
    validate_option("handle", opt[["handle"]], validate_is_string),
    validate_option("appendTo", opt[["appendTo"]], validate_is_string),
    validate_option(
      "pause",
      opt[["pause"]],
      \(v) {validate_any(v, list(
        validate_is_bool,
        validate_is_integer
      ))}
    ),
    validate_option("scroll", opt[["scroll"]], validate_is_bool),
    validate_option("cancel", opt[["cancel"]], validate_is_bool)
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
      "!" = cli::format_inline("Invalid compound gridstack option `draggable` provided."),
      errors
    ),
    call = call
  )
  return(NULL)
}


gridstack_resizable_options <- function(
  ...,
  handles = "w,sw,s,se,e",
  autoHide = NULL
) {
  opt <- structure(
    list(handles = handles, autoHide = autoHide),
    ...,
    class = "gridstack_resizable_options"
  )
  validate_gridstack_resizable_options(opt)
  opt
}

GRIDSTACK_RESIZE_HANDLE_OPTIONS <- c("n", "ne", "e", "se", "s", "sw", "w", "nw", "n")

validate_gridstack_resizable_options <- function(opt, call = NULL) {
  if (is.null(opt)) {return(TRUE)}
  errors <- list(
    validate_option("autoHide", opt[["autoHide"]], validate_is_bool),
    validate_option(
      "handles",
      opt[["handles"]],
      \(v) {validate_all_comma_split(v, \(v2) {validate_is_in(v2, GRIDSTACK_RESIZE_HANDLE_OPTIONS)})}
    )
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
      "!" = cli::format_inline("Invalid compound gridstack `resizable` options provided."),
      errors
    ),
    call = call
  )
  return(NULL)
}


gridstack_removable_options <- function(
  ...,
  accept = 'grid-stack-item',
  decline = 'grid-stack-non-removable'
) {
  opt <- structure(
    list(accept = accept, decline = decline),
    ...,
    class = "gridstack_removable_options"
  )
  validate_gridstack_removable_options(opt)
  opt
}


validate_gridstack_removable_options <- function(opt, call = NULL) {
  if (is.null(opt)) {return(TRUE)}
  errors <- list(
    validate_option("accept", opt[["accept"]], validate_is_string),
    validate_option("decline", opt[["decline"]], validate_is_string)
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
      "!" = cli::format_inline("Invalid compound gridstack `resizable` options provided."),
      errors
    ),
    call = call
  )
  return(NULL)
}

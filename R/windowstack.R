#' windowstack
#'
#' <Add Description>
#' https://github.com/gridstack/gridstack.js/blob/master/src/types.ts#L116
#'
#' @export
windowstack <- function(
  ...,
  width = NULL,
  height = NULL,
  id = NULL,
  item_defaults = list()
) {
  if (is.null(id)) {
    id = sprintf("windowstack-%s", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }

  args <- rlang::list2(...)
  argnames <- rlang::names2(args)
  options <- args[nzchar(argnames)]

  children <- args[!nzchar(argnames)]
  children <- htmltools::renderTags(children)

  is_shiny_input <- !is.null(id)

  options <- gridstack_options_check(options)

  # forward options using x
  x = list(
    html = children$html,
    options = options,
    item_defaults = list()
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'windowstack',
    x,
    width = width,
    height = height,
    package = 'windowstackr',
    elementId = id,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      viewer.fill = FALSE,
      knitr.figure = FALSE,
      viewer.suppress = FALSE,
      browser.external = TRUE,
      browser.fill = TRUE,
      padding = 5
    )
  )
}

#' Shiny bindings for windowstack
#'
#' Output and render functions for using windowstack within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a windowstack
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name windowstack-shiny
#'
#' @export
windowstackOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'windowstack', width, height, package = 'windowstackr')
}

#' @rdname windowstack-shiny
#' @export
renderWindowstack <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, windowstackOutput, env, quoted = TRUE)
}


gridstack_options_check <- function(options) {
  defaults <- gridstack_defaults()
  defaults <- defaults[!names(defaults) %in% options]

  gridstack_draggable_check <- function(li) {
    if (is.null(li)) {return(TRUE)}
    stopifnot(
      is_string(li[["handle"]]),
      is_string(li[["appendTo"]]),
      is.null(li[["pause"]]) | is_bool(li[["pause"]]) | is_integer(li[["pause"]]),
      is_bool(li[["scroll"]]),
      optional(is_string, li[["cancel"]])
    )
    return(TRUE)
  }

  gridstack_resizable_check <- function(li) {
    if (is.null(li)) {return(TRUE)}
    stopifnot(
      optional(is_bool, li[["autoHide"]]),
      all(
        is_string(li[["handles"]]),
        all_comma_split(is_in, li[["handles"]], options = c("n", "ne", "e", "se", "s", "sw", "w", "nw", "n"))
      )
    )
    return(TRUE)
  }

  test_gridstack_removable <- function(li) {
    if (is.null(li)) {return(TRUE)}
    test_all(
      (is_string, li[["accept"]] %|?% test_is_string()
      "decline" = optional(is_string, li[["decline"]])
    )
    return(TRUE)
  }

  options <- c(options, defaults)
  options[["animate"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid animate value. Expect a bool, got %s")

  options[["auto"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid auto value in gridstack options.")

  options[["cellHeight"]] %|?%
    test_any(
      test_is_valid_css_unit(),
      test_is_in(options = c("auto", "initial"))
    ) |>
    test_raise(message = "Invalid cellHeight value in gridstack options.")

  options[["cellHeightThrottle"]] %|?%
    test_is_integer() |>
    test_raise(message = "Invalid cellHeightThrottle value in gridstack options.")

  options[["cellHeightUnit"]] %|?%
    test_is_valid_css_unit_type() |>
    test_raise(message = "Invalid cellHeightUnit value in gridstack options.")

  options[["column"]] %|?%
    test_any(
      test_is_integer(),
      test_less_than(max_ = 12),
      test_is_in(options = c("auto"))
    ) |>
    test_raise(message = "Invalid column value in gridstack options.")

  options[["row"]] %|?%
    test_is_integer() |>
    test_raise(message = "Invalid row value in gridstack options.")

  options[["maxRow"]] %|?%
    test_is_integer() |>
    test_raise(message = "Invalid maxRow value in gridstack options.")

  options[["minRow"]] %|?%
    test_is_integer() |>
    test_raise(message = "Invalid minRow value in gridstack options.")

  options[["float"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid float value in gridstack options.")

  options[["disableDrag"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid disableDrag value in gridstack options.")

  options[["disableResize"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid disableResize value in gridstack options.")

  options[["draggable"]] %|?%
    test_it(FUN = gridstack_draggable_check, message = "Expect a specific list structure. See gridstack documentation.") |>
    test_raise(message = "Invalid draggable value in gridstack options.")

  options[["handleClass"]] %|?%
    test_is_string() |>
    test_raise(message = "Invalid handleClass value in gridstack options.")

  options[["resizable"]] %|?%
    test_it(FUN = gridstack_resizable_check, message = "Expect a specific list structure. See gridstack documentation.") |>
    test_raise(message = "Invalid resizable value in gridstack options.")

  options[["alwaysShowResizeHandle"]] %|?%
    test_any(
      test_is_bool(),
      test_is_in(options = c("mobile"))
    ) |>
    test_raise(message = "Invalid alwaysShowResizeHandle value in gridstack options.")

  options[["margin"]] %|?%
    test_all_space_split(fn = is_valid_css_unit()) |>
    test_raise(message = "Invalid margin value in gridstack options.")

  options[["marginUnit"]] %|?%
    test_is_valid_css_unit_type() |>
    test_raise(message = "Invalid marginUnit value in gridstack options.")

  options[["removable"]] %|?%
    test_any(
      test_is_bool(),
      test_is_string()
    ) |>
    test_raise(message = "Invalid removable value in gridstack options.")

  options[["removableOptions"]] %|?%
    test_it(FUN = gridstack_removable_check, message = "Expect a specific list structure. See gridstack documentation.") |>
    test_raise(message = "Invalid removableOptions value in gridstack options.")

  options[["itemClass"]] %|?%
    test_is_string() |>
    test_raise(message = "Invalid itemClass value in gridstack options.")

  options[["placeholderClass"]] %|?%
    test_is_string() |>
    test_raise(message = "Invalid placeholderClass value in gridstack options.")

  options[["placeholderText"]] %|?%
    test_is_string() |>
    test_raise(message = "Invalid placeholderText value in gridstack options.")

  options[["rtl"]] %|?%
    test_any(
      test_is_bool(),
      test_is_in(options = c("auto"))
    ) |>
    test_raise(message = "Invalid placeholderText value in gridstack options.")

  options[["sizeToContent"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid sizeToContent value in gridstack options.")

  options[["staticGrid"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid staticGrid value in gridstack options.")

  options[["styleInHead"]] %|?%
    test_is_bool() |>
    test_raise(message = "Invalid styleInHead value in gridstack options.")

  options <- drop_nulls(options) |>
    drop_empty_sublists("subGridOpts") |>
    drop_empty_sublists("removableOptions") |>
    drop_empty_sublists("draggable")

  return(options)
}


gridstack_defaults <- function() {
  list(
    alwaysShowResizeHandle = 'mobile',
    animate = TRUE,
    auto = TRUE,
    cellHeight = 'initial',
    cellHeightThrottle = 100,
    cellHeightUnit = 'px',
    column = 6,
    draggable = list(handle = '.windowstack-window-handle', appendTo = 'body', scroll = TRUE ),
    itemClass = 'grid-stack-item',
    margin = "5px 10px 5px 5px",
    marginUnit = 'px',
    maxRow = 0,
    minRow = 2,
    placeholderClass = 'window-stack-placeholder',
    placeholderText = '',
    removableOptions = list(accept = 'grid-stack-item', decline = 'grid-stack-non-removable'),
    resizable = list(handles = "w,sw,s,se,e"),
    disableResize = FALSE,
    disableDrag = FALSE,
    rtl = TRUE
  )
}


#' See: https://github.com/gridstack/gridstack.js/blob/2ba4d412f348119e657a1d51a27fea360a2a9993/src/types.ts#L318
gridstack_widget_defaults <- function() {
  list(
    sizeToContent = TRUE,
    w = 6,
    h = 2
  )
}


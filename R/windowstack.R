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
  gs_options = gridstack_options(),
  gs_item_options = gridstack_item_options()
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

  if (!inherits(gs_options, "gridstack_options") && not_null(gs_options)) {
    gs_options <- do.call(gridstack_options, gs_options)
  }

  if (!inherits(gs_item_options, "gridstack_item_options") && not_null(gs_item_options)) {
    gs_item_options <- do.call(gridstack_item_options, gs_item_options)

  }

  # forward options using x
  x = list(
    html = children$html,
    id = id,
    options = as.list(gs_options),
    item_defaults = as.list(gs_item_options)
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'windowstack',
    x,
    width = width,
    height = height,
    package = 'windowstackr',
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





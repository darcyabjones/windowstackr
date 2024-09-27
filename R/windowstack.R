#' <Add Title>
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

  options <- c(options, defaults)
  ## TODO, do some sanity checking.
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
    placeholderClass = 'grid-stack-placeholder',
    placeholderText = '',
    removableOptions = list(accept= 'grid-stack-item', decline = 'grid-stack-non-removable'),
    resizable = list(handles = "w,sw,s,se,e", autoHide = "auto"),
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

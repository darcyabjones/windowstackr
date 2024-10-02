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
  options <- gridstack_defaults()


  children <- args[!nzchar(argnames)]
  children <- htmltools::renderTags(children)

  is_shiny_input <- !is.null(id)

  # forward options using x
  x = list(
    html = children$html,
    head = children$head,
    options = options,
    id = id,
    item_defaults = list()
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'windowstack',
    x,
    width = width,
    height = height,
    package = 'windowstackr',
    dependencies = children$dependencies,
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
    margin = "10px", #"5px 10px 5px 5px",
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


windowstack_proxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  proxy <- list(id = id, session = session)
  class(proxy) <- "windowstack_proxy"
  return(proxy)
}

#' See: https://github.com/gridstack/gridstack.js/blob/2ba4d412f348119e657a1d51a27fea360a2a9993/src/types.ts#L318
gridstack_widget_defaults <- function() {
  list(
    sizeToContent = TRUE,
    w = 6,
    h = 2
  )
}

windowstack_make_window <- function(proxy, window) {
  gs_options <- gridstack_widget_defaults()
  shiny::insertUI(
    paste0("#", proxy$id),
    ui = window,
    where = "beforeEnd",
    session = proxy$session,
    immediate = TRUE
  )
  message <- list(id = proxy$id, window_id = window$attribs$id)
  proxy$session$sendCustomMessage("gridstack_make_window", message)
}

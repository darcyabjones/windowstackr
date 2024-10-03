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
    head = children$head,
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


windowstack_proxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  proxy <- list(id = id, session = session)
  class(proxy) <- "windowstack_proxy"
  return(proxy)
}

prepend_names <- function(options, prefix = "gs-", suffix = "") {
  opt <- options
  names(opt) <- paste0(prefix, names(opt), suffix)
  return(opt)
}

gs_item <- function(..., style = NULL, class = NULL, id = NULL) {
  if (is.null(id)) {
    id = sprintf("windowstack-%s-container", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }

  tag <- htmltools::div(
    ...,
    class = "grid-stack-item grid-stack-item-edit",
    id = id
  )

  tag <- htmltools::bindFillRole(tag, container = TRUE, item = TRUE)
  tag <- htmltools::tagAppendAttributes(tag, style = style, class = class)
  class(tag) <- c("gs_item", class(tag))
  return(tag)
}

add_gs_window <- function(
  proxy,
  window,
  ...,
  style = NULL,
  class = NULL,
  gs_options = gridstack_item_options()
) {
  if (!inherits(gs_options, "gridstack_item_options")) {
    gs_options <- do.call(gridstack_item_options, gs_options)
  }

  window_id <- window$attrib$id
  stopifnot(!is.null(window_id))

  if (is.null(gs_options$id)) {
    tag_id <- paste0(window_id, "-container")
    gs_options$id <- tag_id
  }

  tag <- gs_item(id = tag_id, style = style, class = class)
  add_gs_element(proxy, tag, as.list(gs_options))

  window <- htmltools::tagAppendAttributes(window, class = "grid-stack-item-content")
  shiny::insertUI(
    paste0("#", tag_id),
    ui = window,
    where = "beforeEnd",
    session = proxy$session
  )
  return(proxy)
}

add_gs_element <- function(
    proxy,
    element,
    options = NULL,
    gs_options = gridstack_item_options()
) {
  # Avoids missing dependencies
  stopifnot(inherits(element, "gs_item"))
  element_rendered <- htmltools::renderTags(element)

  message <- list(
    id = proxy$id,
    element = element_rendered$html,
    options = as.list(gs_options)
  )
  proxy$session$sendCustomMessage("gridstack_add_element", message)
  proxy
}

make_gs_element <- function(proxy, element) {
  shiny::insertUI(
    paste0("#", proxy$id),
    ui = window,
    where = "beforeEnd",
    session = proxy$session,
    immediate = TRUE
  )
  message <- list(id = proxy$id, element_id = element$attribs$id)
  proxy$session$sendCustomMessage("gridstack_make_element", message)
  proxy
}

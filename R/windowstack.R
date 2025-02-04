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
  children <- lapply(children, function(child) {
    if (!is.null(child$attrib$id)) {
      container_id <- paste0(child$id, "-container")
    } else {
      stop("All windows must have an id set.")
    }
    if (!inherits(child, "gs_item")) {
      child <- htmltools::tagAppendAttributes(child, class = "grid-stack-item-content")
      child <- gs_item(child, id = container_id)
    }
    child
  })

  children <- htmltools::renderTags(children)

  if (!inherits(gs_options, "gridstack_options") && not_null(gs_options)) {
    gs_options <- do.call(gridstack_options, gs_options)
  }

  if (!inherits(gs_item_options, "gridstack_item_options") && not_null(gs_item_options)) {
    gs_item_options <- do.call(gridstack_item_options, gs_item_options)
  }

  # forward options using x
  x = list(
    id = id,
    html = children$html,
    head = children$head,
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
windowstackOutput <- function(outputId, ..., width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'windowstack', width, height, package = 'windowstackr')
}

#' @rdname windowstack-shiny
#' @export
renderWindowstack <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, windowstackOutput, env, quoted = TRUE)
}


windowstack_proxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  validate_arguments(list(
    validate_argument("id", id, validate_is_string),
    validate_argument("session", session, \(v) {validate_inherits(v, "ShinySession")})
  ))
  proxy <- list(id = id, session = session)
  class(proxy) <- "windowstack_proxy"
  return(proxy)
}

add_window <- function(
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

  validate_arguments(list(
    validate_argument(
      "proxy",
      proxy,
      \(v) {validate_inherits(v, "windowstack_proxy")}
    ),
    validate_argument(
      "window",
      window,
      \(v) {validate_all(v, list(
        \(v1) {validate_inherits(v1, "shiny.tag")},
        \(v2) {validate_inherits(v2, "windowstackr_window")}
      ))}
    )
  ))

  window_id <- window$attrib$id
  stopifnot(!is.null(window_id))
  window <- htmltools::tagAppendAttributes(window, class = "grid-stack-item-content")

  if (is.null(gs_options$id)) {
    tag_id <- paste0(window_id, "-container")
    gs_options$id <- tag_id
  }

  tag <- gs_item(window, id = tag_id, style = style, class = class, !!!as_html_attributes(gs_options))

  gs_make_element(proxy, tag, as.list(gs_options))
  return(proxy)
}

gs_item <- function(..., style = NULL, class = NULL, id = NULL) {
  if (is.null(id)) {
    id = sprintf("windowstack-%s-container", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }
  tag <- htmltools::div(
    ...,
    class = "grid-stack-item grid-stack-item-edit",
    style = style,
    id = id
  )

  tag <- htmltools::bindFillRole(tag, container = TRUE, item = TRUE)
  tag <- htmltools::tagAppendAttributes(tag, class = class)
  class(tag) <- c("gs_item", class(tag))
  return(tag)
}

gs_add_element <- function(
    proxy,
    element,
    ...,
    options = NULL,
    gs_options = gridstack_item_options()
) {
  validate_arguments(list(
    validate_argument(
      "proxy",
      proxy,
      \(v) {validate_inherits(v, "windowstack_proxy")}
    ),
    validate_argument(
      "element",
      element,
      \(v) {validate_all(v, list(
        \(v1) {validate_inherits(v1, "shiny.tag")},
        \(v2) {validate_inherits(v2, "gs_item")}
      ))}
    )
  ))
  element_rendered <- htmltools::renderTags(element)

  if ((!"id" %in% names(gs_options)) || is.null(gs_options[["id"]])) {
    gs_options$id <- element$attrib$id
  }

  message <- list(
    id = pound(proxy$id),
    element = element_rendered$html,
    options = as.list(gs_options)
  )

  proxy$session$sendCustomMessage("gridstack_add_element", message)
  proxy
}

gs_make_element <- function(proxy, element, ...) {
  validate_arguments(list(
    validate_argument(
      "proxy",
      proxy,
      \(v) {validate_inherits(v, "windowstack_proxy")}
    ),
    validate_argument(
      "element",
      element,
      \(v) {validate_inherits(v, "shiny.tag")}
    )
  ))

  shiny::insertUI(
    pound(proxy$id),
    ui = element,
    where = "beforeEnd",
    session = proxy$session,
    immediate = TRUE
  )

  message <- list(id = pound(proxy$id), element_id = pound(element$attribs$id))
  proxy$session$sendCustomMessage("gridstack_make_element", message)
  proxy
}

gs_set_column <- function(
  proxy,
  column,
  layout = c('moveScale', 'list', 'compact', 'move', 'scale', 'none'),
  ...
) {
  layout <- rlang::arg_match(layout)
  validate_arguments(list(
    validate_argument(
      "proxy",
      proxy,
      \(v) {validate_inherits(v, "windowstack_proxy")}
    ),
    validate_argument("column", column, \(v) {validate_integer_between(v, 0, 12)})
  ))
  message <- list(id = pound(proxy$id), column = column, layout = layout)
  proxy$session$sendCustomMessage("gridstack_set_column", message)
  proxy
}

gs_set_float <- function(proxy, float, ...) {
  validate_arguments(list(
    validate_argument(
      "proxy",
      proxy,
      \(v) {validate_inherits(v, "windowstack_proxy")}
    ),
    validate_argument("float", float, validate_is_bool)
  ))
  message <- list(id = pound(proxy$id), float = float)
  proxy$session$sendCustomMessage("gridstack_set_float", message)
  proxy
}

gs_set_float <- function(proxy, float, ...) {
  validate_arguments(list(
    validate_argument(
      "proxy",
      proxy,
      \(v) {validate_inherits(v, "windowstack_proxy")}
    ),
    validate_argument("float", float, validate_is_bool)
  ))

  message <- list(id = pound(proxy$id), float = float)
  proxy$session$sendCustomMessage("gridstack_set_float", message)
  proxy
}

gs_set_margin <- function(proxy, margin, ...) {
  validate_arguments(list(
    validate_argument(
      "proxy",
      proxy,
      \(v) {validate_inherits(v, "windowstack_proxy")}
    ),
    validate_argument(
      "margin",
      margin,
      \(v) {validate_all_space_split(v, validate_is_valid_css_unit)}
    )
  ))

  message <- list(id = pound(proxy$id), margin = margin)
  proxy$session$sendCustomMessage("gridstack_set_margin", message)
  proxy
}

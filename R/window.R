#' <Add Title>
#'
#' <Add Description>
#'
#' @export
window <- function(
  ...,
  full_screen = FALSE,
  fill = TRUE,
  class = NULL,
  style = NULL,
  wrapper = card_body,
  id = NULL
) {
  if (is.null(id)) {
    id = sprintf("windowstack-window-%s", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }

  args <- rlang::list2(...)
  argnames <- rlang::names2(args)

  attribs <- args[nzchar(argnames)]
  children <- args[!nzchar(argnames)]

  is_shiny_input <- !is.null(id)

  if (is.null(style)) {
    style <- htmltools::css(
      min_height = htmltools::validateCssUnit(NULL)
    )
  }

  tag <- htmltools::tags$div(
    id = id,
    class = "card windowstack-window",
    class = if (is_shiny_input) "windowstack-window-input",
    style = style,
    "data-full-screen" = if (full_screen) "false",
    !!!attribs,
    !!!children
  )

  tag <- htmltools::bindFillRole(tag, container = TRUE, item = fill)
  tag <- htmltools::tagAppendAttributes(tag, class = class)
  return(tag)
}


#' @export
window_toolbar <- function(
  ...,
  container = htmltools::div,
  grabbable = TRUE,
  class = NULL,
  style = NULL,
  id = NULL
) {
  if (is.null(id)) {
    id = sprintf("windowstack-window-header-%s", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }

  tag <- as.window_item(container(
    id = id,
    class = "card-header windowstack-window-header",
    class = if (grabbable) "windowstack-window-handle",
    style = style,
    ...
  ))
  tag <- htmltools::tagAppendAttributes(tag, class = class)
  return(tag)
}


#' @export
window_body <- function(
  ...,
  fillable = TRUE,
  fill = TRUE,
  class = NULL,
  style = NULL,
  id = NULL,
  wrapper = NULL
) {
  if (is.null(id)) {
    id = sprintf("windowstack-window-body-%s", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }
  is_shiny_input <- !is.null(id)

  args <- rlang::list2(...)
  argnames <- rlang::names2(args)

  attribs <- args[nzchar(argnames)]
  children <- args[!nzchar(argnames)]

  children <- as_window_items(children, wrapper = wrapper)

  tag <- htmltools::tags$div(
    id = id,
    class = "card-body windowstack-window-body",
    class = if (is_shiny_input) "windowstack-window-body-input",
    style = style,
    !!!attribs,
    !!!children
  )

  tag <- htmltools::bindFillRole(tag, container = TRUE, item = fill)
  tag <- htmltools::tagAppendAttributes(tag, class = class)
  tag <- as.window_item(tag)
  return(tag)
}

#' @export
window_footer <- function(
  ...,
  container = htmltools::div,
  class = NULL,
  style = NULL,
  id = NULL
) {
  if (is.null(id)) {
    id = sprintf("windowstack-window-header-%s", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }

  tag <- as.window_item(container(
    id = id,
    class = "card-footer windowstack-window-footer",
    style = style,
    ...
  ))
  tag <- htmltools::tagAppendAttributes(tag, class = class)
  return(tag)
}

#' Copied from bslib card
as_window_items <- function(children, wrapper = NULL, ...) {

  if (is.null(wrapper)) {
    wrapper <- function(inner_children, ...) {
      # Convert strings to <p>
      inner_children <- lapply(inner_children, FUN = function(c) {
        if (is(c, "shiny.tag")) {
          return(c)
        } else if (is(c, "character")) {
          return(htmltools::p(c))
        } else {
          return(htmltools::div(c))
        }
      })

      outer <- htmltools::div(
        class = "window-item",
        !!!inner_children,
      )
      outer <- as.window_item(outer)
    }
  }

  # We don't want NULLs creating empty card bodys
  children <- children[vapply(children, function(x) length(x) > 0, logical(1))]

  if (!is.function(wrapper)) {
    return(children)
  }

  # Any children that are `is.card_item` should be included verbatim. Any
  # children that are not, should be wrapped in card_body(). Consecutive children
  # that are not card_item, should be wrapped in a single card_body().
  needs_wrap <- !vapply(children, is.window_item, logical(1))
  needs_wrap_rle <- rle(needs_wrap)
  start_indices <- c(1, head(cumsum(needs_wrap_rle$lengths) + 1, -1))
  children <- mapply(
    start_indices, needs_wrap_rle$lengths, needs_wrap_rle$values,
    FUN = function(start, length, wrap) {
      these_children <- children[start:(start + length - 1)]
      if (wrap) {
        list(wrapper(these_children))
      } else {
        these_children
      }
    },
    SIMPLIFY = FALSE
  )
  unlist(children, recursive = FALSE)
}

#' @describeIn window_body Mark an object as a window item. This will prevent the
#'   [window()] from putting the object inside a `wrapper` (i.e., a
#'   `window_body()`).
#' @param x an object to test (or coerce to) a card item.
#' @export
as.window_item <- function(x) {
  class(x) <- c("window_item", class(x))
  x
}

#' @rdname window_body
#' @export
is.window_item <- function(x) {
  inherits(x, "window_item")
}

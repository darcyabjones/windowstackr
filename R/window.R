#' window
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
    class = "card windowstack-window grid-stack-item-content",
    class = if (is_shiny_input) "windowstack-window-input",
    style = style,
    "data-full-screen" = if (full_screen) "false",
    !!!attribs,
    !!!children
  )

  tag <- htmltools::bindFillRole(tag, container = TRUE, item = fill)
  tag <- htmltools::tagAppendAttributes(tag, class = class)
  class(tag) <- c("windowstackr_window", class(tag))
  return(tag)
}

window_settings <- function(id, settings = NULL, placement = "auto") {
  if (is.null(settings)) {
    return(NULL)
  }

  button <- shiny::actionLink(
    inputId = id,
    class = "card-header-button",
    type="button",
    `aria-label` = "Settings",
    shiny::icon("sliders", class = "fa-solid", lib = "font-awesome")
  )

  po <- bslib::popover(
    trigger = button,
    id = paste0(id, "-popover"),
    placement = placement,
    !!!settings
  )
  return(po)
}

window_close_button <- function(id, `aria-controls` = NULL) {
  htmltools::tags$a(
    id = id,
    class = "card-header-button",
    type = "button",
    onClick = sprintf("windowClose('#%s')", id),
    `aria-label` = "Close",
    `aria-controls` = `aria-controls`,
    shiny::icon("xmark", class = "fa-solid", lib = "font-awesome")
  )
}

window_fullscreen_button <- function(id) {
  #<i class="fa-solid fa-up-right-and-down-left-from-center"></i>
  #<i class="fa-solid fa-down-left-and-up-right-to-center"></i>
  htmltools::tags$a(
    inputId = id,
    class = "card-header-button",
    type="button",
    `aria-label` = "Full screen",
    `aria-expanded` = "false",
    shiny::icon("up-right-and-down-left-from-center", class = "fa-solid", lib = "font-awesome")
  )
}


#' window_toolbar
#'
#' <Add Description>
#'
#' @export
window_toolbar <- function(
  ...,
  container = htmltools::div,
  grabbable = TRUE,
  fullscreen = TRUE,
  closable = TRUE,
  class = NULL,
  style = NULL,
  id = NULL
) {
  if (is.null(id)) {
    id = sprintf("windowstack-window-header-%s", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
  }

  if (is.logical(fullscreen) && fullscreen) {
    fullscreen_button <- window_fullscreen_button(paste0(id, "-fullscreenbutton"))
  } else if (inherits(fullscreen, "shiny.tag")) {
    fullscreen_button <- fullscreen
  } else if (is.null(fullscreen) || (is.logical(fullscreen) && !fullscreen)) {
    fullscreen_button <- NULL
  } else {
    stop("????")
  }

  if (is.logical(closable) && closable) {
    close_button <- window_close_button(paste0(id, "-closebutton"))
  } else if (is(closable, "shiny.tag")) {
    close_button <- closable
  } else if (is.null(closable) || (is.logical(closable) && !closable)) {
    close_button <- NULL
  } else {
    stop("????")
  }

  tag <- as.window_item(container(
    id = id,
    class = "card-header windowstack-window-header",
    class = if (grabbable) "window-stack-handle",
    style = style,
    ...,
    close_button,
    fullscreen_button
  ))

  tag <- htmltools::tagAppendAttributes(tag, class = class)
  htmltools::attachDependencies(tag, fontawesome::fa_html_dependency(), append = TRUE)
  return(tag)
}


#' window_body
#'
#' <Add Description>
#'
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

  tag <- htmltools::bindFillRole(tag, container = TRUE, item = TRUE)
  tag <- htmltools::tagAppendAttributes(tag, class = class)
  tag <- as.window_item(tag)
  return(tag)
}


#' window_footer
#'
#' <Add Description>
#'
#' @export
window_footer <- function(
  ...,
  container = htmltools::div,
  class = NULL,
  style = NULL,
  id = NULL
) {
  if (is.null(id)) {
    id = sprintf("windowstack-window-footer-%s", uuid::UUIDgenerate(use.time = TRUE, output = "string"))
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

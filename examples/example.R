devtools::load_all()

ui <- bslib::page_fluid(
  shiny::tags$head(
    shiny::tags$style("
      .window-stack-handle:hover {
        cursor: move; /* fallback if grab cursor is unsupported */
        cursor: grab;
        cursor: -moz-grab;
        cursor: -webkit-grab;
      }
      .window-stack-handle:active {
        cursor: grabbing;
        cursor: -moz-grabbing;
        cursor: -webkit-grabbing;
      }
      .card {
        border-color: #e5e7eb;
        text-align: left;
      }
      .card-header {
        min-height: 35px;
        padding-top, padding-right, padding-bottom: 5px;
        padding-left: 20px;
        background-color: #e5e7eb;
      }
      .card-header-button {
        float: right;
        height: 25px;
        width: 25px;
        margin-top: 0px;
        margin-left: 5px;
        margin-right: 0px;
        padding-left: 0px;
        padding-right: 0px;
        border-radius: 25px;
        text-align: center;
        color: black;
      }
      .card-header-button:hover {
        background-color: rgba(0,0,0,0.1);
      }
      .card-header-button:active {
        background-color: rgba(0,0,0,0.2);
      }
      .card-header-button i { /* Applies this style to any <i> tags within the container */
        position: relative;
        font-size: 13px;
        top: calc(50% - 13px);
        z-index: 289;
      }
      .card-header-button .fa-up-right-and-down-left-from-center {
        position: relative;
        font-size: 12px;
        top: calc(50% - 14px);
      }
      .card-header-button .fa-xmark {
        position: relative;
        font-size: 16px;
        top: calc(50% - 12px);
      }
      .card-fullscreen-enter {
        float: right;
        height: 25px;
        width: 25px;
        border-radius: 50px;
        text-align: center;
      }
      .ui-resizable-handle {
          opacity: 0 !important;
      }
      .ui-resizable-s, .ui-resizable-se, .ui-resizable-sw {
          bottom: -2.5px !important;
      }
      .ui-resizable-n, .ui-resizable-ne, .ui-resizable-nw {
          top: -2.5px !important;
      }
      .ui-resizable-ne, .ui-resizable-e, .ui-resizable-se {
          right: -2.5px !important;
      }
      .ui-resizable-nw, .ui-resizable-w, .ui-resizable-sw {
          left: -2.5px !important;
      }
    ")
  ),
  htmltools::tags$h2("GridStack example"),
  shiny::actionButton("create_window", "Create window"),
  windowstackOutput("wind")
)

server <- function(input, output, session) {
  bslib::bs_theme()
  output$wind <- renderWindowstack(windowstack(
    id = "wind"
  ))

  wnd <- window(
    window_toolbar(),
    window_body("Initial body")
  )

  proxy <- windowstack_proxy("wind", session) |>
    add_window(wnd)

  shiny::observeEvent(input[["create_window"]], {
    wnd <- window(
      window_toolbar(),
      window_body("test")
    )
    proxy <- windowstack_proxy("wind", session) |>
      add_window(wnd)
  })
}


if (interactive()) {
  shiny::shinyApp(ui, server)
}

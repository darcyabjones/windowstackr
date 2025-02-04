devtools::load_all()

ui <- bslib::page_fluid(
  htmltools::tags$h2("GridStack example"),
  shiny::actionButton("create_window", "Create window"),
  windowstackOutput("wind")
)

server <- function(input, output, session) {
  bslib::bs_theme()
  output$wind <- renderWindowstack(windowstack(
    id = "grid"
  ))

  # It works best if you add things after doing initial render.
  proxy <- windowstack_proxy("wind", session) |>
    add_window(window(
      window_toolbar(),
      window_body("Initial body")
    ))

  shiny::observeEvent(input[["create_window"]], {
    wnd <- window(
      window_toolbar(settings = list("You can put a bunch of control inputs here.")),
      window_body("Put all of your plots in here.")
    )

    proxy <- windowstack_proxy("wind", session) |>
      add_window(wnd)
  })
}


if (interactive()) {
  shiny::shinyApp(ui, server)
}

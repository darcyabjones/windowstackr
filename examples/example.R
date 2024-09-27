devtools::load_all()

windowstack(
  window(window_toolbar("y2o", style = htmltools::css("background" = "red")), window_body("test", style = htmltools::css(width = "100%", padding.top = "20px", text.align="center", vertical.align = "middle")), class = "grid-stack-item"),
)


output_model <- function(model) {
  
  model |>
    select(transition, term, estimate, p.value, conf.low, conf.high) |>
    mutate(across(where(is.numeric), \(x) round(x, digits = 3))) |>
    kableExtra::kbl(format = "html", align = "c") |>
    kableExtra::kable_paper(html_font = "Arial", font_size = 20) |>
    kableExtra::row_spec(row = 0, color = "#cdd6f4", bold = TRUE) |>
    kableExtra::column_spec(column = 1:6, color = "#cdd6f4")
}

app_environment <- new.env(parent = globalenv())
source("app.R", local = app_environment)

stopifnot(
  is.function(app_environment$server),
  inherits(app_environment$ui, "shiny.tag.list")
)
max_factors <- app_environment$MAX_TREATMENT_FACTORS

shiny::testServer(app_environment$server, {
  session$setInputs(
    start_btn = 1,
    design_title = "Completely Randomized Design",
    num_trt = 1,
    level_numbers = "4",
    num_rep = 8,
    interaction_option = "No",
    Type = "F-test",
    Type_ss = "Type III",
    p_value1 = 0.05
  )
  session$flushReact()

  for (invalid_count in c(1.5, 0, max_factors + 1)) {
    session$setInputs(num_trt = invalid_count)
    session$flushReact()
    stopifnot(grepl(
      "integer between 1 and 8",
      output$level_number_ui$html,
      fixed = TRUE
    ))
  }

  session$setInputs(
    num_trt = max_factors,
    level_numbers = paste(rep(2, max_factors), collapse = ",")
  )
  session$flushReact()
  stopifnot(!grepl(
    "integer between 1 and 8",
    output$level_number_ui$html,
    fixed = TRUE
  ))
  stopifnot(grepl('id="factor_8"', output$level_number_ui$html, fixed = TRUE))

  session$setInputs(
    design_title = "Split Plot Design",
    num_trt_main = 4,
    num_trt_sub = 5,
    interaction_option = "Yes"
  )
  session$flushReact()
  stopifnot(grepl(
    "combined cannot exceed 8",
    output$interaction_exist_ui$html,
    fixed = TRUE
  ))
})

message("App construction and server smoke test passed.")

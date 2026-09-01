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
    factor_1 = 4,
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
      output$treatment_names_ui$html,
      fixed = TRUE
    ))
  }

  session$setInputs(
    num_trt = max_factors,
    factor_1 = 2
  )
  session$flushReact()
  stopifnot(!grepl(
    "integer between 1 and 8",
    output$treatment_names_ui$html,
    fixed = TRUE
  ))
  stopifnot(grepl('id="factor_8"', output$treatment_names_ui$html, fixed = TRUE))

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

  session$setInputs(
    design_title = "Completely Randomized Design",
    num_trt = 2,
    factor_1 = 2,
    factor_2 = 2,
    level_numbers = "2,2",
    treatment_factor_name_1 = "Diet",
    treatment_factor_levels_1 = "Control, Supplement",
    treatment_factor_name_2 = "Time",
    treatment_factor_levels_2 = "Week 1, Week 2",
    interaction_option = "Yes",
    interaction_formula = "facA : facB",
    Type = "t-test"
  )
  session$flushReact()

  stopifnot(
    grepl('value="facA : facB" selected>Diet × Time</option>', output$interaction_fac_ui$html, fixed = TRUE),
    grepl("~ Diet + Time + Diet : Time", output$model_ui$html, fixed = TRUE),
    !grepl("facA", output$model_ui$html, fixed = TRUE),
    grepl('value="facA" selected>Diet</option>', output$test_options_ui$html, fixed = TRUE),
    grepl('value="facA:facB">Diet × Time</option>', output$test_options_ui$html, fixed = TRUE),
    grepl('value="NULL" selected>No conditioning variable</option>', output$test_options_ui$html, fixed = TRUE)
  )

  session$setInputs(
    design_title = "Split Plot Design",
    num_trt_main = 1,
    num_trt_sub = 1,
    factor_main_1 = 2,
    factor_sub_1 = 2,
    level_numbers_main = "2",
    level_numbers_sub = "2",
    main_factor_name_1 = "Irrigation",
    main_factor_levels_1 = "Low, High",
    sub_factor_name_1 = "Variety",
    sub_factor_levels_1 = "Standard, Improved",
    interaction_option = "Yes",
    interaction_formula = "trt.main : trt.sub",
    Type = "t-test"
  )
  session$flushReact()

  stopifnot(
    grepl('value="trt.main : trt.sub" selected>Irrigation × Variety</option>', output$interaction_fac_ui$html, fixed = TRUE),
    grepl("~ Irrigation + Variety + Irrigation : Variety", output$model_ui$html, fixed = TRUE),
    grepl('value="trt.main" selected>Irrigation</option>', output$test_options_ui$html, fixed = TRUE),
    grepl('value="trt.main:trt.sub">Irrigation × Variety</option>', output$test_options_ui$html, fixed = TRUE)
  )
})

message("App construction and server smoke test passed.")

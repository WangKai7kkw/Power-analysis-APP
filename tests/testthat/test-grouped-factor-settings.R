app_environment <- new.env(parent = globalenv())
app_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
previous_directory <- setwd(app_root)
tryCatch(
  sys.source("app.R", envir = app_environment),
  finally = setwd(previous_directory)
)

test_that("the app header provides an accessible issue-reporting link", {
  shiny::testServer(app_environment$server, {
    header_html <- paste(as.character(output$main_ui), collapse = "\n")

    expect_match(header_html, "Report an issue", fixed = TRUE)
    expect_match(
      header_html,
      'href="https://github.com/WangKai7kkw/Power-analysis-APP/issues/new"',
      fixed = TRUE
    )
    expect_match(header_html, 'target="_blank"', fixed = TRUE)
    expect_match(header_html, 'rel="noopener noreferrer"', fixed = TRUE)
    expect_match(
      header_html,
      'aria-label="Report an issue on GitHub (opens in a new tab)"',
      fixed = TRUE
    )
  })
})

test_that("grouped factor server state keeps display names separate from identifiers", {
  shiny::testServer(app_environment$server, {
    session$setInputs(
      start_btn = 1,
      design_title = "Completely Randomized Design",
      num_trt = 2
    )
    session$flushReact()

    configuration_html <- paste(as.character(output$treatment_names_ui), collapse = "\n")
    expect_match(configuration_html, "Factor configuration", fixed = TRUE)
    expect_match(configuration_html, "Factor A", fixed = TRUE)
    expect_match(configuration_html, "value=\"facA\"", fixed = TRUE)
    expect_match(configuration_html, "value=\"facA1, facA2\"", fixed = TRUE)
    expect_match(configuration_html, "Number of levels", fixed = TRUE)

    session$setInputs(
      factor_1 = 3,
      factor_2 = 2,
      treatment_factor_name_1 = "Diet",
      treatment_factor_levels_1 = "Control, Supplement, facA3",
      treatment_factor_name_2 = "Time",
      treatment_factor_levels_2 = "Week 1, Week 2"
    )
    session$flushReact()

    spec <- active_treatment_label_spec()
    expect_equal(vapply(spec, `[[`, character(1), "internal"), c("facA", "facB"))
    expect_equal(vapply(spec, `[[`, character(1), "name"), c("Diet", "Time"))
    expect_equal(spec[[1]]$levels, c("Control", "Supplement", "facA3"))
    expect_null(validate_treatment_label_spec(spec))
    expect_equal(
      app_environment$friendly_term_choices(c("facA", "facB", "facA:facB"), display_treatment_label_spec()),
      c(Diet = "facA", Time = "facB", `Diet × Time` = "facA:facB")
    )

    session$setInputs(treatment_factor_levels_1 = "Control, Supplement")
    session$flushReact()
    feedback_html <- paste(
      as.character(output$treatment_factor_feedback_1),
      collapse = "\n"
    )
    expect_match(
      feedback_html,
      "2 of 3 level names entered",
      fixed = TRUE
    )
    expect_match(
      feedback_html,
      "Add 1 more level name",
      fixed = TRUE
    )
  })
})

test_that("split-plot grouped settings use separate headings and stable internals", {
  shiny::testServer(app_environment$server, {
    session$setInputs(
      start_btn = 1,
      design_title = "Split Plot Design",
      num_trt_main = 1,
      num_trt_sub = 1,
      factor_main_1 = 2,
      factor_sub_1 = 2
    )
    session$flushReact()

    configuration_html <- paste(as.character(output$treatment_names_ui), collapse = "\n")
    expect_match(configuration_html, "Whole-plot factors", fixed = TRUE)
    expect_match(configuration_html, "Subplot factors", fixed = TRUE)

    session$setInputs(
      main_factor_name_1 = "Irrigation",
      main_factor_levels_1 = "Dry, Wet",
      sub_factor_name_1 = "Cultivar",
      sub_factor_levels_1 = "Standard, Improved"
    )
    session$flushReact()

    spec <- active_treatment_label_spec()
    expect_equal(vapply(spec, `[[`, character(1), "internal"), c("trt.main", "trt.sub"))
    expect_equal(vapply(spec, `[[`, character(1), "name"), c("Irrigation", "Cultivar"))
    expect_null(validate_treatment_label_spec(spec))
    expect_equal(
      translate_model_term("trt.main:trt.sub", spec),
      "Irrigation × Cultivar"
    )
  })
})

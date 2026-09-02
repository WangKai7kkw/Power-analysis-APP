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

test_that("design selection and replication controls are grouped in the left column", {
  shiny::testServer(app_environment$server, {
    main_html <- paste(as.character(output$main_ui), collapse = "\n")
    selection_position <- regexpr("design-selection-card", main_html, fixed = TRUE)[[1]]
    treatment_position <- regexpr("dynamic_sidebar", main_html, fixed = TRUE)[[1]]

    expect_gt(selection_position, 0)
    expect_gt(treatment_position, selection_position)
    expect_match(main_html, 'id="design_title"', fixed = TRUE)
    expect_match(main_html, "Experiment layout", fixed = TRUE)
    expect_false(grepl("Select a design, then set its replication", main_html, fixed = TRUE))
    expect_false(grepl("design-selector-card", main_html, fixed = TRUE))
  })

  selector_html <- paste(
    as.character(app_environment$experimental_design_input()),
    collapse = "\n"
  )
  expect_match(selector_html, "Choose the design", fixed = TRUE)
  expect_match(selector_html, "Completely randomized design (CRD)", fixed = TRUE)
  expect_match(selector_html, "Randomized complete block design (RCBD)", fixed = TRUE)
  expect_match(selector_html, "Latin square design", fixed = TRUE)
  expect_match(selector_html, "Split-plot design", fixed = TRUE)
  expect_match(selector_html, ">Custom design<", fixed = TRUE)
  expect_false(grepl("Custom design from uploaded data", selector_html, fixed = TRUE))

  expected_controls <- list(
    "Completely Randomized Design" = c("num_rep"),
    "Randomized Complete Block Design" = c("num_block"),
    "Latin Square Design" = c("num_squares", "value_reuse"),
    "Split Plot Design" = c("num_rep"),
    "General Design" = character()
  )
  all_replication_ids <- c("num_rep", "num_block", "num_squares", "value_reuse")

  for (design_title in names(expected_controls)) {
    settings_html <- paste(
      as.character(app_environment$replication_settings_ui(design_title)),
      collapse = "\n"
    )
    expected_ids <- expected_controls[[design_title]]

    for (input_id in expected_ids) {
      expect_match(settings_html, sprintf('id="%s"', input_id), fixed = TRUE)
    }
    for (input_id in setdiff(all_replication_ids, expected_ids)) {
      expect_false(grepl(sprintf('id="%s"', input_id), settings_html, fixed = TRUE))
    }
  }

  crd_html <- paste(
    as.character(app_environment$replication_settings_ui("Completely Randomized Design")),
    collapse = "\n"
  )
  expect_match(crd_html, "Replicates per treatment", fixed = TRUE)
  expect_match(crd_html, "each combination of factor levels is treated as one treatment", fixed = TRUE)
  expect_false(grepl(">Replication<", crd_html, fixed = TRUE))

  rcbd_html <- paste(
    as.character(app_environment$replication_settings_ui("Randomized Complete Block Design")),
    collapse = "\n"
  )
  expect_match(rcbd_html, "Number of blocks", fixed = TRUE)
  expect_match(rcbd_html, "The block size is fixed by the number of treatments", fixed = TRUE)
  expect_match(rcbd_html, "incomplete blocks or multiple replicates", fixed = TRUE)

  latin_html <- paste(
    as.character(app_environment$replication_settings_ui("Latin Square Design")),
    collapse = "\n"
  )
  expect_match(latin_html, "Number of squares", fixed = TRUE)
  expect_match(latin_html, "square size—the number of row and column blocks—is determined", fixed = TRUE)
  expect_match(latin_html, "row and column blocks can represent subjects and time periods", fixed = TRUE)

  split_plot_html <- paste(
    as.character(app_environment$replication_settings_ui("Split Plot Design")),
    collapse = "\n"
  )
  expect_match(split_plot_html, "Whole plots per main treatment", fixed = TRUE)
  expect_false(grepl("Whole-plot replication", split_plot_html, fixed = TRUE))

  general_html <- paste(
    as.character(app_environment$replication_settings_ui("General Design")),
    collapse = "\n"
  )
  expect_match(general_html, "Upload study layout", fixed = TRUE)
  expect_match(general_html, "Upload a long-format design table", fixed = TRUE)
  expect_match(general_html, 'id="uploaded_file"', fixed = TRUE)
  expect_false(grepl("The app reads the column names", general_html, fixed = TRUE))
  expect_false(grepl("Replication is defined by the observation rows", general_html, fixed = TRUE))
  expect_false(grepl("no separate replication field is needed", general_html, fixed = TRUE))
  expect_match(app_environment$app_css, "@media (max-width: 860px)", fixed = TRUE)
  expect_match(
    app_environment$app_css,
    "grid-template-areas: 'design' 'test' 'results'",
    fixed = TRUE
  )
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

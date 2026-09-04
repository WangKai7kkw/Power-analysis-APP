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
      'href="https://github.com/WangKai7kkw/pwr4exp-app/issues/new"',
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

test_that("the app presents a guided workflow with editable result summaries", {
  shiny::testServer(app_environment$server, {
    main_html <- paste(as.character(output$main_ui), collapse = "\n")
    selection_position <- regexpr("design-selection-card", main_html, fixed = TRUE)[[1]]
    treatment_position <- regexpr("treatment_structure_ui", main_html, fixed = TRUE)[[1]]
    assumptions_position <- regexpr("dynamic_sidebar", main_html, fixed = TRUE)[[1]]

    expect_gt(selection_position, 0)
    expect_gt(treatment_position, selection_position)
    expect_gt(assumptions_position, treatment_position)
    expect_match(main_html, 'id="workflow_stage"', fixed = TRUE)
    expect_match(main_html, 'class="workflow-stage design-stage"', fixed = TRUE)
    expect_match(main_html, 'class="workflow-stage assumptions-stage"', fixed = TRUE)
    expect_match(main_html, 'class="workflow-stage test-stage"', fixed = TRUE)
    expect_match(main_html, 'class="results-workspace"', fixed = TRUE)
    expect_match(main_html, 'id="continue_to_assumptions"', fixed = TRUE)
    expect_match(main_html, 'id="continue_to_tests"', fixed = TRUE)
    expect_match(main_html, 'id="run_analysis"', fixed = TRUE)
    expect_match(main_html, 'class="analysis-setup-details"', fixed = TRUE)
    expect_match(main_html, 'id="design_title"', fixed = TRUE)
    expect_match(main_html, "Design setup", fixed = TRUE)
    expect_match(main_html, "Model assumptions", fixed = TRUE)
    expect_match(main_html, "Test settings", fixed = TRUE)
    expect_match(main_html, "Results &amp; export", fixed = TRUE)
    expect_match(main_html, "Experiment layout", fixed = TRUE)
    expect_match(main_html, "Choose the experimental design, define replication", fixed = TRUE)
    expect_false(grepl("Select a design, then set its replication", main_html, fixed = TRUE))
    expect_false(grepl("design-selector-card", main_html, fixed = TRUE))

    session$setInputs(
      start_btn = 1,
      design_title = "Completely Randomized Design",
      num_trt = 1
    )
    session$flushReact()

    structure_html <- paste(as.character(output$treatment_structure_ui), collapse = "\n")
    assumptions_html <- paste(as.character(output$dynamic_sidebar), collapse = "\n")
    navigation_html <- paste(as.character(output$workflow_navigation), collapse = "\n")
    expect_match(structure_html, "Treatment structure", fixed = TRUE)
    expect_match(structure_html, 'id="num_trt"', fixed = TRUE)
    expect_false(grepl("step-badge", structure_html, fixed = TRUE))
    expect_match(assumptions_html, "Analysis model", fixed = TRUE)
    expect_false(grepl("Treatment structure", assumptions_html, fixed = TRUE))
    expect_match(navigation_html, "CRD · 1 factor", fixed = TRUE)
    expect_match(navigation_html, "Available after running", fixed = TRUE)
    expect_match(navigation_html, 'aria-disabled="true"', fixed = TRUE)

    results_seen(TRUE)
    results_generated(TRUE)
    session$flushReact()
    completed_navigation <- paste(as.character(output$workflow_navigation), collapse = "\n")
    expect_match(completed_navigation, "Power results ready", fixed = TRUE)
    expect_false(grepl('aria-disabled="true"', completed_navigation, fixed = TRUE))
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
  expect_match(general_html, "Study layout", fixed = TRUE)
  expect_match(general_html, "Provide a long-format design table", fixed = TRUE)
  expect_match(general_html, 'id="custom_data_source"', fixed = TRUE)
  expect_match(general_html, ">Upload file<", fixed = TRUE)
  expect_match(general_html, ">Create table<", fixed = TRUE)
  expect_match(general_html, 'id="uploaded_file"', fixed = TRUE)
  expect_match(general_html, 'id="manual_column_names"', fixed = TRUE)
  expect_match(general_html, 'id="manual_row_count"', fixed = TRUE)
  expect_match(general_html, 'id="create_manual_table"', fixed = TRUE)
  expect_match(general_html, 'id="custom_layout_table"', fixed = TRUE)
  expect_match(
    app_environment$app_css,
    "#manual_table_feedback > .field-note { margin-top: 10px; }",
    fixed = TRUE
  )
  expect_false(grepl("The app reads the column names", general_html, fixed = TRUE))
  expect_false(grepl("Replication is defined by the observation rows", general_html, fixed = TRUE))
  expect_false(grepl("no separate replication field is needed", general_html, fixed = TRUE))
  expect_match(app_environment$app_css, "@media (max-width: 860px)", fixed = TRUE)
  expect_match(
    app_environment$app_css,
    ".results-workspace { grid-template-columns: 1fr; }",
    fixed = TRUE
  )
})

test_that("standard design changes retain compatible analysis state", {
  expect_identical(
    unname(vapply(
      c(
        "Completely Randomized Design",
        "Randomized Complete Block Design",
        "Latin Square Design"
      ),
      app_environment$design_family_for,
      character(1)
    )),
    rep("standard", 3)
  )
  expect_identical(app_environment$design_family_for("Split Plot Design"), "split_plot")
  expect_identical(app_environment$design_family_for("General Design"), "custom")
  expect_null(app_environment$design_family_for(NULL))

  shiny::testServer(app_environment$server, {
    session$setInputs(
      start_btn = 1,
      design_title = "Completely Randomized Design",
      num_rep = 8,
      num_trt = 2,
      factor_1 = 2,
      factor_2 = 2,
      level_numbers = "2,2",
      treatment_factor_name_1 = "Starch",
      treatment_factor_levels_1 = "Low, High",
      treatment_factor_name_2 = "NDF",
      treatment_factor_levels_2 = "Low, High",
      interaction_option = "Yes",
      interaction_formula = "facA : facB",
      Type = "F-test & t-test",
      Type_ss = "Type II",
      p_value1 = 0.025,
      which_para = "facA",
      by_para = "facB",
      Contrast = "pairwise",
      alternative = "one.sided",
      p.adj = TRUE
    )
    session$flushReact()

    standard_means_cache(matrix(c(1, 2, 3, 4), ncol = 1))
    session$setInputs(
      design_title = "Randomized Complete Block Design",
      num_block = 8
    )
    session$flushReact()

    expect_identical(design_family(), "standard")
    expect_identical(input$num_trt, 2)
    expect_identical(input$treatment_factor_name_1, "Starch")
    expect_identical(input$treatment_factor_name_2, "NDF")
    expect_identical(input$interaction_option, "Yes")
    expect_identical(input$interaction_formula, "facA : facB")
    expect_identical(input$Type, "F-test & t-test")
    expect_identical(input$Type_ss, "Type II")
    expect_equal(input$p_value1, 0.025)
    expect_identical(input$which_para, "facA")
    expect_identical(input$by_para, "facB")
    expect_identical(input$alternative, "one.sided")
    expect_true(input$p.adj)
    expect_equal(as.numeric(values$data[, 1]), c(1, 2, 3, 4))
    expect_identical(rownames(values$variance), c("Block", "Error"))

    model_html <- paste(as.character(output$model_ui), collapse = "\n")
    expect_match(model_html, "Starch + NDF + Starch : NDF", fixed = TRUE)
    expect_match(model_html, "block", fixed = TRUE)

    session$setInputs(
      design_title = "Latin Square Design",
      num_squares = 4,
      value_reuse = "none"
    )
    session$flushReact()

    expect_equal(as.numeric(values$data[, 1]), c(1, 2, 3, 4))
    expect_identical(rownames(values$variance), c("Row", "Col", "Error"))
    expect_identical(input$interaction_formula, "facA : facB")
    expect_identical(input$Type_ss, "Type II")
    expect_identical(input$which_para, "facA")
    expect_identical(input$by_para, "facB")
  })
})

test_that("custom designs can initialize an editable table", {
  shiny::testServer(app_environment$server, {
    session$setInputs(
      start_btn = 1,
      design_title = "General Design",
      custom_data_source = "manual",
      manual_column_names = "treatment, block, period",
      manual_row_count = 6,
      create_manual_table = 1
    )
    session$flushReact()

    expect_equal(names(datavalues$manual_data), c("treatment", "block", "period"))
    expect_equal(dim(datavalues$manual_data), c(6L, 3L))
    expect_match(datavalues$manual_data_error, "Fill every cell", fixed = TRUE)
    expect_null(datavalues$custom_data)

    table_html <- paste(as.character(output$custom_layout_table), collapse = "\n")
    expect_match(table_html, '"treatment":""', fixed = TRUE)
    expect_match(table_html, '"row_above"', fixed = TRUE)

    widget_params <- rhandsontable::rhandsontable(
      datavalues$manual_data,
      rowHeaders = TRUE
    )$x
    completed_rows <- lapply(seq_len(6), function(index) {
      list(if (index <= 3) "control" else "treated", as.character(index), as.character(index))
    })
    session$setInputs(custom_layout_table = list(
      data = completed_rows,
      changes = list(event = "afterChange"),
      params = widget_params
    ))
    session$flushReact()

    expect_null(datavalues$manual_data_error)
    expect_equal(dim(datavalues$custom_data), c(6L, 3L))
    expect_equal(datavalues$custom_data$treatment, rep(c("control", "treated"), each = 3))

    session$setInputs(custom_data_source = "upload")
    session$flushReact()
    expect_null(datavalues$custom_data)

    session$setInputs(custom_data_source = "manual")
    session$flushReact()
    expect_equal(dim(datavalues$custom_data), c(6L, 3L))
  })
})

test_that("custom designs still accept uploaded data", {
  upload_path <- tempfile(fileext = ".csv")
  writeLines(c("treatment,block", "control,1", "treated,2"), upload_path)

  shiny::testServer(app_environment$server, {
    session$setInputs(
      start_btn = 1,
      design_title = "General Design",
      custom_data_source = "upload",
      uploaded_file = data.frame(
        name = "layout.csv",
        size = file.info(upload_path)$size,
        type = "text/csv",
        datapath = upload_path,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()

    expect_equal(names(datavalues$custom_data), c("treatment", "block"))
    expect_equal(nrow(datavalues$custom_data), 2L)

    column_types_html <- paste(as.character(output$file_type_check), collapse = "\n")
    expect_match(column_types_html, "Column types", fixed = TRUE)
    expect_match(column_types_html, "column-role-list", fixed = TRUE)
    expect_match(column_types_html, "column-role-row", fixed = TRUE)
    expect_match(column_types_html, 'for="factor_type_1"', fixed = TRUE)
    expect_match(column_types_html, ">Factor<", fixed = TRUE)
    expect_match(column_types_html, ">Numeric<", fixed = TRUE)
    expect_false(grepl(">Categorical<", column_types_html, fixed = TRUE))
    expect_false(grepl("Categorical factor", column_types_html, fixed = TRUE))
    expect_false(grepl("Continuous covariate", column_types_html, fixed = TRUE))
    expect_false(grepl("Choose Factor for groups or levels", column_types_html, fixed = TRUE))
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

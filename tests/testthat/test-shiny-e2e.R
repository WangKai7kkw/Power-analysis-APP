test_that("Power Calculation renders the CRD result tables", {
  previous_cran_override <- Sys.getenv(
    "SHINYTEST2_APP_DRIVER_TEST_ON_CRAN",
    unset = NA_character_
  )
  Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = "1")
  on.exit({
    if (is.na(previous_cran_override)) {
      Sys.unsetenv("SHINYTEST2_APP_DRIVER_TEST_ON_CRAN")
    } else {
      Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = previous_cran_override)
    }
  }, add = TRUE)

  app_dir <- normalizePath(file.path("..", ".."), mustWork = TRUE)
  app <- withCallingHandlers(
    shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "crd-rendered-tables",
      width = 1400,
      height = 900,
      load_timeout = 30 * 1000,
      timeout = 15 * 1000
    ),
    skip = function(condition) {
      stop(
        "The end-to-end Shiny test requires a runnable headless Chrome: ",
        conditionMessage(condition),
        call. = FALSE
      )
    }
  )
  on.exit(app$stop(), add = TRUE)

  app$click("start_btn")
  app$wait_for_js(
    "document.querySelector('#factor_1') &&
      window.HTMLWidgets &&
      window.HTMLWidgets.find('#design_table')",
    timeout = 15 * 1000
  )
  app$set_inputs(
    factor_1 = 4,
    num_rep = 8,
    wait_ = FALSE,
    priority_ = "event"
  )
  app$set_inputs(level_numbers = "4", priority_ = "event")
  app$wait_for_js(
    "window.HTMLWidgets &&
      window.HTMLWidgets.find('#design_table') &&
      window.HTMLWidgets.find('#design_variance_table') &&
      document.querySelector('#treatment_factor_name_1')",
    timeout = 15 * 1000
  )
  app$run_js(
    "window.HTMLWidgets.find('#design_table').hot.setDataAtCell([
        [0, 0, 35],
        [1, 0, 30],
        [2, 0, 37],
        [3, 0, 38]
      ]);
      window.HTMLWidgets.find('#design_variance_table').hot.setDataAtCell(0, 0, 15);"
  )
  app$set_inputs(
    treatment_factor_name_1 = "Diet",
    treatment_factor_levels_1 = "Control, Low dose, Medium dose, High dose",
    wait_ = FALSE,
    priority_ = "event"
  )
  app$wait_for_js(
    "window.HTMLWidgets.find('#design_table').hot.getRowHeader()[0] === 'Control' &&
      window.HTMLWidgets.find('#design_table').hot.getRowHeader()[3] === 'High dose'",
    timeout = 15 * 1000
  )
  expect_identical(
    app$get_js("window.HTMLWidgets.find('#design_table').hot.countRows()"),
    4L
  )
  expect_equal(
    as.numeric(unlist(app$get_js("window.HTMLWidgets.find('#design_table').hot.getDataAtCol(0)"))),
    c(35, 30, 37, 38)
  )
  app$set_inputs(Type = "F-test & t-test", wait_ = FALSE)
  app$wait_for_idle()

  # Exercise the real action button and the complete server/rendering path.
  app$click("create_result")
  app$wait_for_js(
    "(document.querySelectorAll('#power_omnibus_test table tbody tr').length === 1 &&
      document.querySelectorAll('#power_contrast table tbody tr').length === 6) ||
      document.querySelector('.shiny-notification')",
    timeout = 15 * 1000
  )
  results_ready <- app$get_js(
    "document.querySelectorAll('#power_omnibus_test table tbody tr').length === 1 &&
      document.querySelectorAll('#power_contrast table tbody tr').length === 6"
  )
  expect_true(
    isTRUE(results_ready),
    info = paste(app$get_text(".shiny-notification"), collapse = " ")
  )
  if (!isTRUE(results_ready)) return(invisible())

  omnibus_headers <- trimws(app$get_text("#power_omnibus_test th"))
  omnibus_cells <- trimws(app$get_text("#power_omnibus_test tbody td"))
  contrast_headers <- trimws(app$get_text("#power_contrast th"))
  contrast_cells <- matrix(
    trimws(app$get_text("#power_contrast tbody td")),
    ncol = 6,
    byrow = TRUE
  )

  expect_identical(
    omnibus_headers,
    c("F-test", "NumDF", "DenDF", "sig.level", "power")
  )
  expect_identical(omnibus_cells, c("Diet", "3", "28.00", "0.05", "0.95"))
  expect_identical(
    contrast_headers,
    c("Contrast", "effect", "df", "sig.level", "power", "alternative")
  )
  expect_identical(
    contrast_cells[, 1],
    c(
      "Control - Low dose", "Control - Medium dose", "Control - High dose",
      "Low dose - Medium dose", "Low dose - High dose", "Medium dose - High dose"
    )
  )
  expect_identical(contrast_cells[, 5], c("0.70", "0.17", "0.32", "0.94", "0.98", "0.08"))
  expect_false(any(grepl("An error occurred", app$get_text("body"), fixed = TRUE)))
})

test_that("custom factor names appear throughout multifactor model controls", {
  previous_cran_override <- Sys.getenv(
    "SHINYTEST2_APP_DRIVER_TEST_ON_CRAN",
    unset = NA_character_
  )
  Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = "1")
  on.exit({
    if (is.na(previous_cran_override)) {
      Sys.unsetenv("SHINYTEST2_APP_DRIVER_TEST_ON_CRAN")
    } else {
      Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = previous_cran_override)
    }
  }, add = TRUE)

  app_dir <- normalizePath(file.path("..", ".."), mustWork = TRUE)
  app <- withCallingHandlers(
    shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "custom-multifactor-labels",
      width = 1400,
      height = 900,
      load_timeout = 30 * 1000,
      timeout = 15 * 1000
    ),
    skip = function(condition) {
      stop(
        "The end-to-end Shiny test requires a runnable headless Chrome: ",
        conditionMessage(condition),
        call. = FALSE
      )
    }
  )
  on.exit(app$stop(), add = TRUE)

  app$click("start_btn")
  app$set_inputs(num_trt = 2, wait_ = FALSE, priority_ = "event")
  app$wait_for_js(
    "document.querySelector('#factor_2') &&
      document.querySelector('#treatment_factor_name_2')",
    timeout = 15 * 1000
  )
  app$set_inputs(
    treatment_factor_name_1 = "Diet",
    treatment_factor_levels_1 = "Control, Supplement",
    wait_ = FALSE,
    priority_ = "event"
  )
  app$wait_for_js(
    "document.querySelector('label[for=\"factor_1\"]').textContent.trim() === 'Diet'",
    timeout = 15 * 1000
  )
  app$set_inputs(
    treatment_factor_name_2 = "Time",
    treatment_factor_levels_2 = "Week 1, Week 2",
    wait_ = FALSE,
    priority_ = "event"
  )
  app$wait_for_js(
    "document.querySelector('label[for=\"factor_1\"]').textContent.trim() === 'Diet' &&
      document.querySelector('label[for=\"factor_2\"]').textContent.trim() === 'Time' &&
      document.querySelector('#model_ui').innerText.includes('Diet + Time') &&
      !document.querySelector('#model_ui').innerText.includes('facA')",
    timeout = 15 * 1000
  )

  app$set_inputs(interaction_option = "Yes", wait_ = FALSE, priority_ = "event")
  app$wait_for_js(
    "document.querySelector('#interaction_formula')",
    timeout = 15 * 1000
  )
  app$set_inputs(
    interaction_formula = "facA : facB",
    wait_ = FALSE,
    priority_ = "event"
  )
  app$wait_for_js(
    "document.querySelector('#model_ui').innerText.includes('Diet : Time') &&
      document.querySelector('#interaction_formula option:checked').value === 'facA : facB' &&
      document.querySelector('#interaction_formula option:checked').textContent === 'Diet × Time'",
    timeout = 15 * 1000
  )
  expect_true(isTRUE(app$get_js(
    "document.querySelector('#interaction_formula').value === 'facA : facB'"
  )))

  app$set_inputs(Type = "t-test", wait_ = FALSE, priority_ = "event")
  app$wait_for_js(
    "document.querySelector('#which_para') &&
      document.querySelector('#by_para')",
    timeout = 15 * 1000
  )
  app$run_js("document.querySelector('#which_para-selectized').click()")
  app$wait_for_js(
    "document.querySelector('#which_para').parentElement.querySelectorAll('.option[data-value]').length >= 3",
    timeout = 15 * 1000
  )
  which_options <- app$get_js(
    "Array.from(document.querySelector('#which_para').parentElement.querySelectorAll('[data-value]'))
      .map(option => `${option.dataset.value}=${option.textContent.trim()}`).join('|')"
  )
  app$run_js("document.querySelector('#by_para-selectized').click()")
  app$wait_for_js(
    "document.querySelector('#by_para').parentElement.querySelector('.option[data-value=\"facB\"]')",
    timeout = 15 * 1000
  )
  by_options <- app$get_js(
    "Array.from(document.querySelector('#by_para').parentElement.querySelectorAll('[data-value]'))
      .map(option => `${option.dataset.value}=${option.textContent.trim()}`).join('|')"
  )
  expect_match(which_options, "facA=Diet", fixed = TRUE)
  expect_match(which_options, "facB=Time", fixed = TRUE)
  expect_match(which_options, "facA:facB=Diet × Time", fixed = TRUE)
  expect_match(by_options, "facB=Time", fixed = TRUE)
  expect_true(isTRUE(app$get_js(
    "document.querySelector('#which_para').value === 'facA' &&
      document.querySelector('#by_para').value === 'NULL'"
  )))
  expect_false(any(grepl("An error occurred", app$get_text("body"), fixed = TRUE)))
})

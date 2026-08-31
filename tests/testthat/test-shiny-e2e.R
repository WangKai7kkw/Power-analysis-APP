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
      window.HTMLWidgets.find('#design_variance_table')",
    timeout = 15 * 1000
  )
  expect_identical(
    app$get_js("window.HTMLWidgets.find('#design_table').hot.countRows()"),
    4L
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
  app$set_inputs(Type = "F-test & t-test", wait_ = FALSE)
  app$wait_for_idle()

  # Exercise the real action button and the complete server/rendering path.
  app$click("create_result")
  app$wait_for_js(
    "document.querySelectorAll('#power_omnibus_test table tbody tr').length === 1 &&
      document.querySelectorAll('#power_contrast table tbody tr').length === 6",
    timeout = 15 * 1000
  )

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
  expect_identical(omnibus_cells, c("trt", "3", "28.00", "0.05", "0.95"))
  expect_identical(
    contrast_headers,
    c("Contrast", "effect", "df", "sig.level", "power", "alternative")
  )
  expect_identical(
    contrast_cells[, 1],
    c(
      "trt1 - trt2", "trt1 - trt3", "trt1 - trt4",
      "trt2 - trt3", "trt2 - trt4", "trt3 - trt4"
    )
  )
  expect_identical(contrast_cells[, 5], c("0.70", "0.17", "0.32", "0.94", "0.98", "0.08"))
  expect_false(any(grepl("An error occurred", app$get_text("body"), fixed = TRUE)))
})

make_vignette_crd <- function() {
  build_crd_design(
    treatments = 4,
    replicates = 8,
    means = c(35, 30, 37, 38),
    sigma2 = 15
  )
}

test_that("omnibus results are formatted for display", {
  result <- calculate_power_results(
    make_vignette_crd(),
    test_type = "F-test"
  )$omnibus

  table <- format_omnibus_result(result)

  expect_identical(names(table), c("F-test", names(result)))
  expect_identical(table[["F-test"]], "trt")
  expect_equal(table$power, 0.95467, tolerance = 1e-5)
  expect_identical(row.names(table), "1")
})

test_that("unconditioned contrast results are formatted for display", {
  result <- calculate_power_results(
    make_vignette_crd(),
    test_type = "t-test",
    which = "trt",
    contrast = "pairwise"
  )$contrast

  table <- format_contrast_result(result)

  expect_identical(names(table), c(" Contrast ", names(result)))
  expect_identical(table[[" Contrast "]], row.names(result))
  expect_equal(table$power, result$power)
})

test_that("custom contrast labels are formatted for display", {
  result <- calculate_power_results(
    make_vignette_crd(),
    test_type = "t-test",
    which = "trt",
    contrast = c(-1, 1 / 3, 1 / 3, 1 / 3)
  )$contrast

  table <- format_contrast_result(result, custom = TRUE)

  expect_identical(table[[" Contrast "]], "Contrast vector")
  expect_equal(table$power, 0.05, tolerance = 1e-12)
})

test_that("conditioned contrast lists are flattened for display", {
  design <- build_rcbd_design(
    treatments = c(2, 2),
    blocks = 8,
    means = c(35, 40, 38, 41),
    vcomp = 11,
    sigma2 = 4
  )
  result <- calculate_power_results(
    design,
    test_type = "t-test",
    which = "facA",
    by = "facB",
    contrast = "pairwise"
  )$contrast

  table <- format_contrast_result(result)

  expect_identical(
    names(table),
    c(" Variable ", " Contrast ", names(result[[1]]))
  )
  result_rows <- vapply(result, nrow, integer(1))
  expect_identical(table[[" Variable "]], rep(names(result), result_rows))
  expect_equal(table$power, unlist(lapply(result, `[[`, "power"), use.names = FALSE))
})

test_that("combined calculations format both result sections", {
  result <- calculate_power_results(
    make_vignette_crd(),
    test_type = "F-test & t-test",
    which = "trt",
    contrast = "pairwise"
  )

  expect_no_error(omnibus <- format_omnibus_result(result$omnibus))
  expect_no_error(contrasts <- format_contrast_result(result$contrast))
  expect_equal(omnibus$power, 0.95467, tolerance = 1e-5)
  expect_equal(nrow(contrasts), 6)
})

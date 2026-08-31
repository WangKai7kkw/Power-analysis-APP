test_that("upload detection uses the original filename, not the temporary path", {
  temporary_path <- tempfile()
  writeLines(c("group,value", "A,1", "B,2"), temporary_path)
  upload <- list(name = "EXPERIMENT.CSV", datapath = temporary_path)

  expect_equal(uploaded_file_extension(upload), "csv")
  result <- read_uploaded_data(upload)
  expect_equal(names(result), c("group", "value"))
  expect_equal(nrow(result), 2)
})

test_that("unsupported and empty uploads fail clearly", {
  temporary_path <- tempfile()
  writeLines("content", temporary_path)
  expect_error(
    read_uploaded_data(list(name = "experiment.pdf", datapath = temporary_path)),
    "Unsupported file type"
  )

  empty_path <- tempfile()
  writeLines("value", empty_path)
  expect_error(
    read_uploaded_data(list(name = "empty.csv", datapath = empty_path)),
    "empty"
  )
})

test_that("factor generation is bounded", {
  expect_equal(generate_factor_combinations_safe(2), c("facA", "facB", "facA:facB"))
  expect_error(validate_factor_count(0), "positive integer")
  expect_error(validate_factor_count(MAX_TREATMENT_FACTORS + 1), "At most")
})

test_that("custom contrasts reject unsafe values", {
  expect_equal(parse_custom_contrast("-1, 0.5, 0.5", 3), c(-1, 0.5, 0.5))
  expect_error(parse_custom_contrast("1, nope, -1"), "finite numbers")
  expect_error(parse_custom_contrast("1, -1", 3), "3 are required")
  expect_error(parse_custom_contrast("1, 1, -1"), "sum to zero")
})

test_that("model formula validation preserves dotted names and random effects", {
  data <- data.frame(
    treatment.group = factor(c("A", "B")),
    block = factor(c(1, 2)),
    check.names = FALSE
  )

  formula <- validate_model_formula("~ treatment.group + (1 | block)", data)
  expect_s3_class(formula, "formula")
  expect_equal(all.vars(formula), c("treatment.group", "block"))
  expect_error(validate_model_formula("treatment.group", data), "must start")
  expect_error(validate_model_formula("~ missing", data), "not found")
})

test_that("invalid correlation specifications stop", {
  data <- data.frame(subject = factor(c(1, 1, 2, 2)), time = factor(c(1, 2, 1, 2)))

  expect_error(
    build_correlation_spec(data, "corAR1", list(time = "missing", group = "subject", rho = 0.5)),
    "not found"
  )
  expect_error(
    build_correlation_spec(data, "corARMA", list(time = "time", group = "subject", p = 0, q = 0)),
    "at least one"
  )
  expect_error(
    build_correlation_spec(data, "corExp", list(coordinates = "time", range = 0)),
    "positive"
  )
})

test_that("combined calculations return both result families", {
  design <- build_crd_design(
    treatments = 2,
    replicates = 8,
    means = c(1, 2),
    sigma2 = 1
  )
  result <- calculate_power_results(
    design,
    test_type = "F-test & t-test",
    which = "trt",
    contrast = "pairwise"
  )

  expect_s3_class(result$omnibus, "data.frame")
  expect_s3_class(result$contrast, "data.frame")
})

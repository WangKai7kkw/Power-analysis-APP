# Reference: https://an-ethz.github.io/pwr4exp/articles/pwr4exp.html
# Expected values below are the rendered results in the official pwr4exp 1.0.1
# vignette. They intentionally do not come from this app's previous output.

test_that("official CRD omnibus example remains stable", {
  design <- build_crd_design(
    treatments = 4,
    replicates = 8,
    means = c(35, 30, 37, 38),
    sigma2 = 15
  )

  result <- calculate_power_results(design, test_type = "F-test")$omnibus

  expect_equal(row.names(result), "trt")
  expect_equal(result$NumDF, 3)
  expect_equal(result$DenDF, 28)
  expect_equal(result$sig.level, 0.05)
  expect_equal(result$power, 0.95467, tolerance = 1e-5)
})

test_that("official CRD pairwise contrasts remain stable", {
  design <- build_crd_design(
    treatments = 4,
    replicates = 8,
    means = c(35, 30, 37, 38),
    sigma2 = 15
  )

  result <- calculate_power_results(
    design,
    test_type = "t-test",
    which = "trt",
    contrast = "pairwise"
  )$contrast

  expect_equal(result$effect, c(5, -2, -3, -7, -8, -1))
  expect_equal(
    result$power,
    c(0.70287390, 0.16949749, 0.32168033, 0.93677955, 0.97860686, 0.07896844),
    tolerance = 1e-8
  )
})

test_that("official CRD polynomial and custom contrasts remain stable", {
  design <- build_crd_design(
    treatments = 4,
    replicates = 8,
    means = c(35, 30, 37, 38),
    sigma2 = 15
  )

  polynomial <- calculate_power_results(
    design,
    test_type = "t-test",
    which = "trt",
    contrast = "poly"
  )$contrast
  custom <- calculate_power_results(
    design,
    test_type = "t-test",
    which = "trt",
    contrast = c(-1, 1 / 3, 1 / 3, 1 / 3)
  )$contrast

  expect_equal(polynomial$effect, c(16, 6, -18))
  expect_equal(polynomial$power, c(0.7130735, 0.5617849, 0.8098383), tolerance = 1e-7)
  expect_equal(custom$effect, 0, tolerance = 1e-12)
  expect_equal(custom$power, 0.05, tolerance = 1e-12)
})

test_that("official RCBD omnibus and conditioned contrasts remain stable", {
  design <- build_rcbd_design(
    treatments = c(2, 2),
    blocks = 8,
    means = c(35, 40, 38, 41),
    vcomp = 11,
    sigma2 = 4
  )

  result <- calculate_power_results(
    design,
    test_type = "F-test & t-test",
    which = "facA",
    by = "facB",
    contrast = "pairwise"
  )

  expect_equal(row.names(result$omnibus), c("facA", "facB", "facA:facB"))
  expect_equal(result$omnibus$power, c(0.99969, 0.76950, 0.27138), tolerance = 1e-5)
  expect_equal(result$contrast[[1]]$power, 0.9974502, tolerance = 1e-7)
  expect_equal(result$contrast[[2]]$power, 0.8160596, tolerance = 1e-7)
})

test_that("official repeated-measures AR1 example remains stable", {
  n_subject <- 6
  n_treatment <- 3
  n_hour <- 8
  data <- data.frame(
    subject = factor(rep(seq_len(n_treatment * n_subject), each = n_hour)),
    hour = factor(rep(seq_len(n_hour), n_subject * n_treatment)),
    trt = rep(c("CON", "TRT1", "TRT2"), each = n_subject * n_hour)
  )
  means <- c(
    1, 2.50, 3.50, 1, 3.50, 4.54, 1, 3.98, 5.80, 1, 4.03, 5.40,
    1, 3.68, 5.49, 1, 3.35, 4.71, 1, 3.02, 4.08, 1, 2.94, 3.78
  )
  correlation <- build_correlation_spec(
    data,
    "corAR1",
    list(time = "hour", group = "subject", rho = 0.6)
  )
  design <- build_general_design(
    ~ trt * hour,
    correlation$df_model,
    means = means,
    sigma2 = 2,
    correlation = correlation$cor
  )

  result <- calculate_power_results(
    design,
    test_type = "F-test & t-test",
    which = "trt",
    by = "hour",
    contrast = "trt.vs.ctrl",
    p_adj = TRUE
  )

  expect_equal(result$omnibus$power, c(1, 0.74687, 0.38500), tolerance = 1e-5)
  expect_equal(
    result$contrast[[1]]$power,
    c(0.3299823, 0.7765112),
    tolerance = 1e-7
  )
  expect_equal(
    result$contrast[[2]]$power,
    c(0.7765112, 0.9777118),
    tolerance = 1e-7
  )
})

test_that("Latin square and split-plot vignette designs calculate valid power", {
  latin_square <- build_latin_square_design(
    treatments = c(2, 2),
    label = list(temp = c("T1", "T2"), dosage = c("D1", "D2")),
    squares = 4,
    reuse = "none",
    means = c(35, 40, 38, 41),
    vcomp = c(11, 2),
    sigma2 = 2
  )
  split_plot <- build_split_plot_design(
    trt.main = 2,
    trt.sub = 3,
    replicates = 10,
    means = c(20, 22, 22, 24, 24, 28),
    vcomp = 4,
    sigma2 = 11
  )

  for (design in list(latin_square, split_plot)) {
    result <- calculate_power_results(design, test_type = "F-test")$omnibus
    expect_true(nrow(result) >= 2)
    expect_true(all(is.finite(result$power)))
    expect_true(all(result$power >= 0 & result$power <= 1))
  }
})

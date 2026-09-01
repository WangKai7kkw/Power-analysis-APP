test_that("treatment label specifications validate clearly", {
  valid <- list(build_treatment_factor_spec(
    internal = "trt",
    display_name = "Diet",
    level_labels = "Control, Low dose, High dose",
    level_count = 3
  ))
  expect_null(validate_treatment_label_spec(valid))
  expect_true(treatment_labels_customised(valid))

  wrong_count <- list(build_treatment_factor_spec("trt", "Diet", "Control, Dose", 3))
  expect_match(validate_treatment_label_spec(wrong_count), "needs 3")

  duplicate_levels <- list(build_treatment_factor_spec("trt", "Diet", "Control, control", 2))
  expect_match(validate_treatment_label_spec(duplicate_levels), "unique")

  duplicate_factors <- list(
    build_treatment_factor_spec("facA", "Treatment", "A1, A2", 2),
    build_treatment_factor_spec("facB", "treatment", "B1, B2", 2)
  )
  expect_match(validate_treatment_label_spec(duplicate_factors), "unique name")
})

test_that("custom treatment names translate tables and result labels", {
  spec <- list(build_treatment_factor_spec(
    internal = "trt",
    display_name = "Diet",
    level_labels = "Control, Low dose, High dose",
    level_count = 3
  ))

  expect_equal(
    translate_design_labels(c("trt1", "trt2", "trt3"), spec),
    c("Control", "Low dose", "High dose")
  )
  expect_equal(translate_model_term("trt", spec), "Diet")
  expect_equal(
    translate_free_treatment_text("trt1 - trt3", spec),
    "Control - High dose"
  )

  result <- data.frame(
    `F-test` = "trt",
    Contrast = "trt1 - trt3",
    check.names = FALSE
  )
  translated <- translate_power_result_labels(result, spec)
  expect_equal(translated[["F-test"]], "Diet")
  expect_equal(translated$Contrast, "Control - High dose")
})

test_that("multifactor names remain qualified and exports are translated", {
  spec <- list(
    build_treatment_factor_spec("facA", "Diet", "Control, Supplement", 2),
    build_treatment_factor_spec("facB", "Week", "Week 1, Week 2", 2)
  )

  expect_equal(
    translate_design_labels(c("facA1:facB2", "facA2:facB1"), spec),
    c("Diet: Control × Week: Week 2", "Diet: Supplement × Week: Week 1")
  )
  expect_equal(translate_model_term("facA:facB", spec), "Diet × Week")
  expect_equal(translate_model_term("facA : facB", spec), "Diet × Week")
  expect_equal(
    translate_model_formula_text(
      "facA + facB + facA : facB + ( 1 | block )",
      spec
    ),
    "Diet + Week + Diet : Week + ( 1 | block )"
  )

  collision_spec <- list(
    build_treatment_factor_spec("facA", "facB", "facB1, Other", 2),
    build_treatment_factor_spec("facB", "Time", "Week 1, Week 2", 2)
  )
  expect_equal(
    translate_model_formula_text("facA + facB", collision_spec),
    "facB + Time"
  )
  expect_equal(
    translate_free_treatment_text("facA1 - facB1", collision_spec),
    "facB: facB1 - Time: Week 1"
  )

  export <- matrix(c("facA:facB", "facA1 - facA2"), ncol = 1)
  translated <- translate_power_export(export, spec)
  expect_equal(translated[, 1], c("Diet × Week", "Diet: Control - Diet: Supplement"))

  data_frame_export <- data.frame(
    section = c("facA:facB", "facA1 - facA2"),
    value = c("0.80", "0.90"),
    check.names = FALSE
  )
  translated_data_frame <- translate_power_export(data_frame_export, spec)
  expect_s3_class(translated_data_frame, "data.frame")
  expect_equal(
    translated_data_frame$section,
    c("Diet × Week", "Diet: Control - Diet: Supplement")
  )
  expect_equal(translated_data_frame$value, c("0.80", "0.90"))
})

test_that("default generated labels remain unchanged", {
  spec <- list(build_treatment_factor_spec("trt", "trt", "trt1, trt2", 2))
  expect_false(treatment_labels_customised(spec))
  expect_equal(translate_design_labels(c("trt1", "trt2"), spec), c("trt1", "trt2"))
  expect_equal(translate_model_formula_text("trt + (1 | block)", spec), "trt + (1 | block)")
})

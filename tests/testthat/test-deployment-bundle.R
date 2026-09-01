test_that("deployment bundle excludes development-only files", {
  app_dir <- normalizePath(file.path("..", ".."), mustWork = TRUE)
  files <- rsconnect::listDeploymentFiles(app_dir)

  excluded_directories <- c(".github/", "docs/", "tests/")
  for (directory in excluded_directories) {
    expect_false(any(startsWith(files, directory)), info = directory)
  }

  expect_false(any(c(
    ".gitignore", ".Rprofile", "Power-analysis-APP.Rproj", "README.md"
  ) %in% files))
  expect_true(all(c(
    "app.R", "DESCRIPTION", "R/app_helpers.R", "renv.lock"
  ) %in% files))
})

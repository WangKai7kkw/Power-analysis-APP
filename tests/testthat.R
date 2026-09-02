library(testthat)

# GitHub-hosted runners can need longer than chromote's 10-second default to
# cold-start Chrome and expose its debugging port. Keep E2E failures strict,
# but allow the browser enough time to start in CI.
if (identical(tolower(Sys.getenv("CI")), "true")) {
  options(chromote.timeout = max(getOption("chromote.timeout", 10), 60))
}

test_dir("tests/testthat", reporter = "summary")

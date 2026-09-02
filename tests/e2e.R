library(testthat)

# Browser startup can take longer on shared or cold CI hosts. This runner is
# deliberately separate from testthat.R so deployment does not depend on a
# locally available browser.
if (identical(tolower(Sys.getenv("CI")), "true")) {
  options(chromote.timeout = max(getOption("chromote.timeout", 10), 60))
}

test_dir("tests/e2e", reporter = "summary")

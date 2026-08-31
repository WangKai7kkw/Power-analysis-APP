app_environment <- new.env(parent = globalenv())
source("app.R", local = app_environment)

stopifnot(
  is.function(app_environment$server),
  inherits(app_environment$ui, "shiny.tag.list")
)

shiny::testServer(app_environment$server, {
  session$setInputs(
    start_btn = 1,
    design_title = "Completely Randomized Design",
    num_trt = 1,
    level_numbers = "4",
    num_rep = 8,
    interaction_option = "No",
    Type = "F-test",
    Type_ss = "Type III",
    p_value1 = 0.05
  )
  session$flushReact()
})

message("App construction and server smoke test passed.")

runtime_packages <- c(
  "pwr4exp", "rhandsontable", "shiny", "bslib", "data.table", "readxl", "nlme"
)
missing_packages <- runtime_packages[
  !vapply(runtime_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    paste0(
      "Missing required R packages: ", paste(missing_packages, collapse = ", "),
      ". Restore the project dependencies before starting the app."
    ),
    call. = FALSE
  )
}
invisible(lapply(runtime_packages, library, character.only = TRUE))
source(file.path("R", "app_helpers.R"), local = TRUE)

section_header <- function(icon_name, title, description) {
  div(
    class = "section-heading",
    div(class = "section-icon", icon(icon_name)),
    div(
      tags$h3(title),
      tags$p(description)
    )
  )
}

panel_header <- function(step, eyebrow, title, description) {
  div(
    class = "panel-heading",
    div(class = "step-badge", step),
    div(
      div(class = "panel-eyebrow", eyebrow),
      tags$h2(title),
      tags$p(description)
    )
  )
}

field_note <- function(...) {
  div(class = "field-note", icon("circle-info"), tags$span(...))
}

friendly_term_choices <- function(values) {
  labels <- values
  labels <- gsub("trt\\.main", "Whole-plot treatment", labels)
  labels <- gsub("trt\\.sub", "Subplot treatment", labels)
  labels <- gsub("fac([A-Z])\\.main", "Whole-plot factor \\1", labels)
  labels <- gsub("fac([A-Z])\\.sub", "Subplot factor \\1", labels)
  labels <- gsub("fac([A-Z])", "Factor \\1", labels)
  labels <- gsub(":", " × ", labels, fixed = TRUE)
  stats::setNames(values, labels)
}

friendly_contrast_choices <- function(values) {
  labels <- c(
    pairwise = "All pairwise comparisons",
    poly = "Polynomial trend contrasts",
    trt.vs.ctrl = "Each treatment versus control",
    `Contrast vector` = "Custom coefficient vector"
  )
  stats::setNames(values, unname(labels[values]))
}

result_table_block <- function(title, description, output) {
  div(
    class = "result-block",
    tags$h3(title),
    tags$p(description),
    div(class = "result-table-wrap", output)
  )
}

interpretation_guide <- function() {
  div(
    class = "interpretation-note",
    tags$strong("How to read power: "),
    "Power is the probability of detecting the specified effect under these assumptions. Values nearer 1 indicate a greater chance of detection. A target such as 0.80 is common, but the appropriate threshold depends on the scientific and practical consequences of a missed effect."
  )
}

result_empty_state <- function(stale = FALSE) {
  div(
    class = "result-empty",
    div(
      div(class = "result-empty-icon", icon(if (stale) "rotate" else "chart-column")),
      tags$h3(if (stale) "Assumptions changed" else "Results will appear here"),
      tags$p(if (stale) {
        "Run the analysis again so the displayed power matches your updated assumptions."
      } else {
        "Complete the design assumptions, choose the result type, and select Run power analysis."
      })
    )
  )
}

app_css <- "
  :root {
    --ink: #17243d;
    --muted: #60708a;
    --primary: #2563eb;
    --primary-dark: #1d4ed8;
    --primary-soft: #eff6ff;
    --accent: #0f766e;
    --surface: #ffffff;
    --surface-subtle: #f7f9fc;
    --border: #dce3ee;
    --warning-soft: #fff8e6;
    --danger: #b42318;
    --success: #18794e;
    --shadow: 0 10px 30px rgba(23, 36, 61, .08);
  }

  html, body, .bslib-page-fill {
    min-height: 100%;
    height: auto !important;
  }

  body {
    margin: 0;
    overflow-y: auto !important;
    background: #f3f6fb;
    color: var(--ink);
    font-family: Inter, ui-sans-serif, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    font-size: 15px;
    line-height: 1.5;
  }

  .app-shell {
    width: min(1760px, 100%);
    margin: 0 auto;
    padding: 24px 28px 32px;
  }

  .app-topbar {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 24px;
    margin-bottom: 18px;
  }

  .brand-lockup { display: flex; align-items: center; gap: 12px; }
  .brand-mark {
    display: grid;
    place-items: center;
    width: 42px;
    height: 42px;
    border-radius: 12px;
    color: #fff;
    background: linear-gradient(145deg, #2563eb, #0f766e);
    box-shadow: 0 8px 18px rgba(37, 99, 235, .22);
    font-size: 18px;
  }
  .brand-name { font-size: 21px; font-weight: 780; letter-spacing: -.02em; }
  .brand-tagline { color: var(--muted); font-size: 13px; margin-top: -2px; }

  .about-button {
    border: 1px solid var(--border) !important;
    border-radius: 10px !important;
    background: #fff !important;
    color: var(--ink) !important;
    font-weight: 650 !important;
    padding: 9px 14px !important;
  }

  .design-selector-card {
    display: grid;
    grid-template-columns: minmax(260px, 1fr) minmax(340px, 620px);
    align-items: end;
    gap: 32px;
    padding: 24px 26px;
    border: 1px solid #cfe0ff;
    border-radius: 18px;
    background: linear-gradient(120deg, #fff 20%, #eff6ff 100%);
    box-shadow: var(--shadow);
  }
  .design-selector-card h1 {
    margin: 0 0 6px;
    font-size: clamp(26px, 2vw, 34px);
    line-height: 1.15;
    letter-spacing: -.035em;
    font-weight: 780;
  }
  .design-selector-card p { margin: 0; max-width: 690px; color: var(--muted); }
  .design-control { display: grid; grid-template-columns: auto 1fr; gap: 12px; align-items: end; }
  .design-control .form-group { margin-bottom: 0; }
  .step-kicker {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 34px;
    height: 38px;
    margin-bottom: 1px;
    border-radius: 10px;
    background: var(--primary);
    color: #fff;
    font-weight: 750;
  }

  .workflow-map {
    display: grid;
    grid-template-columns: repeat(4, minmax(0, 1fr));
    gap: 8px;
    margin: 16px 0;
  }
  .workflow-map-item {
    display: flex;
    align-items: center;
    gap: 10px;
    min-width: 0;
    padding: 10px 12px;
    color: var(--muted);
    font-size: 13px;
    font-weight: 650;
  }
  .workflow-map-item span {
    display: grid;
    place-items: center;
    flex: 0 0 auto;
    width: 24px;
    height: 24px;
    border-radius: 50%;
    background: #e5edfa;
    color: #36506f;
    font-size: 12px;
  }

  .app-panels {
    display: grid;
    grid-template-columns: minmax(380px, 1.15fr) minmax(300px, .78fr) minmax(370px, 1.07fr);
    grid-template-areas: 'design test results';
    gap: 18px;
    align-items: start;
  }
  .design-panel { grid-area: design; }
  .test-panel { grid-area: test; }
  .results-panel { grid-area: results; }

  .workflow-card.card {
    overflow: hidden;
    border: 1px solid var(--border);
    border-radius: 16px;
    background: var(--surface);
    box-shadow: var(--shadow);
  }
  .workflow-card > .card-header {
    padding: 18px 20px;
    border-bottom: 1px solid var(--border);
    background: #fff;
  }
  .panel-heading { display: flex; align-items: flex-start; gap: 13px; }
  .step-badge {
    display: grid;
    place-items: center;
    flex: 0 0 auto;
    width: 34px;
    height: 34px;
    border-radius: 10px;
    background: var(--primary-soft);
    color: var(--primary-dark);
    font-weight: 800;
  }
  .panel-eyebrow {
    color: var(--primary-dark);
    font-size: 11px;
    font-weight: 800;
    letter-spacing: .09em;
    text-transform: uppercase;
  }
  .panel-heading h2 { margin: 1px 0 2px; font-size: 19px; line-height: 1.25; font-weight: 750; }
  .panel-heading p { margin: 0; color: var(--muted); font-size: 13px; font-weight: 400; }
  .panel-body { padding: 18px 20px 20px; }
  .panel-scroll { max-height: calc(100vh - 330px); overflow: auto; scrollbar-gutter: stable; }

  #dynamic_sidebar > .card,
  #dynamic_sidebar > .shiny-html-output > .card,
  #dynamic_sidebar .card {
    margin: 0 0 14px !important;
    padding: 16px !important;
    height: auto !important;
    border: 1px solid var(--border) !important;
    border-radius: 13px !important;
    background: var(--surface-subtle) !important;
    box-shadow: none !important;
  }
  .section-heading { display: flex; gap: 10px; align-items: flex-start; margin-bottom: 13px; }
  .section-icon {
    display: grid;
    place-items: center;
    flex: 0 0 auto;
    width: 30px;
    height: 30px;
    border-radius: 9px;
    background: #eaf1ff;
    color: var(--primary-dark);
  }
  .section-heading h3 { margin: 0; font-size: 16px; font-weight: 740; }
  .section-heading p { margin: 2px 0 0; color: var(--muted); font-size: 12px; line-height: 1.4; }

  label, .control-label { margin-bottom: 6px !important; color: #263650; font-size: 13px; font-weight: 650 !important; }
  .form-group { margin-bottom: 14px; }
  .form-control, .selectize-input, .selectize-control.single .selectize-input {
    min-height: 40px;
    border-color: #cbd5e1;
    border-radius: 9px;
    background: #fff;
    box-shadow: none !important;
    font-size: 14px;
  }
  .form-control:focus, .selectize-input.focus {
    border-color: var(--primary) !important;
    box-shadow: 0 0 0 3px rgba(37, 99, 235, .13) !important;
  }
  .field-note {
    display: flex;
    align-items: flex-start;
    gap: 7px;
    margin: -6px 0 13px;
    color: var(--muted);
    font-size: 12px;
    line-height: 1.45;
  }
  .field-note svg, .field-note i { margin-top: 2px; color: var(--primary); }
  .model-preview {
    margin: 2px 0 14px;
    padding: 12px 13px;
    border: 1px solid #cfe0ff;
    border-radius: 10px;
    background: var(--primary-soft);
    color: #29466c;
    font-size: 13px;
  }
  .model-preview strong { display: block; margin-bottom: 4px; color: #1f3b63; }

  .btn-primary {
    border-color: var(--primary) !important;
    border-radius: 10px !important;
    background: var(--primary) !important;
    font-weight: 700 !important;
    min-height: 44px;
    box-shadow: 0 7px 14px rgba(37, 99, 235, .18);
  }
  .btn-primary:hover { border-color: var(--primary-dark) !important; background: var(--primary-dark) !important; }
  #create_result { margin-top: 8px; }

  .result-empty {
    display: grid;
    place-items: center;
    min-height: 290px;
    padding: 34px 24px;
    text-align: center;
    border: 1px dashed #bdc9d9;
    border-radius: 13px;
    background: var(--surface-subtle);
  }
  .result-empty-icon {
    display: grid;
    place-items: center;
    width: 48px;
    height: 48px;
    margin: 0 auto 13px;
    border-radius: 14px;
    background: var(--primary-soft);
    color: var(--primary);
    font-size: 20px;
  }
  .result-empty h3 { margin: 0 0 6px; font-size: 17px; }
  .result-empty p { margin: 0 auto; max-width: 370px; color: var(--muted); font-size: 13px; }
  .result-block { margin-bottom: 16px; }
  .result-block h3 { margin: 0 0 4px; font-size: 16px; font-weight: 740; }
  .result-block > p { margin: 0 0 10px; color: var(--muted); font-size: 12px; }
  .result-table-wrap {
    overflow-x: auto;
    padding: 6px 10px;
    border: 1px solid var(--border);
    border-radius: 11px;
    background: #fff;
  }
  .result-table-wrap table { margin-bottom: 0 !important; font-size: 13px; white-space: nowrap; }
  .result-table-wrap th { color: #31435f; background: #f5f8fc; }
  .interpretation-note {
    margin-top: 15px;
    padding: 13px 14px;
    border-left: 3px solid var(--accent);
    border-radius: 0 9px 9px 0;
    background: #effaf8;
    color: #34524f;
    font-size: 12px;
  }
  .download-area { margin-top: 14px; }
  .download-area .btn { width: 100%; }
  .download-placeholder {
    padding: 10px 12px;
    border: 1px solid var(--border);
    border-radius: 10px;
    color: #7a879a;
    background: #f6f8fb;
    text-align: center;
    font-size: 13px;
  }

  .alert { border-radius: 10px; font-size: 13px; }
  .shiny-notification { border-radius: 11px !important; box-shadow: var(--shadow); }
  .modal-content { overflow: hidden; border: 0; border-radius: 18px; box-shadow: 0 24px 70px rgba(23,36,61,.28); }
  .modal-dialog { width: min(92%, 620px) !important; max-width: 620px !important; margin: 7vh auto 0 !important; }
  .welcome-modal { padding: 34px 34px 28px; text-align: left; }
  .welcome-modal .welcome-icon {
    display: grid; place-items: center; width: 48px; height: 48px; border-radius: 14px;
    background: linear-gradient(145deg, #2563eb, #0f766e); color: #fff; font-size: 20px;
  }
  .welcome-modal h2 { margin: 20px 0 8px; font-size: 28px; letter-spacing: -.03em; }
  .welcome-modal .lead { margin: 0 0 17px; color: var(--muted); font-size: 16px; }
  .welcome-checklist { display: grid; gap: 9px; margin: 20px 0 24px; padding: 0; list-style: none; }
  .welcome-checklist li { display: flex; gap: 9px; align-items: flex-start; color: #3a4b65; font-size: 14px; }
  .welcome-checklist i, .welcome-checklist svg { margin-top: 3px; color: var(--success); }
  .welcome-byline { margin: 18px 0 0; color: #7a879a; font-size: 12px; text-align: center; }

  @media (max-width: 1280px) {
    .app-panels {
      grid-template-columns: minmax(420px, 1.1fr) minmax(330px, .9fr);
      grid-template-areas: 'design test' 'design results';
    }
    .panel-scroll { max-height: none; }
  }

  @media (max-width: 860px) {
    .app-shell { padding: 16px 14px 26px; }
    .design-selector-card { grid-template-columns: 1fr; gap: 18px; padding: 20px; }
    .app-panels { grid-template-columns: 1fr; grid-template-areas: 'design' 'test' 'results'; }
    .workflow-map { grid-template-columns: repeat(2, 1fr); }
    .app-topbar { align-items: flex-start; }
  }

  @media (max-width: 520px) {
    .brand-tagline { display: none; }
    .about-button { font-size: 13px !important; }
    .design-control { grid-template-columns: 1fr; }
    .step-kicker { display: none; }
    .workflow-map { gap: 2px; }
    .workflow-map-item { padding: 8px 5px; font-size: 11px; }
    .workflow-card > .card-header, .panel-body { padding: 15px; }
    .welcome-modal { padding: 26px 22px 22px; }
  }
"

ui <- page_fillable(
  theme = bs_theme(preset = "cosmo", primary = "#2563eb"),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(app_css))
  ),
  uiOutput("main_ui")
)

server<-function(input,output,session) {
  
  page_started <- reactiveVal(FALSE)
  results_generated <- reactiveVal(FALSE)
  design_revision <- reactiveVal(0L)
  
  ui_main<-function(){
    div(
      class = "app-shell",
      tags$header(
        class = "app-topbar",
        div(
          class = "brand-lockup",
          div(class = "brand-mark", icon("chart-line")),
          div(
            div(class = "brand-name", "pwr4exp"),
            div(class = "brand-tagline", "Power analysis for designed experiments")
          )
        ),
        tags$div(class = "dropdown",
                 tags$button(
                   tagList(icon("circle-info"), " About & resources"),
                   class = "btn btn-light dropdown-toggle about-button",
                   type = "button",
                   `data-bs-toggle` = "dropdown",
                   `aria-expanded` = "false"
                 ),
                 tags$ul(
                   class = "dropdown-menu",
                   tags$li(
                     tags$a(icon("code-branch"), " Package source code", rel="noopener noreferrer", class="dropdown-item", href="https://github.com/an-ethz/pwr4exp", target="_blank")
                   ),
                   tags$li(
                     tags$a(icon("book-open"), " Method documentation", rel="noopener noreferrer", class="dropdown-item", href="https://an-ethz.github.io/pwr4exp/articles/pwr4exp.html", target="_blank")
                   )
                 )
        )
      ),
      div(
        class = "design-selector-card",
        div(
          tags$h1("Plan your power analysis"),
          tags$p("Describe the experiment you intend to run, enter realistic assumptions, then estimate the chance of detecting the effects that matter.")
        ),
        div(
          class = "design-control",
          div(class = "step-kicker", "1"),
          selectInput(
            inputId = "design_title",
            label = "Experimental design",
            choices = c(
              "Completely randomized (CRD)" = "Completely Randomized Design",
              "Randomized complete block (RCBD)" = "Randomized Complete Block Design",
              "Latin square" = "Latin Square Design",
              "Split-plot" = "Split Plot Design",
              "Custom design from uploaded data" = "General Design"
            ),
            selected = "Completely Randomized Design",
            width = '100%'
          )
        )
      ),
      tags$nav(
        class = "workflow-map",
        `aria-label` = "Power analysis workflow",
        div(class = "workflow-map-item", tags$span("1"), "Choose a design"),
        div(class = "workflow-map-item", tags$span("2"), "Enter assumptions"),
        div(class = "workflow-map-item", tags$span("3"), "Configure the test"),
        div(class = "workflow-map-item", tags$span("4"), "Review & export")
      ),
      
      div(
        class = "app-panels",
        
        div(
          class = "app-panel design-panel",
          card(
            class = "workflow-card",
            card_header(panel_header("2", "Design assumptions", "Describe the study", "Set factor levels, sample size, expected means, and sources of variation.")),
            div(
              class = "panel-body panel-scroll",
              uiOutput("dynamic_sidebar")
            ),
            full_screen = TRUE
          )
        ),
        
        div(
          class = "app-panel test-panel",
          card(
            class = "workflow-card",
            card_header(panel_header("3", "Analysis settings", "Choose what to test", "Use an overall F-test, focused contrasts, or both.")),
            div(
              class = "panel-body panel-scroll",
              div(
                selectInput(
                  inputId = 'Type',
                  label = 'Power results to calculate',
                  choices = c(
                    'Overall effects (F-test)' = 'F-test',
                    'Specific comparisons (t-tests)' = 't-test',
                    'Overall effects and comparisons' = 'F-test & t-test'
                  ),
                  selected = 'F-test',
                  width = "100%"
                ),
                field_note("Choose overall effects for a first-pass analysis; add comparisons when particular treatment differences are your main question.")
              ),
              uiOutput('test_options_ui')
            ),
            full_screen = TRUE
          )
        ),
        
        div(
          class = "app-panel results-panel",
          card(
            class = "workflow-card",
            card_header(panel_header("4", "Decision support", "Review the results", "Compare power across effects and download a record of the analysis.")),
            div(
              class = "panel-body panel-scroll",
              uiOutput("results_display")
            ),
            div(
              class = "panel-body download-area",
              uiOutput("download_actions")
            ),
            full_screen = TRUE
          )
        )
      )
    )
  }
  
  output$main_ui <- renderUI({
    ui_main()
  })

  output$download_actions <- renderUI({
    if (isTRUE(results_generated())) {
      tagList(
        downloadButton("download_all", tagList(icon("download"), " Download results as CSV"), class = "btn-primary"),
        field_note("The export includes the selected design and the result tables currently shown above.")
      )
    } else {
      div(class = "download-placeholder", icon("lock"), " Run the calculation to enable download")
    }
  })
  
  observe({
    if (!page_started()) {
      
      showModal(
        modalDialog(
          size = "l",
          easyClose = FALSE,
          fade = TRUE,
          
          div(
            class = "welcome-modal",
            div(class = "welcome-icon", icon("chart-line")),
            h2("Plan an experiment with confidence"),
            p(class = "lead", "Estimate statistical power for standard and custom experiments analyzed with linear mixed models."),
            tags$ul(
              class = "welcome-checklist",
              tags$li(icon("circle-check"), tags$span("Choose the design that matches how treatments are assigned.")),
              tags$li(icon("circle-check"), tags$span("Enter plausible means and variance estimates from prior data or a pilot study.")),
              tags$li(icon("circle-check"), tags$span("Calculate overall or comparison-specific power, then export the results."))
            ),
            actionButton(
              "start_btn", tagList("Start planning ", icon("arrow-right")),
              class = "btn btn-primary",
              style="width:100%;"
            ),
            p(class = "welcome-byline", "Developed by Ao Wang and Kai Wang")
          ),
          
          footer = NULL
        )
      )
    }
  })
  
  observeEvent(input$start_btn, {
    page_started(TRUE)
    removeModal()
  })

  observeEvent(input$design_title, {
    session$onFlushed(function() {
      design_revision(isolate(design_revision()) + 1L)
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  generate_factor_combinations <- function(n){
    generate_factor_combinations_safe(n)
  }

  factor_count_result <- function(value) {
    tryCatch(
      list(value = validate_factor_count(value), message = NULL),
      error = function(error) {
        list(value = NULL, message = conditionMessage(error))
      }
    )
  }

  factor_count_warning <- function(message) {
    div(
      class = "alert alert-warning",
      role = "alert",
      style = "margin-top:6px; padding:8px 12px;",
      icon("triangle-exclamation"),
      tags$span(style = "margin-left:6px;", message)
    )
  }
  
  filter_combinations <- function(all_combinations, exclude_factors) {
    if (length(exclude_factors) == 0) return(friendly_term_choices(all_combinations))
    exclude_factors <- unlist(strsplit(exclude_factors, "\\:"))
    filtered <- sapply(all_combinations, function(comb) {
      factors <- unlist(strsplit(comb, "\\:"))
      !any(factors %in% exclude_factors)
    })
    values <- c("NULL", all_combinations[filtered])
    labels <- c("No conditioning variable", unname(names(friendly_term_choices(all_combinations[filtered]))))
    stats::setNames(values, labels)
  }
  
  generate_spd_factors <- function(num_trt_main, num_trt_sub) {
    validate_factor_count(num_trt_main)
    validate_factor_count(num_trt_sub)
    validate_factor_count(num_trt_main + num_trt_sub)

    make_factors <- function(prefix, n) {
      if (n == 1) {
        return(paste0("trt.", prefix))
      } else {
        letters_used <- LETTERS[1:n]
        return(paste0("fac", letters_used, ".", prefix))
      }
    }
    
    main_factors <- make_factors("main", num_trt_main)
    sub_factors  <- make_factors("sub", num_trt_sub)
    
    all_factors <- c(main_factors, sub_factors)
    
    interaction_terms <- c()
    if (length(all_factors) >= 2) {
      for (k in 2:length(all_factors)) {
        combs <- combn(all_factors, k, simplify = FALSE)
        inter_terms_k <- sapply(combs, function(x) paste(x, collapse = ":"))
        interaction_terms <- c(interaction_terms, inter_terms_k)
      }
    }
    
    all_terms <- c(all_factors, interaction_terms)
    return(all_terms)
  }
  
  output$dynamic_sidebar<-renderUI({
    req(page_started())
    req(input$design_title)
    if(input$design_title=='Completely Randomized Design'){
      tagList(
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("layer-group", "Treatment structure", "Define the factors whose effects you want to detect."),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_trt", "Treatment factors",
                               value = 1, min = 1, max = MAX_TREATMENT_FACTORS,
                               step = 1,
                               width = '100%')
              )
          ),
          div(style="margin-bottom: 2px;",
              uiOutput("level_number_ui")
          )
        ),
        
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("users", "Replication", "Set how many independent experimental units receive each treatment combination."),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_rep", "Replicates per treatment combination",
                               value = 8,min=1,width='100%')
              )
          )
        ),
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("diagram-project", "Analysis model", "Review the generated model and include interactions only when scientifically meaningful."),
          
          uiOutput('model_ui'),
          uiOutput('interaction_exist_ui'),
          uiOutput('interaction_fac_ui')
        ),
        
        uiOutput('input_data_ui'),
        uiOutput('input_variance_ui'),
        
        tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
        
      )
    }else if(input$design_title=='Randomized Complete Block Design'){
      tagList(
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("layer-group", "Treatment structure", "Define the factors whose effects you want to detect."),
          
          div(style = "display: flex; align-items: center;width:100%;;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_trt", "Treatment factors",
                               value = 1, min = 1, max = MAX_TREATMENT_FACTORS,
                               step = 1,
                               width = "100%")
              )
          ),
          div(style="margin-bottom: 2px;",
              uiOutput("level_number_ui")
          )
        ),
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("table-cells-large", "Blocks", "Each complete block contains every treatment combination once."),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_block", "Complete blocks", value = 8,min=1,width="100%")
              )
          )
        ),
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("diagram-project", "Analysis model", "The generated model includes a random intercept for block."),
          
          uiOutput('interaction_exist_ui'),
          uiOutput('interaction_fac_ui'),
          uiOutput('model_ui')
        ),
        
        uiOutput('input_data_ui'),
        uiOutput('input_variance_ui'),
        
        tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
        
      )
    }else if(input$design_title=='Latin Square Design'){
      tagList(
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("layer-group", "Treatment structure", "Define the treatments arranged within each square."),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_trt", "Treatment factors",
                               value = 1, min = 1, max = MAX_TREATMENT_FACTORS,
                               step = 1,
                               width = "100%")
              )
          ),
          div(style="margin-bottom: 2px;",
              uiOutput("level_number_ui")
          )
        ),
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("border-all", "Square replication", "Set the number of squares and whether row or column blocks repeat across them."),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_squares", "Replicated squares", value = 4,min=1,width = '100%')
              )
          ),
          
          div(
            style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "value_reuse",
                label = "Blocks reused across squares",
                choices = c("Reuse row blocks" = 'row', "Reuse column blocks" = 'col', "Use new row and column blocks" = 'none'),
                selected = "none",
                width = "100%")
            ),
            tags$span(
              `data-toggle` = "tooltip",
              title = "Note: 'row' for reusing row blocks, 'col' for reusing column blocks, or 'none' for reusing neither row nor column blocks to replicate a single square.",
              style = "margin-left: 5px; cursor: pointer;",
              icon("question-circle")
            )
          )),
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("diagram-project", "Analysis model", "The generated model includes random row and column effects."),
          
          uiOutput('interaction_exist_ui'),
          uiOutput('interaction_fac_ui'),
          uiOutput('model_ui')
        ),
        uiOutput('input_data_ui'),
        uiOutput('input_variance_ui'),
        
        tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
      )
    }else if(input$design_title=='Split Plot Design'){
      tagList(
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("layer-group", "Treatment structure", "Separate factors applied to whole plots from those applied within plots."),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_trt_main", "Whole-plot factors",
                               value = 1, min = 1, max = MAX_TREATMENT_FACTORS,
                               step = 1,
                               width="100%")
              )
          ),
          
          div(style="margin-bottom: 2px;",
              uiOutput("level_numbers_main_ui")
          ),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_trt_sub", "Subplot factors",
                               value = 1, min = 1, max = MAX_TREATMENT_FACTORS,
                               step = 1,
                               width="100%")
              )
          ),
          div(style="margin-bottom: 2px;",
              uiOutput("level_numbers_sub_ui")
          )
        ),
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("users", "Whole-plot replication", "Set the number of independent whole plots assigned to each whole-plot treatment."),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                  numericInput("num_rep", "Whole plots per whole-plot treatment",
                               value = 10,min=1,width="100%")
              )
          )
        ),
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          section_header("diagram-project", "Analysis model", "The generated model accounts for whole-plot variation separately from residual error."),
          
          uiOutput('interaction_exist_ui'),
          uiOutput('interaction_fac_ui'),
          uiOutput('model_ui')
        ),
        uiOutput('input_data_ui'),
        uiOutput('input_variance_ui'),
        
        tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
        
      )
    }else if(input$design_title=='General Design'){
      tagList(
        bslib::card(
        div(
          style = "display: flex; flex-direction: column;width:100%;",
          section_header("file-arrow-up", "Study layout", "Upload one row per observation or planned observation, with a separate column for each design variable.")
        ),
        
        div(
          style = "display: flex; align-items: center;width:100%;",
          div(
            style='flex:1;',
            fileInput(
              inputId = "uploaded_file",
              label = "Study data",
              accept = c(".csv", ".xlsx",".xls", ".txt", ".tsv"),
              buttonLabel = "Choose file",
              placeholder = "CSV, Excel, TSV, or TXT",
              width = "100%")
          )
        ),
        field_note("The app reads the column names from this file and uses them in the model and correlation settings."),
        
        uiOutput("file_feedback"),
        
        uiOutput("file_type_check")
        ),
        bslib::card(
        section_header("code", "Analysis model", "Describe fixed effects and random effects using the column names in your uploaded data."),
        
        div(style = "font-weight: bold;display: flex; align-items: center;width:100%;",
            div(style="flex:1;",
                textInput("Formula_general", "Model formula",
                          placeholder = "For example: ~ treatment + time + (1 | subject)",width = "100%")
            ),
            tags$span(
              `data-toggle` = "tooltip",
              title = "Enter the right-hand side only, beginning with ~. Random effects follow lme4 syntax, such as (1 | block).",
              style = "margin-left: 5px; cursor: pointer;",
              icon("question-circle")
            )
        ),
        
        uiOutput("level_numbers_validation3"),
        
        div(style = "font-weight: bold;display: flex; align-items: center;width:100%;",
            div(style="flex:1;",
                selectInput(
                  "cor_type",
                  "Within-group residual correlation",
                  choices = c(
                    "Independent residuals (none)" = "none",
                    "AR(1): equally spaced repeated measures" = "corAR1",
                    "ARMA: autoregressive moving average" = "corARMA",
                    "Continuous AR(1): unequally spaced time" = "corCAR1",
                    "Compound symmetry: constant correlation" = "corCompSymm",
                    "Exponential spatial correlation" = "corExp",
                    "Gaussian spatial correlation" = "corGaus",
                    "Linear spatial correlation" = "corLin",
                    "Unstructured correlation" = "corSymm",
                    "Ratio-based spatial correlation" = "corRatio",
                    "Spherical spatial correlation" = "corSpher"
                  ),
                  selected = "none",
                  width = "100%"
                )
            ),
            tags$a(
              href = "https://www.rdocumentation.org/packages/nlme/versions/3.1-168/topics/corClasses",
              target = "_blank",
              `data-toggle` = "tooltip",
              `data-bs-html` = "true",
              `data-bs-title` = "Note: Specifies residual (R-side) correlation structures using nlme::corClasses functions.<br>Click for documentation on correlation structures.",
              style = "margin-left: 8px; color: #000; font-size: 18px;",
              icon("question-circle")
            )
        ),
        field_note("Leave this as independent residuals unless observations within a subject, plot, or location are expected to remain correlated."),
        uiOutput("cor_params_ui"),
        uiOutput("cor_validation_ui")
        ),
        uiOutput('input_data_ui'),
        uiOutput('input_variance_ui'),
        tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
        
      )
    }
  })
  
  output$level_number_ui <- renderUI({
    req(page_started())
    req(!is.null(input$num_trt))
    count_result <- factor_count_result(input$num_trt)
    if (is.null(count_result$value)) {
      return(factor_count_warning(count_result$message))
    }
    factor_count <- count_result$value

    num_inputs <- lapply(seq_len(factor_count), function(i) {
      factor_name <- paste0("Factor ", LETTERS[i])
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        numericInput(
          inputId = paste0("factor_", i),
          label = factor_name,
          value = 2,
          min = 2,
          step = 1,
          width = "100px"
        )
      )
    })
    
    hidden_text_input <- tags$div(
      style = "display: none;",
      textInput(
        inputId = "level_numbers",
        label = NULL,
        value = paste(rep(2, factor_count), collapse = ",")
      )
    )
    
    observeEvent(
      lapply(seq_len(factor_count), function(i) input[[paste0("factor_", i)]]),
      {
        factor_values <- sapply(seq_len(factor_count), function(i) {
          input[[paste0("factor_", i)]] %||% 2
        })
        updateTextInput(
          session,
          inputId = "level_numbers",
          value = paste(factor_values, collapse = ",")
        )
      },
      ignoreNULL = FALSE
    )
    
    tagList(
      tags$label("Levels per treatment factor",
                 style = "margin-bottom: 10px; display: block;"),
      field_note("Enter the number of categories or treatment settings for each factor (minimum 2)."),
      div(
        style = "font-size: 12px;font-weight: bold;display: flex; gap: 10px; overflow-x: auto; align-items: flex-end;",
        num_inputs
      ),
      hidden_text_input
    )
  })
  
  output$level_numbers_main_ui <- renderUI({
    req(page_started())
    req(!is.null(input$num_trt_main))
    count_result <- factor_count_result(input$num_trt_main)
    if (is.null(count_result$value)) {
      return(factor_count_warning(count_result$message))
    }
    factor_count <- count_result$value
    
    num_inputs_main <- lapply(seq_len(factor_count), function(i) {
      factor_name_main <- paste0("Factor ", LETTERS[i])
      
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        numericInput(
          inputId = paste0("factor_main_", i),
          label = factor_name_main,
          value = 2,
          min = 2,
          step = 1,
          width = "100px"
        )
      )
    })
    
    hidden_text_input_main <- tags$div(
      style = "display: none;",
      textInput(
        inputId = "level_numbers_main",
        label = NULL,
        value = paste(rep(2, factor_count), collapse = ",")
      )
    )
    
    observeEvent(
      lapply(seq_len(factor_count), function(i) input[[paste0("factor_main_", i)]]),
      {
        factor_values_main <- sapply(seq_len(factor_count), function(i) {
          input[[paste0("factor_main_", i)]] %||% 2
        })
        updateTextInput(
          session,
          inputId = "level_numbers_main",
          value = paste(factor_values_main, collapse = ",")
        )
      },
      ignoreNULL = FALSE
    )
    
    tagList(
      tags$label("Levels per whole-plot factor",
                 style = "margin-bottom: 10px; display: block;"),
      field_note("Enter the number of settings applied at the whole-plot level."),
      div(
        style = "font-size: 12px;font-weight: bold;display: flex; gap: 10px; overflow-x: auto; align-items: flex-end;",
        num_inputs_main
      ),
      hidden_text_input_main
    )
  })
  
  output$level_numbers_sub_ui <- renderUI({
    req(page_started())
    req(!is.null(input$num_trt_sub))
    count_result <- factor_count_result(input$num_trt_sub)
    if (is.null(count_result$value)) {
      return(factor_count_warning(count_result$message))
    }
    factor_count <- count_result$value
    
    num_inputs_sub <- lapply(seq_len(factor_count), function(i) {
      factor_name_sub <- paste0("Factor ", LETTERS[i])
      
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        numericInput(
          inputId = paste0("factor_sub_", i),
          label = factor_name_sub,
          value = 2,
          min = 2,
          step = 1,
          width = "100px"
        )
      )
    })
    
    hidden_text_input_sub <- tags$div(
      style = "display: none;",
      textInput(
        inputId = "level_numbers_sub",
        label = NULL,
        value = paste(rep(2, factor_count), collapse = ",")
      )
    )
    
    observeEvent(
      lapply(seq_len(factor_count), function(i) input[[paste0("factor_sub_", i)]]),
      {
        factor_values_sub <- sapply(seq_len(factor_count), function(i) {
          input[[paste0("factor_sub_", i)]] %||% 2
        })
        updateTextInput(
          session,
          inputId = "level_numbers_sub",
          value = paste(factor_values_sub, collapse = ",")
        )
      },
      ignoreNULL = FALSE
    )
    
    tagList(
      tags$label("Levels per subplot factor",
                 style = "margin-bottom: 10px; display: block;"),
      field_note("Enter the number of settings applied within each whole plot."),
      div(
        style = "font-size: 12px;font-weight: bold;display: flex; gap: 10px; overflow-x: auto; align-items: flex-end;",
        num_inputs_sub
      ),
      hidden_text_input_sub
    )  
  })
  
  observeEvent(input$uploaded_file, {
    req(page_started())
    req(input$uploaded_file)
    
    df <- tryCatch(
      read_uploaded_data(input$uploaded_file),
      error = function(e) {
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = 6
      )
      return(NULL)
    })
    if (is.null(df)) return(NULL)
    datavalues$uploaded_data <- df
  })
  
  observeEvent(input$design_title, {
    req(page_started())
    req(input$design_title)
    if (input$design_title == "General Design") {
      
      output$file_feedback <- renderUI({
        if (is.null(input$uploaded_file)) {
          return(NULL)
        }
        req(input$uploaded_file)
        
        df <- datavalues$uploaded_data
        
        if (is.null(df)) {
          tags$div(style = "color: red; font-weight: bold;margin-bottom: 10px;",
                   "The file could not be read. Check that it is not empty and uses a supported format, then choose it again.")
        } else {
          tags$div(style = "color: green; font-weight: bold;margin-bottom: 10px;",
                   paste("File ready:", nrow(df), "rows and", ncol(df), "columns detected."))
        }
      })
    }
  })
  
  output$file_type_check<-renderUI({
    req(page_started())
    req(input$uploaded_file)
    
    cols <- colnames(datavalues$uploaded_data)
    
    type_inputs <- lapply(seq_along(cols), function(i) {
      col_name <- cols[i]
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        
        selectInput(
          inputId = paste0("factor_type_", i),
          label = col_name,
          choices = c("Categorical factor" = "Categorical", "Continuous covariate" = "Continuous"),
          selected = "Categorical",
          width = "160px"
        )
      )
      
    })
    
    hidden_text_input <- tags$div(
      style = "display: none;",
      textInput(
        inputId = "factor_types",
        label = NULL,
        value = paste(rep("Categorical", length(cols)), collapse = ",")
      )
    )
    
    observeEvent(
      lapply(seq_along(cols), function(i) input[[paste0("factor_type_", i)]]),
      {
        type_values <- sapply(seq_along(cols), function(i) {
          input[[paste0("factor_type_", i)]] %||% "Categorical"
        })
        updateTextInput(
          session,
          "factor_types",
          value = paste(type_values, collapse = ",")
        )
      },
      ignoreNULL = FALSE
    )
    
    tagList(
      tags$label("Role of each data column",
                 style = "margin-bottom: 10px; display: block;font-weight: bold"),
      field_note("Categorical columns define groups or levels; continuous columns contain numeric covariates such as time or dose."),
      div(
        style = "font-size: 12px;display: flex; gap: 10px; overflow-x: auto; align-items: flex-end;",
        type_inputs
      ),
      hidden_text_input
    )
    
  })
  
  factor_types_number <- reactive({
    req(page_started())
    req(input$factor_types)
    
    types <- unlist(strsplit(input$factor_types, ","))
    types <- trimws(types)
    
    return(types)
  })
  
  observeEvent(input$design_title, {
    req(page_started())
    req(input$design_title)
    resetInput <- function(id) {
      if (id %in% names(input)) {
        tryCatch({
          updateTextInput(session, id, value = "")
        }, error = function(e) {
          tryCatch({
            updateSelectInput(session, id, selected = character(0))
          }, error = function(e2) {})
        })
      }
    }
    
    resetInput("which_para")
    resetInput("by_para")
    resetInput("Contrast")
  })
  
  level_nums <- reactive({
    req(page_started())
    if (is.null(input$uploaded_file)|is.null(input$which_para)){
      return(NULL)
    }else{
      req(input$uploaded_file)
      df <- datavalues$uploaded_data
      factors <- unlist(strsplit(input$which_para, "\\:"))
      factors <- trimws(factors)
      
      missing_cols <- setdiff(factors, colnames(df))
      
      output$which_validation <- renderUI({
        if (length(missing_cols) > 0) {
          div(
            style = "color: #d9534f; margin-top: 10px;",
            paste("Use column names from the uploaded file. Not found:",
                  paste(missing_cols, collapse = ", "))
          )
        } else {
          NULL
        }
      })
      
      if (length(missing_cols) > 0) {
        return(NULL)
      }
      level_counts <- as.numeric(sapply(factors, function(fac) {
        length(unique(df[[fac]]))
      }))
      
      level_counts
    }
  })  
  
  values<-reactiveValues(
    data = NULL,
    variance=NULL)
  
  datavalues<-reactiveValues(
    uploaded_data=NULL
  )
  
  observeEvent(input$design_title, {
    req(page_started())
    req(input$design_title)
    if (input$design_title == "General Design") {
      values$data <- NULL
      values$variance <- NULL
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$design_title, {
    req(page_started())
    req(input$design_title)
    if (input$design_title != "General Design") {
      datavalues$uploaded_data <- NULL
    }
  })
  
  output$interaction_exist_ui <- renderUI({
    req(page_started())
    if(!input$design_title%in%c('Split Plot Design','General Design')){
      req(!is.null(input$num_trt))
      count_result <- factor_count_result(input$num_trt)
      if (is.null(count_result$value)) {
        return(factor_count_warning(count_result$message))
      }
      if (count_result$value <= 1L) {
        return(NULL)
      }else {
        div(
          style = "flex: 1; padding-top: 2px;width:100%;",
          div(
            style='flex:1;',
            selectInput(
              inputId = 'interaction_option',
              label = 'Include interaction effects?',
              choices = c('Include selected interactions' = 'Yes', 'Main effects only' = 'No'),
              selected = 'No',
              width = "100%")
          )
        )
      }
    }else if(input$design_title=='Split Plot Design'){
      req(!is.null(input$num_trt_main), !is.null(input$num_trt_sub))
      main_result <- factor_count_result(input$num_trt_main)
      sub_result <- factor_count_result(input$num_trt_sub)
      if (is.null(main_result$value)) {
        return(factor_count_warning(main_result$message))
      }
      if (is.null(sub_result$value)) {
        return(factor_count_warning(sub_result$message))
      }
      if (main_result$value + sub_result$value > MAX_TREATMENT_FACTORS) {
        return(factor_count_warning(sprintf(
          "Main-plot and sub-plot factors combined cannot exceed %d.",
          MAX_TREATMENT_FACTORS
        )))
      }
      
      div(
        style = "flex: 1; padding-top: 2px;width:100%;",
        div(
          style='flex:1;',
          selectInput(
            inputId = 'interaction_option',
            label = 'Include interaction effects?',
            choices = c('Include selected interactions' = 'Yes', 'Main effects only' = 'No'),
            selected = 'No',
            width = "100%")
        )
      )
    }
  })
  
  output$interaction_fac_ui <- renderUI({
    req(page_started())
    req(input$interaction_option)
    if (input$interaction_option == "No") {
      return(NULL)
    }
    if(!input$design_title%in%c('Split Plot Design','General Design')){
      factor_count <- levels_num_trt()
      if(factor_count>1){
        fac_names <- paste0("fac", LETTERS[seq_len(factor_count)])
        all_combinations <- unlist(
          lapply(2:factor_count, function(k) {
            combn(fac_names, k, FUN = function(x) paste(x, collapse = " : "))
          })
        )
        tagList(
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectizeInput(
                inputId = "interaction_formula",
                label = "Interactions to include",
                choices = all_combinations,
                multiple = TRUE,
                selected = NULL,
                options = list(placeholder = 'Choose one or more factor combinations'),
                width = "100%"
              )
            )
          ),
          uiOutput("interaction_formula_hint")
        )
      }else{
        NULL
      }
    }else if(input$design_title=='Split Plot Design'){
      num_trt_main <- levels_num_trt_main()
      num_trt_sub <- levels_num_trt_sub()
      req(num_trt_main + num_trt_sub <= MAX_TREATMENT_FACTORS)
      
      if(num_trt_main==1){
        fac_names_main<-'trt.main'
      }else if(num_trt_main>1){
        fac_names_main <- paste0("fac", LETTERS[seq_len(num_trt_main)],'.main')
      }
      
      if(num_trt_sub==1){
        fac_names_sub<-'trt.sub'
      }else if(num_trt_sub>1){
        fac_names_sub <- paste0("fac", LETTERS[seq_len(num_trt_sub)],'.sub')
      }
      
      fac_names<-c(fac_names_main,fac_names_sub)
      
      all_combinations <- unlist(
        lapply(2:length(fac_names), function(k) {
          combn(fac_names, k, FUN = function(x) paste(x, collapse = " : "))
        })
      )
      tagList(
        div(
          style = "flex: 1; padding-top: 2px;width:100%;",
          div(
            style='flex:1;',
            selectizeInput(
              inputId = "interaction_formula",
              label = "Interactions to include",
              choices = all_combinations,
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = 'Choose one or more factor combinations'),
              width = "100%"
            )
          )
        ),
        uiOutput("interaction_formula_hint")
      )
    }
  })
  
  observeEvent({
    list(
      tryCatch(input$design_title,error=function(e) NULL),
      tryCatch(levels_vec(), error = function(e) NULL),
      tryCatch(levels_num_trt(), error = function(e) NULL),
      tryCatch(input$num_rep, error = function(e) NULL),
      tryCatch(interaction_option_number(), error = function(e) NULL),
      tryCatch(interaction_formula_number(), error = function(e) NULL),
      
      tryCatch(levels_num_trt_main(), error = function(e) NULL),
      tryCatch(levels_num_trt_sub(), error = function(e) NULL),
      tryCatch(levels_vec_main(), error = function(e) NULL),
      tryCatch(levels_vec_sub(), error = function(e) NULL),
      tryCatch(input$Formula_general,error=function(e) NULL),
      tryCatch(factor_types_number(),error=function(e) NULL),
      tryCatch(datavalues$uploaded_data,error=function(e) NULL),
      tryCatch(input$cor_type,error=function(e) NULL)
    )
  }, {
    
    output$model_ui<-renderUI({
      req(page_started())
      
      fac_formula <- NULL
      if(!input$design_title %in% c('Split Plot Design','General Design')){
        req(input$num_trt)
        if(levels_num_trt() == 1){
          fac_formula <- 'trt'
        } else if(levels_num_trt() > 1){
          req(input$interaction_option)
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          if(input$interaction_option == 'No'){
            fac_formula <- paste(fac_names, collapse = " + ")
          } else if(input$interaction_option == 'Yes'){
            if(is.null(input$interaction_formula)){
              fac_formula <- paste(fac_names, collapse = " + ")
            } else {
              interaction_terms <- input$interaction_formula
              interaction_terms <- unique(interaction_terms)
              interaction_terms <- interaction_terms[order(interaction_terms)]
              fac_formula <- paste(c(fac_names, interaction_terms), collapse = " + ")
            }
          }
        }
        if(input$design_title=='Randomized Complete Block Design'){
          fac_formula<-paste0(fac_formula,' + ( 1 | block )')
        }else if(input$design_title=='Latin Square Design'){
          fac_formula<-paste0(fac_formula,' + ( 1 | row ) + ( 1 | col )')
        }
      } else if(input$design_title == 'Split Plot Design'){
        req(input$num_trt_main)
        req(input$num_trt_sub)
        fac_names_main <- if(levels_num_trt_main() == 1) 'trt.main' else paste0("fac", LETTERS[1:levels_num_trt_main()], '.main')
        fac_names_sub <- if(levels_num_trt_sub() == 1) 'trt.sub' else paste0("fac", LETTERS[1:levels_num_trt_sub()], '.sub')
        fac_names <- c(fac_names_main, fac_names_sub)
        
        req(input$interaction_option)
        if(input$interaction_option == 'No'){
          fac_formula <- paste(fac_names, collapse = " + ")
        } else if(input$interaction_option == 'Yes'){
          if(is.null(input$interaction_formula)){
            fac_formula <- paste(fac_names, collapse = " + ")
          } else {
            interaction_terms <- input$interaction_formula
            interaction_terms <- unique(interaction_terms)
            interaction_terms <- interaction_terms[order(interaction_terms)]
            fac_formula <- paste(c(fac_names, interaction_terms), collapse = " + ")
          }
        }
      }
      
      note_text <- 
        if(input$design_title=='Completely Randomized Design'){
          paste0(
            '<strong>Generated model</strong>',
            'Fixed effects: ~ ', fac_formula, '<br>',
            'Variance component: residual error (&sigma;<sup>2</sup><sub>e</sub>)'
          )
        } else if(input$design_title=='Randomized Complete Block Design'){
          paste0(
            '<strong>Generated model</strong>',
            'Fixed and random effects: ~ ', fac_formula, '<br>',
            'Variance components: block and residual error'
          )
        } else if(input$design_title=='Latin Square Design'){
          paste0(
            '<strong>Generated model</strong>',
            'Fixed and random effects: ~ ', fac_formula, '<br>',
            'Variance components: row, column, and residual error'
          )
        } else if(input$design_title=='Split Plot Design'){
          paste0(
            '<strong>Generated model</strong>',
            'Fixed and random effects: ~ ', fac_formula, ' + (1 | mainplot)', '<br>',
            'Variance components: whole plot and residual error'
          )
        } 
      div(
        class = "model-preview",
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          div(
            HTML(paste0(
              '<p style="margin: 0;">',
              note_text,
              '</p>'
            )),
            style = "flex: 1;"
          )
        )
      )
    })
  })
  
  output$interaction_formula_hint <- renderUI({
    req(page_started())
    req(input$interaction_option)
    
    if (input$interaction_option == "Yes") {
      if (is.null(input$interaction_formula) || length(input$interaction_formula) == 0) {
        tags$p(
          "Choose at least one interaction, or return to main effects only.",
          style = "color: red; font-weight: 500; margin-top: 10px;margin_bottom:10px;"
        )
      }
    }
  })
  
  levels_vec <- reactive({
    req(page_started())
    req(input$level_numbers)
    nums <- trimws(unlist(strsplit(input$level_numbers, ",")))
    as.numeric(nums[nums != ""])
  })
  
  levels_vec_main <- reactive({
    req(page_started())
    req(input$level_numbers_main)
    nums <- trimws(unlist(strsplit(input$level_numbers_main, ",")))
    as.numeric(nums[nums != ""])
  })
  
  levels_vec_sub <- reactive({
    req(page_started())
    req(input$level_numbers_sub)
    nums <- trimws(unlist(strsplit(input$level_numbers_sub, ",")))
    as.numeric(nums[nums != ""])
  })
  
  levels_num_trt<-reactive({
    req(page_started())
    req(!is.null(input$num_trt))
    count_result <- factor_count_result(input$num_trt)
    req(!is.null(count_result$value))
    count_result$value
  })
  
  levels_num_trt_main<-reactive({
    req(page_started())
    req(!is.null(input$num_trt_main))
    count_result <- factor_count_result(input$num_trt_main)
    req(!is.null(count_result$value))
    count_result$value
  })
  
  levels_num_trt_sub<-reactive({
    req(page_started())
    req(!is.null(input$num_trt_sub))
    count_result <- factor_count_result(input$num_trt_sub)
    req(!is.null(count_result$value))
    count_result$value
  })
  
  interaction_formula_number<-reactive({
    req(page_started())
    req(input$interaction_formula)
    input$interaction_formula
  })
  
  interaction_option_number<-reactive({
    req(page_started())
    req(input$interaction_option)
    input$interaction_option
  })
  
  observeEvent({list(
    design_revision(),
    tryCatch(levels_vec(), error = function(e) NULL),
    tryCatch(levels_vec_main(), error = function(e) NULL),
    tryCatch(levels_vec_sub(), error = function(e) NULL),
    tryCatch(interaction_formula_number(), error = function(e) NULL),
    tryCatch(interaction_option_number(), error = function(e) NULL),
    tryCatch(datavalues$uploaded_data, error = function(e) NULL),
    tryCatch(input$Formula_general, error = function(e) NULL),
    tryCatch(factor_types_number(), error = function(e) NULL)
  )
  },{
    req(page_started())
    req(input$design_title)
    if (input$design_title %in% c('Completely Randomized Design','Randomized Complete Block Design','Latin Square Design')){
      req(input$level_numbers)
    }
    if (input$design_title == 'Split Plot Design') {
      req(input$level_numbers_main, input$level_numbers_sub)
    }
    if (input$design_title == 'General Design') {
      req(input$uploaded_file)
    }
    
    if(input$design_title=="Completely Randomized Design"){
      req(input$num_trt, input$num_rep)
      if(min(levels_vec())==1){
        values$data<-NULL
        values$variance<-NULL
      }else{
        if(levels_num_trt()==1&min(levels_vec())>1){
          crd<-build_crd_design(
            treatments = levels_vec(),
            replicates = input$num_rep,
            template = T
          )
        }else if(levels_num_trt()>1&interaction_option_number()=='No'){
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd<-build_crd_design(
            treatments = levels_vec(),
            replicates = input$num_rep,
            formula= as.formula(paste0('~',fac_formula)),
            template = T
          ) 
        }else if(levels_num_trt()>1&interaction_option_number()=='Yes'){
          
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          
          if(is.null(interaction_formula_number())){
            fac_formula <- paste(fac_names, collapse = "+")
          }else{
            interaction_terms <- interaction_formula_number()
            interaction_terms<-unique(interaction_terms)
            interaction_terms<-interaction_terms[order(interaction_terms)]
            
            fac_formula <- paste(c(fac_names, interaction_terms), collapse = "+")
          }
          
          crd<-build_crd_design(
            treatments = levels_vec(),
            replicates = input$num_rep,
            formula= as.formula(paste0('~',fac_formula)),
            template = T
          ) 
        }
        df<-data.frame(Mean = crd$fixeff$means)
        df[]<-' '
        values$data<-as.matrix(df)
        df2<-data.frame(Variance=1)
        df2[]<-' '
        row.names(df2)<-'Error'
        values$variance<-as.matrix(df2)
      }
    }else if(input$design_title=="Randomized Complete Block Design"){
      req(input$num_trt, input$num_block)
      if(min(levels_vec())==1){
        values$data<-NULL
        values$variance<-NULL
      }else{
        if(levels_num_trt()==1&min(levels_vec())>1){
          crd<-build_rcbd_design(
            treatments = levels_vec(),
            blocks = input$num_block,
            template = T
          )
        }else if(levels_num_trt()>1&interaction_option_number()=='No'){
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd<-build_rcbd_design(
            treatments = levels_vec(),
            blocks = input$num_block,
            formula= as.formula(paste0('~',fac_formula,'+(1|block)')),
            template = T
          ) 
        }else if(levels_num_trt()>1&interaction_option_number()=='Yes'){
          
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          
          if(is.null(interaction_formula_number())){
            fac_formula <- paste(fac_names, collapse = "+")
          }else{
            interaction_terms <- interaction_formula_number()
            interaction_terms<-unique(interaction_terms)
            interaction_terms<-interaction_terms[order(interaction_terms)]
            
            fac_formula <- paste(c(fac_names, interaction_terms), collapse = "+")
          }
          
          crd<-build_rcbd_design(
            treatments = levels_vec(),
            blocks = input$num_block,
            formula= as.formula(paste0('~',fac_formula,'+(1|block)')),
            template = T
          ) 
        }
        df<-data.frame(Mean = crd$fixeff$means)
        df[]<-' '
        values$data<-as.matrix(df)
        df2<-data.frame(Variance=c(1,1))
        df2[]<-' '
        row.names(df2)<-c('Block','Error')
        values$variance<-as.matrix(df2)
      }
    }else if(input$design_title=="Latin Square Design"){
      req(input$num_trt, input$num_squares, input$value_reuse)
      if(min(levels_vec())==1){
        values$data<-NULL
        values$variance<-NULL
      }else{
        if(levels_num_trt()==1&min(levels_vec())>1){
          crd<-build_latin_square_design(
            treatments = levels_vec(),
            squares = input$num_squares,
            reuse=input$value_reuse,
            template = T
          )
        }else if(levels_num_trt()>1&interaction_option_number()=='No'){
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd<-build_latin_square_design(
            treatments = levels_vec(),
            squares = input$num_squares,
            reuse=input$value_reuse,
            formula= as.formula(paste0('~',fac_formula,'+(1|row)+(1|col)')),
            template = T
          ) 
        }else if(levels_num_trt()>1&interaction_option_number()=='Yes'){
          
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          
          if(is.null(interaction_formula_number())){
            fac_formula <- paste(fac_names, collapse = "+")
          }else{
            interaction_terms <- interaction_formula_number()
            interaction_terms<-unique(interaction_terms)
            interaction_terms<-interaction_terms[order(interaction_terms)]
            
            fac_formula <- paste(c(fac_names, interaction_terms), collapse = "+")
          }
          
          crd<-build_latin_square_design(
            treatments = levels_vec(),
            squares = input$num_squares,
            reuse=input$value_reuse,
            formula= as.formula(paste0('~',fac_formula,'+(1|row)+(1|col)')),
            template = T
          ) 
        }
        df<-data.frame(Mean = crd$fixeff$means)
        df[]<-' '
        values$data<-as.matrix(df)
        df2<-data.frame(Variance=c(1,1,1))
        df2[]<-' '
        row.names(df2)<-c('Row','Col','Error')
        values$variance<-as.matrix(df2)
      }
    }else if(input$design_title=="Split Plot Design"){
      req(input$num_trt_main)
      req(input$num_trt_sub)
      req(input$num_rep)
      if(min(levels_vec_main())==1||min(levels_vec_sub())==1){
        values$data<-NULL
        values$variance<-NULL
      }else{
        if(levels_num_trt_main()==1){
          fac_names_main<-'trt.main'
        }else if(levels_num_trt_main()>1){
          fac_names_main <- paste0("fac", LETTERS[1:levels_num_trt_main()],'.main')
        }
        
        if(levels_num_trt_sub()==1){
          fac_names_sub<-'trt.sub'
        }else if(levels_num_trt_sub()>1){
          fac_names_sub <- paste0("fac", LETTERS[1:levels_num_trt_sub()],'.sub')
        }
        
        fac_names<-c(fac_names_main,fac_names_sub)
        
        if(interaction_option_number()=='No'){
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd<-build_split_plot_design(
            trt.main = levels_vec_main(),
            trt.sub = levels_vec_sub(),
            replicates = input$num_rep,
            formula= as.formula(paste0('~',fac_formula,'+(1|mainplot)')),
            template = T
          )
          
        }else if(interaction_option_number()=='Yes'){
          if(is.null(interaction_formula_number())){
            fac_formula <- paste(fac_names, collapse = "+")
          }else{
            interaction_terms <- interaction_formula_number()
            interaction_terms<-unique(interaction_terms)
            interaction_terms<-interaction_terms[order(interaction_terms)]
            
            fac_formula <- paste(c(fac_names, interaction_terms), collapse = "+")
          }
          
          crd<-build_split_plot_design(
            trt.main = levels_vec_main(),
            trt.sub = levels_vec_sub(),
            replicates = input$num_rep,
            formula= as.formula(paste0('~',fac_formula,'+(1|mainplot)')),
            template = T
          )
        }
        df<-data.frame(Mean = crd$fixeff$means)
        df[]<-' '
        values$data<-as.matrix(df)
        df2<-data.frame(Variance=c(1,1))
        df2[]<-' '
        row.names(df2)<-c('Mainplot','Error')
        values$variance<-as.matrix(df2)
      }
    }else if(input$design_title=="General Design"){
      req(datavalues$uploaded_data)
      #req(factor_types_number())
      tryCatch({
        df<-datavalues$uploaded_data
        cols <- colnames(df)
        types <- factor_types_number()
        
        stopifnot(length(cols) == length(types))
        
        for (i in seq_along(cols)) {
          if (types[i] == "Categorical") {
            df[[cols[i]]] <- as.factor(df[[cols[i]]])
          } else if (types[i] == "Continuous") {
            df[[cols[i]]] <- as.numeric(df[[cols[i]]])
          }
        }
        
        crd<-build_general_design(
          formula = as.formula(input$Formula_general),
          data=df,
          template = T
        )
        
        df<-data.frame(Mean = crd$fixeff$means)
        df[]<-' '
        values$data<-as.matrix(df)
        n_variance_table<-length(crd$varcov)
        if(n_variance_table==0){
          df2<-data.frame(crd$varcov)
          df2<-rbind(df2,NA)
          colnames(df2)<-'Variance'
          row.names(df2)<-c(names(crd$varcov),'Error')
          df2[]<-' '
          df2[1,1]<-'Error'
          values$variance[[1]]<-as.matrix(df2)
        }else if(n_variance_table>0){
          for(n_number in 1:n_variance_table){
            df2<-data.frame(crd$varcov[n_number])
            colnames(df2)<-row.names(df2)
            df2[]<-' '
            df2[1,1]<-names(crd$varcov[n_number])
            values$variance[[n_number]] <- as.matrix(df2)
          }
          df2<-data.frame(Error=NA)
          row.names(df2)<-colnames(df2)
          df2[1,1]<-'Error'
          values$variance[[n_variance_table+1]]<-as.matrix(df2)
        }
      }, error = function(e){
        values$data <- NULL
        values$variance <- NULL
      })
    }
  }, ignoreNULL = FALSE)
  
  observeEvent({
    list(
      tryCatch(input$design_title,error=function(e) NULL),
      tryCatch(levels_vec(), error = function(e) NULL),
      tryCatch(levels_num_trt(), error = function(e) NULL),
      tryCatch(input$num_rep, error = function(e) NULL),
      tryCatch(interaction_option_number(), error = function(e) NULL),
      tryCatch(interaction_formula_number(), error = function(e) NULL),
      
      tryCatch(levels_num_trt_main(), error = function(e) NULL),
      tryCatch(levels_num_trt_sub(), error = function(e) NULL),
      tryCatch(levels_vec_main(), error = function(e) NULL),
      tryCatch(levels_vec_sub(), error = function(e) NULL),
      tryCatch(input$Formula_general,error=function(e) NULL),
      tryCatch(factor_types_number(),error=function(e) NULL),
      tryCatch(datavalues$uploaded_data,error=function(e) NULL),
      tryCatch(input$cor_type,error=function(e) NULL)
    )
  }, {
    req(page_started())
    output$input_data_ui <- renderUI({
      if(!input$design_title%in%c('Split Plot Design','General Design')){
        factor_count <- levels_num_trt()
        note_text <- if (factor_count>=2) {
          req(input$interaction_option)
          if(input$interaction_option=='Yes'){
            "Enter an expected mean response for every treatment combination."
          } else if (input$interaction_option == "No") {
            "Enter the expected marginal mean at each level of every factor."
          } 
        } else if(factor_count<=1) {
          "Enter the expected mean response for each treatment level."
        }
        
      }else if(input$design_title=='Split Plot Design'){
        num_trt_main <- levels_num_trt_main()
        num_trt_sub <- levels_num_trt_sub()
        req(num_trt_main + num_trt_sub <= MAX_TREATMENT_FACTORS)
        req(input$interaction_option)
        
        note_text <- if(input$interaction_option=='Yes'){
          "Enter an expected mean response for every whole-plot and subplot treatment combination."
        } else if (input$interaction_option == "No") {
          "Enter the expected marginal mean at each level of every factor."
        } 
      }else if(input$design_title=='General Design'){
        req(datavalues$uploaded_data)
        note_text<-NULL
      }
      
      if(input$design_title!='General Design'){
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
                border-radius: 8px; padding: 10px; margin-top: 8px;
                height: 300px; overflow-y: auto;",
          section_header("table", "Expected responses", "Use values on the same scale as the outcome you plan to analyze."),
          tags$p(note_text, class = "field-note"),
          uiOutput('design_table_ui')
        )
      }else if(input$design_title=='General Design'){
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
                border-radius: 8px; padding: 10px; margin-top: 8px;
                height: 300px; overflow-y: auto;",
          section_header("table", "Expected responses", "Enter the expected response for every model row generated from the uploaded design."),
          tags$p("Use estimates from prior studies, pilot data, or the smallest effects that would be scientifically meaningful.", class = "field-note"),
          uiOutput('design_table_ui')
        )
      }
    })
    
    output$design_table_ui <- renderUI({
      if(!input$design_title%in%c('Split Plot Design','General Design')){
        req(input$level_numbers)
        req(input$num_trt)
      }else if(input$design_title=='Split Plot Design'){
        req(input$level_numbers_main)
        req(input$num_trt_main)
        req(input$level_numbers_sub)
        req(input$num_trt_sub)
      }else if(input$design_title=='General Design'){
        req(datavalues$uploaded_data)
      }
      if (is.null(values$data)) {
        return(
          div(
            style = "height: 180px; border: 1px solid #ddd; background-color: #f9f9f9; 
                 display: flex; align-items: center; justify-content: center;",
            tags$em("Complete the treatment and model settings above to create the expected-response table.")
          )
        )
      }else{
        rhandsontable::rHandsontableOutput("design_table")
      }
    })
    
    output$design_table <- rhandsontable::renderRHandsontable({
      req(values$data)
      
      df <- as.data.frame(values$data)
      
      col_widths <- pmax(160, nchar(colnames(df))) 
      row_name_width <- max(nchar(rownames(df))) * 10+5
      
      rhandsontable::rhandsontable(
        df,
        rowHeaders = row.names(df),
        colHeaders = colnames(df),
        stretchH = "all",
        height = 180,
        colWidths = col_widths,
        rowHeaderWidth = row_name_width,
        digits = 2
      ) %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        rhandsontable::hot_cols(
          renderer = "function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        td.style.textAlign = 'center';
        if (value === null || value === undefined || value === '') {
          td.style.background = '#FFF9E6';
        }
      }"
        )
    })
  })
  
  observeEvent({
    list(
      tryCatch(input$design_title,error=function(e) NULL),
      tryCatch(levels_vec(), error = function(e) NULL),
      tryCatch(levels_num_trt(), error = function(e) NULL),
      tryCatch(input$num_rep, error = function(e) NULL),
      tryCatch(interaction_option_number(), error = function(e) NULL),
      tryCatch(interaction_formula_number(), error = function(e) NULL),
      
      tryCatch(levels_num_trt_main(), error = function(e) NULL),
      tryCatch(levels_num_trt_sub(), error = function(e) NULL),
      tryCatch(levels_vec_main(), error = function(e) NULL),
      tryCatch(levels_vec_sub(), error = function(e) NULL),
      tryCatch(input$Formula_general,error=function(e) NULL),
      tryCatch(factor_types_number(),error=function(e) NULL),
      tryCatch(datavalues$uploaded_data,error=function(e) NULL),
      tryCatch(input$cor_type,error=function(e) NULL)
    )
  }, {
    req(page_started())
    output$input_variance_ui <- renderUI({
      
      if(!input$design_title%in%c('Split Plot Design','General Design')){
        req(input$num_trt)
        
      }else if(input$design_title=='Split Plot Design'){
        req(input$num_trt_main)
        req(input$num_trt_sub)
        
      }else if(input$design_title=='General Design'){
        req(datavalues$uploaded_data)
      }
      
      bslib::card(
        style = "background-color: #f8f9fa; border: 1px solid #ddd; 
                  border-radius: 8px; padding: 10px; margin-top: 8px;
                  height: 240px; display: flex; flex-direction: column;",
        section_header("wave-square", "Variation assumptions", "Enter variance estimates for each random effect and for residual error. Variances must be zero or greater."),
        div(
          style = "flex: 0 0 auto; overflow-y: auto;",
          uiOutput('design_variance_table_ui')
        )
      )
    })
    
    output$design_variance_table_ui <- renderUI({
      if(!input$design_title%in%c('Split Plot Design','General Design')){
        req(input$level_numbers)
        req(input$num_trt)
      }else if(input$design_title=='Split Plot Design'){
        req(input$level_numbers_main)
        req(input$num_trt_main)
        req(input$level_numbers_sub)
        req(input$num_trt_sub)
      }else if(input$design_title=='General Design'){
        req(datavalues$uploaded_data)
      }
      if (is.null(values$variance)) {
        return(
          div(
            style = "height: 100px; border: 1px solid #ddd; background-color: #f9f9f9; 
                 display: flex; align-items: center; justify-content: center;",
            tags$em("Complete the treatment and model settings above to create the variance table.")
          )
        )
      }else{
        if(input$design_title!="General Design"){
          rhandsontable::rHandsontableOutput("design_variance_table")
        }else if(input$design_title=="General Design"){
          if (!is.list(values$variance)) {
            return(tags$em("Complete the uploaded-data model settings to create the variance tables."))
          }
          variance_list <- values$variance
          
          tables_ui <- lapply(seq_along(variance_list), function(i) {
            tags$div(
              style = "margin-bottom: 15px;",
              tags$h4(style = "font-weight: bold; margin-bottom: 8px;",
                      variance_list[[i]][1, 1]),
              rHandsontableOutput(paste0("design_variance_table_", i))
            )
          })
          
          do.call(tagList, tables_ui)
        }
      }
    })
    
    output$design_variance_table <- rhandsontable::renderRHandsontable({
      req(values$variance)
      req(input$design_title != "General Design")
      
      df <- as.data.frame(values$variance)
      
      col_widths <- pmax(140, nchar(colnames(df))) 
      row_name_width <- max(nchar(rownames(df))) * 10
      
      rhandsontable::rhandsontable(
        df,
        rowHeaders = row.names(df),
        colHeaders = colnames(df),
        stretchH = "all",
        height = 100,
        colWidths = col_widths,
        rowHeaderWidth = row_name_width,
        digits = 2
      ) %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        rhandsontable::hot_cols(
          renderer = "function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        td.style.textAlign = 'center';
        if (value === null || value === undefined || value === '') {
          td.style.background = '#FFF9E6';
        }
      }"
        )
    })
  })
  
  observe({
    req(page_started())
    req(input$design_title == "General Design")
    req(!is.null(values$variance))
    req(is.list(values$variance))
    
    variance_list <- values$variance
    
    for (i in seq_along(variance_list)) {
      
      local({
        j <- i
        mat <- variance_list[[j]]
        
        mat_display <- mat
        mat_display[,] <- " "
        
        n_row <- nrow(mat_display)
        n_col <- ncol(mat_display)
        
        for (r in 1:n_row) {
          for (c in 1:n_col) {
            if (r < c) {
              mat_display[r, c] <- "--"
            }
          }
        }
        
        df <- as.data.frame(mat_display)
        
        col_widths <- pmax(140, nchar(colnames(df)))
        row_name_width <- max(nchar(rownames(df))) * 10
        table_height <- 30 + n_row * 30
        
        output[[paste0("design_variance_table_", j)]] <- renderRHandsontable({
          rh<-rhandsontable(
            df,
            rowHeaders = row.names(df),
            colHeaders = colnames(df),
            stretchH = "all",
            height = table_height,
            colWidths = col_widths,
            rowHeaderWidth = row_name_width,
            digits = 2
          )%>%
            rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            rhandsontable::hot_cols(
              renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.NumericRenderer.apply(this, arguments);
                      td.style.textAlign = 'center';
                      if (value === null || value === undefined || value === '') {
                      td.style.background = '#FFF9E6';
                     }
                   }"
            )
          for (r in 1:n_row) {
            for (c in 1:n_col) {
              if (r < c) {
                rh <- rh %>% hot_cell(row = r, col = c, readOnly = TRUE)
              }
            }
          }
          rh
        })
      })
    }
  })
  
  # ── Correlation UI helpers ──────────────────────────────────────────────────
  
  # Dynamic parameter inputs that depend on the selected correlation type
  output$cor_params_ui <- renderUI({
    req(page_started())
    req(input$cor_type)
    
    if (input$cor_type == "none") return(NULL)
    
    req(datavalues$uploaded_data)
    cols <- colnames(datavalues$uploaded_data)
    
    var_sel <- function(id, lbl, extra_none = FALSE) {
      ch <- if (extra_none) c("None" = "", cols) else cols
      sel <- if (extra_none) "" else cols[1]
      selectInput(id, lbl, choices = ch, selected = sel, width = "100%")
    }
    
    group_sel <- var_sel("cor_group", "Group observations by (optional)", extra_none = TRUE)
    
    rho_slider <- sliderInput("cor_rho", "Adjacent-observation correlation (\u03c1)",
                              min = -0.99, max = 0.99, value = 0.3, step = 0.01, width = "100%")
    
    if (input$cor_type %in% c("corAR1", "corCAR1", "corCompSymm")) {
      return(tagList(
        var_sel("cor_time", "Time or sequence column"),
        group_sel,
        rho_slider
      ))
    }
    
    if (input$cor_type == "corARMA") {
      return(tagList(
        var_sel("cor_time", "Time or sequence column"),
        group_sel,
        numericInput("cor_p", "Autoregressive order (p)", value = 1, min = 0, step = 1, width = "100%"),
        numericInput("cor_q", "Moving-average order (q)", value = 0, min = 0, step = 1, width = "100%"),
        uiOutput("cor_arma_params_ui")
      ))
    }
    
    if (input$cor_type %in% c("corExp", "corGaus", "corLin", "corRatio", "corSpher")) {
      return(tagList(
        selectInput("cor_dim", "Number of spatial coordinates",
                    choices = c("1D: ~ x" = "1", "2D: ~ x + y" = "2", "3D: ~ x + y + z" = "3"),
                    selected = "1", width = "100%"),
        var_sel("cor_x", "X-coordinate column"),
        conditionalPanel("input.cor_dim >= '2'", var_sel("cor_y", "Y-coordinate column")),
        conditionalPanel("input.cor_dim == '3'", var_sel("cor_z", "Z-coordinate column")),
        group_sel,
        numericInput("cor_range", "Correlation range", value = 1, min = 1e-6, step = 0.1, width = "100%"),
        checkboxInput("cor_nugget", "Include a nugget effect", value = FALSE)
      ))
    }
    
    if (input$cor_type == "corSymm") {
      return(tagList(
        var_sel("cor_time", "Index column defining repeated levels"),
        group_sel,
        tags$div(
          style = "margin-top:6px; font-size:13px; color:#555;",
          "Enter the pairwise correlations below the diagonal. Every value must be greater than -1 and less than 1."
        ),
        uiOutput("cor_symm_table_ui")
      ))
    }
    
    NULL
  })
  
  # ARMA parameter vector inputs (general p + q)
  output$cor_arma_params_ui <- renderUI({
    req(page_started(), input$cor_type == "corARMA")
    req(input$cor_p, input$cor_q)
    
    p <- as.integer(input$cor_p)
    q <- as.integer(input$cor_q)
    
    if (is.na(p) || is.na(q) || p < 0 || q < 0)
      return(tags$div(style = "color:#d9534f;", "Enter whole numbers of zero or greater for both p and q."))
    
    k <- p + q
    if (k == 0)
      return(tags$div(style = "margin-top:6px; color:#555;",
                      "Set p or q above zero to define an ARMA correlation structure."))
    
    inputs <- lapply(seq_len(k), function(i) {
      lab <- if (i <= p) paste0("AR parameter \u03c6", i) else paste0("MA parameter \u03b8", i - p)
      numericInput(paste0("cor_arma_", i), lab, value = 0.1, min = -0.99, max = 0.99, step = 0.01, width = "100%")
    })
    
    tagList(tags$div(style = "margin-top:6px; font-weight:600;", "ARMA parameters"), inputs)
  })
  
  # corSymm: lower-triangle table UI
  output$cor_symm_table_ui <- renderUI({
    req(page_started(), input$cor_type == "corSymm")
    req(datavalues$uploaded_data, input$cor_time)
    
    lev <- unique(datavalues$uploaded_data[[input$cor_time]])
    lev <- as.character(lev[!is.na(lev)])
    m   <- length(lev)
    
    if (m < 2)
      return(tags$div(style = "color:#d9534f; margin-top:6px;",
                      "Choose an index column with at least two distinct non-missing values."))
    if (m > 15)
      return(tags$div(style = "color:#d9534f; margin-top:6px;",
                      "This column has more than 15 levels, which is too many for manual unstructured correlations. Choose a simpler correlation structure."))
    
    rhandsontable::rHandsontableOutput("cor_symm_table")
  })
  
  output$cor_symm_table <- rhandsontable::renderRHandsontable({
    req(page_started(), input$cor_type == "corSymm")
    req(datavalues$uploaded_data, input$cor_time)
    
    lev <- unique(datavalues$uploaded_data[[input$cor_time]])
    lev <- as.character(lev[!is.na(lev)])
    m   <- length(lev)
    req(m >= 2, m <= 15)
    
    # Build lower-triangle numeric data frame; diagonal = 1, upper = NA (read-only)
    mat <- matrix(NA_real_, nrow = m, ncol = m)
    diag(mat) <- 1
    df_mat <- as.data.frame(mat)
    rownames(df_mat) <- lev
    colnames(df_mat) <- lev
    
    rh <- rhandsontable::rhandsontable(df_mat, rowHeaders = lev, width = "100%", height = 250) %>%
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      rhandsontable::hot_cols(
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.textAlign = 'center';
        }"
      )
    
    for (r in seq_len(m)) {
      for (c in seq_len(m)) {
        if (r <= c) {                           # diagonal + upper triangle: read-only
          rh <- rh %>% rhandsontable::hot_cell(row = r, col = c, readOnly = TRUE)
        }
      }
    }
    rh
  })
  
  # Validation feedback
  output$cor_validation_ui <- renderUI({
    req(page_started(), input$cor_type)
    if (input$cor_type == "none") return(NULL)
    req(datavalues$uploaded_data)
    
    cols <- colnames(datavalues$uploaded_data)
    msgs <- character(0)
    
    # Time / index variable check (for non-spatial types)
    if (input$cor_type %in% c("corAR1", "corARMA", "corCAR1", "corCompSymm", "corSymm")) {
      t <- tryCatch(input$cor_time, error = function(e) NULL)
      if (is.null(t) || !nzchar(t))
        msgs <- c(msgs, "Choose the column that defines time or observation order.")
      else if (!(t %in% cols))
        msgs <- c(msgs, paste0("The selected time or sequence column is not in the uploaded data: ", t, "."))
    }
    
    # Spatial coordinate checks
    if (input$cor_type %in% c("corExp", "corGaus", "corLin", "corRatio", "corSpher")) {
      x <- tryCatch(input$cor_x, error = function(e) NULL)
      if (is.null(x) || !(x %in% cols))
        msgs <- c(msgs, "Choose an X-coordinate column from the uploaded data.")
      dim_val <- tryCatch(input$cor_dim, error = function(e) "1")
      if (!is.null(dim_val) && dim_val >= "2") {
        y <- tryCatch(input$cor_y, error = function(e) NULL)
        if (is.null(y) || !(y %in% cols))
          msgs <- c(msgs, "Choose a Y-coordinate column from the uploaded data.")
      }
      if (!is.null(dim_val) && dim_val == "3") {
        z <- tryCatch(input$cor_z, error = function(e) NULL)
        if (is.null(z) || !(z %in% cols))
          msgs <- c(msgs, "Choose a Z-coordinate column from the uploaded data.")
      }
    }
    
    # Grouping variable (optional)
    grp <- tryCatch(input$cor_group, error = function(e) NULL)
    if (!is.null(grp) && nzchar(grp) && !(grp %in% cols))
      msgs <- c(msgs, paste0("The selected grouping column is not in the uploaded data: ", grp, "."))
    
    # ARMA: p+q check
    if (input$cor_type == "corARMA") {
      p <- tryCatch(as.integer(input$cor_p), error = function(e) NA_integer_)
      q <- tryCatch(as.integer(input$cor_q), error = function(e) NA_integer_)
      if (is.na(p) || is.na(q) || p < 0 || q < 0)
        msgs <- c(msgs, "Enter whole numbers of zero or greater for p and q.")
      else if (p == 0 && q == 0)
        msgs <- c(msgs, "Set at least one ARMA order (p or q) above zero.")
    }
    
    if (length(msgs) > 0)
      div(style = "color:#d9534f; margin-top:4px; font-size:13px;", paste(msgs, collapse = "  "))
    else
      div(style = "color:#28a745; margin-top:4px; font-size:13px;", "\u2713 Correlation settings are complete.")
  })
  
  # ── Build correlation object from safe whitelist ────────────────────────────
  build_correlation <- function(df, type) {
    if (is.null(type) || type == "none") return(NULL)

    params <- list(
      group = input$cor_group,
      time = input$cor_time,
      rho = input$cor_rho,
      p = input$cor_p,
      q = input$cor_q,
      range = input$cor_range,
      nugget = input$cor_nugget
    )

    if (type == "corARMA") {
      parameter_count <- as.integer(input$cor_p) + as.integer(input$cor_q)
      params$values <- vapply(seq_len(parameter_count), function(i) {
        as.numeric(input[[paste0("cor_arma_", i)]])
      }, numeric(1))
    }

    if (type %in% c("corExp", "corGaus", "corLin", "corRatio", "corSpher")) {
      dimension <- as.integer(input$cor_dim)
      params$coordinates <- c(
        input$cor_x,
        if (dimension >= 2L) input$cor_y,
        if (dimension >= 3L) input$cor_z
      )
    }

    symm <- NULL
    if (type == "corSymm") {
      table <- rhandsontable::hot_to_r(input$cor_symm_table)
      level_count <- nrow(table)
      symm <- diag(level_count)
      for (row in seq_len(level_count)) {
        for (column in seq_len(level_count)) {
          if (row > column) {
            value <- suppressWarnings(as.numeric(as.character(table[row, column])))
            symm[row, column] <- value
            symm[column, row] <- value
          }
        }
      }
    }

    build_correlation_spec(df, type, params, symm)
  }
  
  output$test_options_ui<-renderUI({
    req(page_started())
    if(input$Type=='F-test'){
      tagList(
        div(
          style = "flex: 1; padding-top: 2px;width:100%;",
          div(
            style='flex:1;',
            selectInput(
              inputId = "Type_ss",
              label = "Sum-of-squares method",
              choices = c('Type I (sequential)' = 'Type I', 'Type II (main effects adjusted)' = 'Type II', 'Type III (fully adjusted)' = 'Type III'),
              selected = "Type III",
              width = '100%'),
            sliderInput("p_value1",'Significance threshold (\u03b1)',min=0.005,max=0.2,value=0.05,step=0.005,width = "100%"),
            field_note("Type III and \u03b1 = 0.05 are common defaults. Change them only when your analysis plan specifies another choice."),
            actionButton('create_result', tagList(icon("calculator"), ' Run power analysis'),
                         class = "btn-primary",
                         style = "width: 100%;",width = "100%")
          )
        )
      )
    }else if(input$Type=='t-test'){
      if(!input$design_title%in%c("Split Plot Design",'General Design')){
        req(input$num_trt,input$level_numbers)
        num_trt <- levels_num_trt()
        
        level_nums <-as.numeric(unlist(strsplit(input$level_numbers, ",")))
        
        contrast_choices <- if(!is.null(level_nums)) {
          if(any(level_nums >= 4, na.rm = TRUE)) {
            c('pairwise','poly','trt.vs.ctrl','Contrast vector')
          } else {
            c('pairwise','trt.vs.ctrl','Contrast vector')
          }
        } else {
          c('pairwise','trt.vs.ctrl','Contrast vector')
        }
        
        tagList(
          if(num_trt>1){
            selected_which <- if (is.null(input$which_para)) "facA" else input$which_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "which_para",
                  label = "Effect to compare",
                  choices = friendly_term_choices(generate_factor_combinations(num_trt)),
                  selected = selected_which,
                  width = "100%")
              )
            )
          },
          if(num_trt>1){
            selected_by <- if (is.null(input$by_para)) "NULL" else input$by_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "by_para",
                  label='Show comparisons within (optional)',
                  choices = filter_combinations(generate_factor_combinations(num_trt), input$which_para),
                  selected = selected_by,
                  width = "100%")
              )
            )
          },
          
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "Contrast",
                label = "Comparison method",
                choices = friendly_contrast_choices(contrast_choices),
                width = "100%",
                selected = if (is.null(input$Contrast)) "pairwise" else {
                  if(input$Contrast %in% contrast_choices) input$Contrast else "pairwise"
                })
            )
          ),
          
          if(!is.null(input$Contrast) && input$Contrast == 'Contrast vector'){
            tagList(
              div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
                  div(
                    style='flex:1;',
                    textInput("custom_contrast", 
                              "Contrast coefficients",
                              placeholder = "For example: 1, -1",
                              width = "100%")
                  )
              ),
              tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
              uiOutput("contrast_validation")
            )
          },
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "alternative",
                label = "Alternative hypothesis",
                choices = c('One-sided' = 'one.sided', 'Two-sided' = 'two.sided'),
                selected = "two.sided",
                width = "100%"),
              sliderInput("p_value2",'Significance threshold (\u03b1)',min=0.005,max=0.2,value=0.05,step=0.005,width = "100%"),
              checkboxInput("p.adj", "Adjust for multiple comparisons (Bonferroni)", value = F,width = "100%"),
              actionButton('create_result', tagList(icon("calculator"), ' Run power analysis'),
                           class = "btn-primary",
                           style = "width: 100%;",width = "100%")
            )
          )
        )
      }else if(input$design_title=='Split Plot Design'){
        req(input$num_trt_main,input$num_trt_sub,input$level_numbers_main,input$level_numbers_sub)
        num_trt_main<-levels_num_trt_main()
        num_trt_sub<-levels_num_trt_sub()
        
        numbers_main<-levels_vec_main()
        numbers_sub<-levels_vec_sub()
        
        contrast_choices <- if(!is.null(num_trt_main)&!is.null(num_trt_sub)) {
          if(any(numbers_main>4)|any(numbers_sub>4)) {
            c('pairwise','poly','trt.vs.ctrl','Contrast vector')
          } else {
            c('pairwise','trt.vs.ctrl','Contrast vector')
          }
        } else {
          c('pairwise','trt.vs.ctrl','Contrast vector')
        }
        
        tagList(
          if(num_trt_main>=1){
            selected_which <- if (is.null(input$which_para)) generate_spd_factors(num_trt_main,num_trt_sub)[1] else input$which_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "which_para",
                  label = "Effect to compare",
                  choices = friendly_term_choices(generate_spd_factors(num_trt_main,num_trt_sub)),
                  selected = selected_which,
                  width = "100%")
              )
            )
          },
          
          if(num_trt_main>=1){
            selected_by <- if (is.null(input$by_para)) "NULL" else input$by_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "by_para",
                  label='Show comparisons within (optional)',
                  choices = filter_combinations(generate_spd_factors(num_trt_main,num_trt_sub),input$which_para),
                  selected = selected_by,
                  width = "100%")
              )
            )
          },
          
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "Contrast",
                label = "Comparison method",
                choices = friendly_contrast_choices(contrast_choices),
                width = "100%",
                selected = if (is.null(input$Contrast)) "pairwise" else {
                  if(input$Contrast %in% contrast_choices) input$Contrast else "pairwise"
                })
            )
          ),
          
          if(!is.null(input$Contrast) && input$Contrast == 'Contrast vector'){
            tagList(
              div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
                  div(
                    style='flex:1;',
                    textInput("custom_contrast", 
                              "Contrast coefficients",
                              placeholder = "For example: 1, -1",
                              width = "100%")
                  )
              ),
              tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
              uiOutput("contrast_validation")
            )
          },
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "alternative",
                label = "Alternative hypothesis",
                choices = c('One-sided' = 'one.sided', 'Two-sided' = 'two.sided'),
                selected = "two.sided",
                width = "100%"),
              sliderInput("p_value2",'Significance threshold (\u03b1)',min=0.005,max=0.2,value=0.05,step=0.005,width = "100%"),
              checkboxInput("p.adj", "Adjust for multiple comparisons (Bonferroni)", value = F),
              actionButton('create_result', tagList(icon("calculator"), ' Run power analysis'),
                           class = "btn-primary",
                           style = "width: 100%;",width = "100%")
            )
          )
        )
      }else if(input$design_title=='General Design'){
        #req(input$uploaded_file)
        df<-if (!is.null(input$uploaded_file)) datavalues$uploaded_data else NULL
        tagList(
          div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                textInput("which_para", "Effect to compare",
                          placeholder = "For example: treatment or treatment:time",width = "100%")
              )
          ),
          tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
          uiOutput("which_validation"),
          
          div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                textInput("by_para", "Show comparisons within (optional)",
                          value = "",
                          placeholder = "For example: time",width = "100%")
              )
          ),
          tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
          uiOutput("by_validation"),
          
          uiOutput('contrast_ui'),
          
          if(!is.null(input$Contrast) && input$Contrast == 'Contrast vector'){
            tagList(
              div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
                  div(
                    style='flex:1;',
                    textInput("custom_contrast", 
                              "Contrast coefficients",
                              placeholder = "For example: 1, -1",
                              width = "100%")
                  )
              ),
              tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
              uiOutput("contrast_validation2")
            )
          },
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "alternative",
                label = "Alternative hypothesis",
                choices = c('One-sided' = 'one.sided', 'Two-sided' = 'two.sided'),
                selected = "two.sided",width = "100%"),
              sliderInput("p_value2",'Significance threshold (\u03b1)',min=0.005,max=0.2,value=0.05,step=0.005,width = "100%"),
              checkboxInput("p.adj", "Adjust for multiple comparisons (Bonferroni)", value = F,width = "100%"),
              actionButton('create_result', tagList(icon("calculator"), ' Run power analysis'),
                           class = "btn-primary",
                           style = "width: 100%;",width = "100%")
            )
          )
        )
      }
    }else if(input$Type=='F-test & t-test'){
      if(!input$design_title%in%c('Split Plot Design','General Design')){
        req(input$num_trt, input$level_numbers)
        num_trt <- levels_num_trt()
        
        level_nums <-as.numeric(unlist(strsplit(input$level_numbers, ",")))
        
        contrast_choices <- if(!is.null(level_nums)) {
          if(any(level_nums >= 4, na.rm = TRUE)) {
            c('pairwise','poly','trt.vs.ctrl','Contrast vector')
          } else {
            c('pairwise','trt.vs.ctrl','Contrast vector')
          }
        } else {
          c('pairwise','trt.vs.ctrl','Contrast vector')
        }
        
        tagList(
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "Type_ss",
                label = "Sum-of-squares method",
                choices = c('Type I (sequential)' = 'Type I', 'Type II (main effects adjusted)' = 'Type II', 'Type III (fully adjusted)' = 'Type III'),
                selected = "Type III",
                width = "100%"),
              sliderInput("p_value1",'Significance threshold (\u03b1)',min=0.005,max=0.2,value=0.05,step=0.005,width = "100%")
            )
          ),
          if(num_trt>1){
            selected_which <- if (is.null(input$which_para)) "facA" else input$which_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "which_para",
                  label = "Effect to compare",
                  choices = friendly_term_choices(generate_factor_combinations(num_trt)),
                  selected = selected_which,width = "100%")
              )
            )
          },
          if(num_trt>1){
            selected_by <- if (is.null(input$by_para)) "NULL" else input$by_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "by_para",
                  label='Show comparisons within (optional)',
                  choices = filter_combinations(generate_factor_combinations(num_trt), input$which_para),
                  selected = selected_by,width = "100%")
              )
            )
          },
          
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "Contrast",
                label = "Comparison method",
                choices = friendly_contrast_choices(contrast_choices),
                width = "100%",
                selected = if (is.null(input$Contrast)) "pairwise" else {
                  if(input$Contrast %in% contrast_choices) input$Contrast else "pairwise"
                })
            )
          ),
          if(!is.null(input$Contrast) && input$Contrast == 'Contrast vector'){
            tagList(
              div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
                  div(
                    style='flex:1;',
                    textInput("custom_contrast", 
                              "Contrast coefficients",
                              placeholder = "For example: 1, -1",
                              width = "100%")
                  )
              ),
              tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
              uiOutput("contrast_validation")
            )
          },
          
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "alternative",
                label = "Alternative hypothesis",
                choices = c('One-sided' = 'one.sided', 'Two-sided' = 'two.sided'),
                selected = "two.sided",
                width = "100%"),
              checkboxInput("p.adj", "Adjust for multiple comparisons (Bonferroni)", value = F,width = "100%"),
              actionButton('create_result', tagList(icon("calculator"), ' Run power analysis'),
                           class = "btn-primary",
                           style = "width: 100%;",width = "100%")
            )
          )
        )
      }else if(input$design_title=='Split Plot Design'){
        req(input$num_trt_main,input$num_trt_sub,input$level_numbers_main,input$level_numbers_sub)
        num_trt_main<-levels_num_trt_main()
        num_trt_sub<-levels_num_trt_sub()
        
        numbers_main<-levels_vec_main()
        numbers_sub<-levels_vec_sub()
        
        contrast_choices <- if(!is.null(num_trt_main)&!is.null(num_trt_sub)) {
          if(any(num_trt_main*num_trt_sub >= 4, na.rm = TRUE)) {
            c('pairwise','poly','trt.vs.ctrl','Contrast vector')
          } else {
            c('pairwise','trt.vs.ctrl','Contrast vector')
          }
        } else {
          c('pairwise','trt.vs.ctrl','Contrast vector')
        }
        
        tagList(
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "Type_ss",
                label = "Sum-of-squares method",
                choices = c('Type I (sequential)' = 'Type I', 'Type II (main effects adjusted)' = 'Type II', 'Type III (fully adjusted)' = 'Type III'),
                selected = "Type III",
                width = "100%"),
              sliderInput("p_value1",'Significance threshold (\u03b1)',min=0.005,max=0.2,value=0.05,step=0.005,width = "100%")
            )
          ),
          
          if(num_trt_main>=1){
            selected_which <- if (is.null(input$which_para)) generate_spd_factors(num_trt_main,num_trt_sub)[1] else input$which_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "which_para",
                  label = "Effect to compare",
                  choices = friendly_term_choices(generate_spd_factors(num_trt_main,num_trt_sub)),
                  selected = selected_which,
                  width = "100%")
              )
            )
          },
          
          if(num_trt_main>=1){
            selected_by <- if (is.null(input$by_para)) "NULL" else input$by_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "by_para",
                  label='Show comparisons within (optional)',
                  choices = filter_combinations(generate_spd_factors(num_trt_main,num_trt_sub),input$which_para),
                  selected = selected_by,
                  width = "100%")
              )
            )
          },
          
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "Contrast",
                label = "Comparison method",
                choices = friendly_contrast_choices(contrast_choices),
                width = "100%",
                selected = if (is.null(input$Contrast)) "pairwise" else {
                  if(input$Contrast %in% contrast_choices) input$Contrast else "pairwise"
                })
            )
          ),
          
          if(!is.null(input$Contrast) && input$Contrast == 'Contrast vector'){
            tagList(
              div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
                  div(
                    style='flex:1;', 
                    textInput("custom_contrast", 
                              "Contrast coefficients",
                              placeholder = "For example: 1, -1",
                              width = "100%")
                  )
              ),
              tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
              uiOutput("contrast_validation")
            )
          },
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "alternative",
                label = "Alternative hypothesis",
                choices = c('One-sided' = 'one.sided', 'Two-sided' = 'two.sided'),
                selected = "two.sided",
                width = "100%"),
              checkboxInput("p.adj", "Adjust for multiple comparisons (Bonferroni)", value = F,width = "100%"),
              actionButton('create_result', tagList(icon("calculator"), ' Run power analysis'),
                           class = "btn-primary",
                           style = "width: 100%;",width = "100%")
            )
          )
        )
      }else if(input$design_title=='General Design'){
        df<-if (!is.null(input$uploaded_file)) datavalues$uploaded_data else NULL
        tagList(
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "Type_ss",
                label = "Sum-of-squares method",
                choices = c('Type I (sequential)' = 'Type I', 'Type II (main effects adjusted)' = 'Type II', 'Type III (fully adjusted)' = 'Type III'),
                selected = "Type III",
                width = "100%"),
              sliderInput("p_value1",'Significance threshold (\u03b1)',min=0.005,max=0.2,value=0.05,step=0.005,width = "100%")
            )
          ),
          
          div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                textInput("which_para", "Effect to compare",
                          placeholder = "For example: treatment or treatment:time",width = "100%")
              )
          ),
          tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
          uiOutput("which_validation"),
          
          div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                textInput("by_para", "Show comparisons within (optional)",
                          value = "",
                          placeholder = "For example: time",width = "100%")
              )
          ),
          tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
          uiOutput("by_validation"),
          
          uiOutput('contrast_ui'),
          
          if(!is.null(input$Contrast) && input$Contrast == 'Contrast vector'){
            tagList(
              div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
                  div(
                    style='flex:1;',
                    textInput("custom_contrast", 
                              "Contrast coefficients",
                              placeholder = "For example: 1, -1",
                              width = "100%")
                  )
              ),
              tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
              uiOutput("contrast_validation2")
            )
          },
          div(
            style = "flex: 1; padding-top: 2px;width:100%;",
            div(
              style='flex:1;',
              selectInput(
                inputId = "alternative",
                label = "Alternative hypothesis",
                choices = c('One-sided' = 'one.sided', 'Two-sided' = 'two.sided'),
                selected = "two.sided",
                width = "100%"),
              checkboxInput("p.adj", "Adjust for multiple comparisons (Bonferroni)", value = F,width = "100%"),
              actionButton('create_result', tagList(icon("calculator"), ' Run power analysis'),
                           class = "btn-primary",
                           style = "width: 100%;",width = "100%")
            )
          )
        )
      }
    }})
  
  output$by_validation <- renderUI({
    req(page_started())
    if (is.null(input$uploaded_file) || is.null(input$by_para) || input$by_para == "") return(NULL)
    if (toupper(input$by_para) == "NULL") return(NULL) 
    
    df <- datavalues$uploaded_data
    by_factors <- unlist(strsplit(input$by_para, "\\:"))
    by_factors <- trimws(by_factors)
    
    missing_cols <- setdiff(by_factors, colnames(df))
    
    which_factors <- unlist(strsplit(input$which_para, "\\:"))
    which_factors <- trimws(which_factors)
    duplicated_factors <- intersect(by_factors, which_factors)
    
    msgs <- c()
    if (length(missing_cols) > 0) {
      msgs <- c(msgs, paste0("Use column names from the uploaded data. Not found: ",
                             paste(missing_cols, collapse = ", ")))
    }
    if (length(duplicated_factors) > 0) {
      msgs <- c(msgs, paste0("Remove columns already included in the effect to compare: ",
                             paste(duplicated_factors, collapse = ", ")))
    }
    
    if (length(msgs) > 0) {
      div(
        style = "color: #d9534f; margin-top: 4px;",
        paste(msgs, collapse = " | ")
      )
    } else {
      NULL
    }
  })
  
  contrast_choices <- reactive({
    req(page_started())
    nums <- level_nums()
    if (is.null(nums)) {
      c('pairwise', 'poly','trt.vs.ctrl', 'Contrast vector')
    }else if (any(prod(nums) >= 4, na.rm = TRUE)) {
      c('pairwise', 'poly', 'trt.vs.ctrl', 'Contrast vector')
    } else {
      c('pairwise', 'trt.vs.ctrl', 'Contrast vector')
    }
  })
  
  output$contrast_ui <- renderUI({
    req(page_started())
    div(
      style = "flex: 1; padding-top: 2px;width:100%;",
      div(
        style='flex:1;',
        selectInput(
          inputId = "Contrast",
          label = "Comparison method",
          choices = friendly_contrast_choices(contrast_choices()),
          width = "100%",
          selected = if (is.null(input$Contrast)) {
            "pairwise"
          } else {
            if (input$Contrast %in% contrast_choices()) input$Contrast else "pairwise"
          }
        )
      )
    )
  })
  
  output$contrast_validation <- renderUI({
    req(page_started())
    req(input$custom_contrast, input$Contrast == 'Contrast vector')
    
    vec <- tryCatch({
      as.numeric(unlist(strsplit(input$custom_contrast, ",")))
    }, warning = function(w) {
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(vec) || any(is.na(vec))) {
      return(tags$div(style = "color: red;", 
                      "Enter only numeric coefficients separated by commas, such as 1, -1."))
    }
    
    if (sum(vec) != 0) {
      return(tags$div(style = "color: red;", 
                      paste0("Contrast coefficients must sum to 0. The current sum is ", sum(vec), ".")))
    }
    
    level_nums <- tryCatch({
      as.numeric(unlist(strsplit(input$level_numbers, ",")))
    }, error = function(e) return(NULL))
    
    if (is.null(level_nums) || any(is.na(level_nums))) {
      return(tags$div(style = "color: red;", 
                      "Set valid treatment levels before entering custom contrast coefficients."))
    }
    
    expected_length <- NA 
    
    if(input$design_title!='Split Plot Design'){
      factor_count <- levels_num_trt()
      if (factor_count == 1) {
        expected_length <- level_nums[1]
      } else {  
        factors <- paste0("fac", LETTERS[seq_len(factor_count)])
        level_map <- setNames(level_nums, factors)
        chosen <- input$which_para
        if (chosen %in% factors) {
          expected_length <- level_map[chosen]
        } else if (grepl("\\:", chosen)) {
          parts <- unlist(strsplit(chosen, "\\:"))
          expected_length <- prod(level_map[parts])
        }
      }
    }else if(input$design_title=='Split Plot Design'){
      expected_length <- {
        selected_factors <- unlist(strsplit(input$which_para, "\\:"))
        recorded_numbers <- numeric(0)
        for (fac in selected_factors) {
          if (fac == "trt.main") {
            recorded_numbers <- c(recorded_numbers, as.numeric(input$level_numbers_main))
          } else if (fac == "trt.sub") {
            recorded_numbers <- c(recorded_numbers, as.numeric(input$level_numbers_sub))
          } else if (grepl("\\.main$", fac)) {
            letter <- gsub("fac([A-Z]+)\\.main", "\\1", fac)
            index <- match(letter, LETTERS)
            
            numbers_main<-levels_vec_main()
            
            recorded_numbers <- c(recorded_numbers, as.numeric(numbers_main[index]))
          } else if (grepl("\\.sub$", fac)) {
            letter <- gsub("fac([A-Z]+)\\.sub", "\\1", fac)
            index <- match(letter, LETTERS)
            
            numbers_sub<-levels_vec_sub()
            
            recorded_numbers <- c(recorded_numbers, as.numeric(numbers_sub[index]))
          }
        }
        expected_length <- prod(recorded_numbers, na.rm = TRUE)
        expected_length
      }
    }
    
    if (length(vec) != expected_length) {
      return(tags$div(style = "color: red;", 
                      paste0("Enter ", expected_length, " coefficients for ",
                             friendly_term_choices(input$which_para) |> names(),
                             "; ", length(vec), " were provided.")
      ))
    }
    
    tags$div(style = "color: green;", "\u2713 Contrast coefficients are valid.")
  })
  
  output$contrast_validation2 <- renderUI({
    req(page_started())
    req(input$custom_contrast, input$Contrast == 'Contrast vector')
    req(datavalues$uploaded_data)
    df <- datavalues$uploaded_data
    req(input$which_para)
    
    vec <- tryCatch({
      as.numeric(unlist(strsplit(input$custom_contrast, ",")))
    }, warning = function(w) return(NULL),
    error = function(e) return(NULL))
    
    if (is.null(vec) || any(is.na(vec))) {
      return(tags$div(style = "color: red;", 
                      "Enter only numeric coefficients separated by commas, such as 1, -1."))
    }
    
    if (sum(vec) != 0) {
      return(tags$div(style = "color: red;", 
                      paste0("Contrast coefficients must sum to 0. The current sum is ", sum(vec), ".")))
    }
    
    factors <- unlist(strsplit(input$which_para, "\\:"))
    factors <- trimws(factors)
    
    missing_cols <- setdiff(factors, colnames(df))
    if (length(missing_cols) > 0) {
      return(tags$div(style = "color: red;", 
                      paste("Use column names from the uploaded data. Not found:",
                            paste(missing_cols, collapse = ", "))))
    }
    
    level_counts <- sapply(factors, function(fac) length(unique(df[[fac]])))
    expected_length <- prod(level_counts)
    
    if (length(vec) != expected_length) {
      return(tags$div(style = "color: red;", 
                      paste0("Enter ", expected_length, " coefficients for ", input$which_para,
                             "; ", length(vec), " were provided.")
      ))
    }
    
    tags$div(style = "color: green;", "\u2713 Contrast coefficients are valid.")
  })
  
  observeEvent(input$which_para, {
    req(page_started())
    req(!input$design_title %in% c("Split Plot Design", "General Design"))
    factor_count <- levels_num_trt()
    updateSelectInput(session=getDefaultReactiveDomain(), "by_para",
                      choices = filter_combinations(generate_factor_combinations(factor_count), input$which_para),
                      selected = if (input$by_para %in% filter_combinations(generate_factor_combinations(factor_count), input$which_para)) {
                        input$by_para
                      } else {
                        "NULL"
                      }
    )
  }, ignoreInit = TRUE)
  
  output$level_numbers_validation3 <- renderUI({
    req(page_started())
    req(datavalues$uploaded_data)
    req(input$Formula_general)
    
    validation_error <- tryCatch({
      validate_model_formula(input$Formula_general, datavalues$uploaded_data)
      NULL
    }, error = function(error) conditionMessage(error))
    if (!is.null(validation_error)) {
      return(div(
        style = "color: #d9534f; margin-top: 4px;",
        validation_error
      ))
    }
    
    return(div(style = "color: #28a745; margin-top: 4px;",
               "\u2713 Model formula is valid and all variables were found in the uploaded data."))
  })

  active_factor_count_error <- function() {
    if (input$design_title == "General Design") {
      return(NULL)
    }
    if (input$design_title == "Split Plot Design") {
      main_result <- factor_count_result(input$num_trt_main)
      sub_result <- factor_count_result(input$num_trt_sub)
      if (!is.null(main_result$message)) return(main_result$message)
      if (!is.null(sub_result$message)) return(sub_result$message)
      if (main_result$value + sub_result$value > MAX_TREATMENT_FACTORS) {
        return(sprintf(
          "Main-plot and sub-plot factors combined cannot exceed %d.",
          MAX_TREATMENT_FACTORS
        ))
      }
      return(NULL)
    }

    factor_count_result(input$num_trt)$message
  }
  
  observeEvent(input$create_result,{
    req(page_started())
    req(input$design_title)
    count_error <- active_factor_count_error()
    if (!is.null(count_error)) {
      showNotification(count_error, type = "warning", duration = 8)
      return(NULL)
    }
    results_generated(FALSE)
    tryCatch({
      output$results_display <- renderUI({
        req(input$Type)
        if (input$Type == "F-test") {
          return(
            tagList(
              result_table_block(
                "Overall-effect power",
                "Each row estimates power for the omnibus F-test of one model effect.",
                tableOutput("power_omnibus_test")
              ),
              interpretation_guide()
            )
          )
        } else if (input$Type == "t-test") {
          return(
            tagList(
              result_table_block(
                "Comparison-specific power",
                "Each row estimates power for one selected contrast or pairwise comparison.",
                tableOutput("power_contrast")
              ),
              interpretation_guide()
            )
          )
        } else if (input$Type == "F-test & t-test") {
          return(
            tagList(
              result_table_block(
                "Overall-effect power",
                "Omnibus F-tests answer whether an effect is detectable somewhere across its levels.",
                tableOutput("power_omnibus_test")
              ),
              result_table_block(
                "Comparison-specific power",
                "Contrasts answer whether the selected treatment differences are detectable.",
                tableOutput("power_contrast")
              ),
              interpretation_guide()
            )
          )
        }
      })
      
      req(values$data,values$variance,input$design_table)
      
      values_mean<-hot_to_r(input$design_table)
      values_mean <- as.numeric(unlist(values_mean))
      if(input$design_title=="Completely Randomized Design"){
        values_variance<-hot_to_r(input$design_variance_table)
        values_variance<-as.numeric(unlist(values_variance))
        
        if (any(is.na(values_mean)) || any(is.na(values_variance))) {
          showNotification("Complete every highlighted cell in the expected-response and variation tables, then run the analysis again.",
                           type = "error", duration = 5)
          return(NULL)
        }else{
          
          if(levels_num_trt()==1){
            crd <- build_crd_design(
              treatments = levels_vec(),
              replicates = input$num_rep,
              means = values_mean,
              sigma2 = values_variance
            )
          }else if(levels_num_trt()>1&interaction_option_number()=='No'){
            fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
            fac_formula <- paste(fac_names, collapse = "+")
            crd <- build_crd_design(
              treatments = levels_vec(),
              replicates = input$num_rep,
              formula=as.formula(paste0('~',fac_formula)),
              means = values_mean,
              sigma2 = values_variance
            )
          }else if(levels_num_trt()>1&interaction_option_number()=='Yes'){
            fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
            
            if(is.null(interaction_formula_number())){
              fac_formula <- paste(fac_names, collapse = "+")
            }else{
              interaction_terms <- interaction_formula_number()
              interaction_terms<-unique(interaction_terms)
              interaction_terms<-interaction_terms[order(interaction_terms)]
              
              fac_formula <- paste(c(fac_names, interaction_terms), collapse = "+")
            }
            crd <- build_crd_design(
              treatments = levels_vec(),
              replicates = input$num_rep,
              formula=as.formula(paste0('~',fac_formula)),
              means = values_mean,
              sigma2 = values_variance
            )
          }
        }
      }else if(input$design_title=='Randomized Complete Block Design'){
        values_variance<-hot_to_r(input$design_variance_table)
        values_variance<-as.numeric(unlist(values_variance))
        values_vcomp<-as.numeric(values_variance[1])
        values_sigma2<-as.numeric(values_variance[2])
        
        if (any(is.na(values_mean)) || any(is.na(values_variance))) {
          showNotification("Complete every highlighted cell in the expected-response and variation tables, then run the analysis again.",
                           type = "error", duration = 5)
          return(NULL)
        }else{
          if(levels_num_trt()==1){
            crd <- build_rcbd_design(
              treatments = levels_vec(),
              blocks = input$num_block,
              means = values_mean,
              vcomp = values_vcomp,
              sigma2 = values_sigma2
            )
          }else if(levels_num_trt()>1&interaction_option_number()=='No'){
            fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
            fac_formula <- paste(fac_names, collapse = "+")
            
            crd <- build_rcbd_design(
              treatments = levels_vec(),
              blocks = input$num_block,
              formula= as.formula(paste0('~',fac_formula,'+(1|block)')),
              means = values_mean,
              vcomp = values_vcomp,
              sigma2 = values_sigma2
            )
          }else if(levels_num_trt()>1&interaction_option_number()=='Yes'){
            fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
            
            if(is.null(interaction_formula_number())){
              fac_formula <- paste(fac_names, collapse = "+")
            }else{
              interaction_terms <- interaction_formula_number()
              interaction_terms<-unique(interaction_terms)
              interaction_terms<-interaction_terms[order(interaction_terms)]
              
              fac_formula <- paste(c(fac_names, interaction_terms), collapse = "+")
            }
            crd <- build_rcbd_design(
              treatments = levels_vec(),
              blocks = input$num_block,
              formula= as.formula(paste0('~',fac_formula,'+(1|block)')),
              means = values_mean,
              vcomp = values_vcomp,
              sigma2 = values_sigma2
            )
          }
        }
      }else if(input$design_title=='Latin Square Design'){
        values_variance<-hot_to_r(input$design_variance_table)
        values_variance<-as.numeric(unlist(values_variance))
        
        values_vcomp<-as.numeric(values_variance[c(1,2)])
        values_sigma2<-as.numeric(values_variance[3])
        
        if (any(is.na(values_mean)) || any(is.na(values_variance))) {
          showNotification("Complete every highlighted cell in the expected-response and variation tables, then run the analysis again.",
                           type = "error", duration = 5)
          return(NULL)
        }else{
          if(levels_num_trt()==1){
            crd <- build_latin_square_design(
              treatments = levels_vec(),
              squares = input$num_squares,
              reuse=input$value_reuse,
              means = values_mean,
              vcomp = values_vcomp,
              sigma2 = values_sigma2
            )
          }else if(levels_num_trt()>1&interaction_option_number()=='No'){
            fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
            fac_formula <- paste(fac_names, collapse = "+")
            crd <- build_latin_square_design(
              treatments = levels_vec(),
              squares = input$num_squares,
              reuse=input$value_reuse,
              formula= as.formula(paste0('~',fac_formula,'+(1|row)+(1|col)')),
              means = values_mean,
              vcomp = values_vcomp,
              sigma2 = values_sigma2
            )
          }else if(levels_num_trt()>1&interaction_option_number()=='Yes'){
            fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
            
            if(is.null(interaction_formula_number())){
              fac_formula <- paste(fac_names, collapse = "+")
            }else{
              interaction_terms <- interaction_formula_number()
              interaction_terms<-unique(interaction_terms)
              interaction_terms<-interaction_terms[order(interaction_terms)]
              
              fac_formula <- paste(c(fac_names, interaction_terms), collapse = "+")
            }
            crd <- build_latin_square_design(
              treatments = levels_vec(),
              squares = input$num_squares,
              reuse=input$value_reuse,
              formula= as.formula(paste0('~',fac_formula,'+(1|row)+(1|col)')),
              means = values_mean,
              vcomp = values_vcomp,
              sigma2 = values_sigma2
            )
          }
        }
      }else if(input$design_title=='Split Plot Design'){
        values_variance<-hot_to_r(input$design_variance_table)
        values_variance<-as.numeric(unlist(values_variance))
        
        values_vcomp<-as.numeric(values_variance[1])
        values_sigma2<-as.numeric(values_variance[2])
        
        if (any(is.na(values_mean)) || any(is.na(values_variance))) {
          showNotification("Complete every highlighted cell in the expected-response and variation tables, then run the analysis again.",
                           type = "error", duration = 5)
          return(NULL)
        }else{
          
          if(levels_num_trt_main()==1){
            fac_names_main<-'trt.main'
          }else if(levels_num_trt_main()>1){
            fac_names_main <- paste0("fac", LETTERS[1:levels_num_trt_main()],'.main')
          }
          
          if(levels_num_trt_sub()==1){
            fac_names_sub<-'trt.sub'
          }else if(levels_num_trt_sub()>1){
            fac_names_sub <- paste0("fac", LETTERS[1:levels_num_trt_sub()],'.sub')
          }
          
          fac_names<-c(fac_names_main,fac_names_sub)
          
          if(input$interaction_option=='No'){
            fac_formula <- paste(fac_names, collapse = " + ")
          }else if(input$interaction_option=='Yes'){
            if(is.null(input$interaction_formula)){
              fac_formula <- paste(fac_names, collapse = " + ")
            }else{
              interaction_terms <- input$interaction_formula
              interaction_terms<-unique(interaction_terms)
              interaction_terms<-interaction_terms[order(interaction_terms)]
              
              fac_formula <- paste(c(fac_names, interaction_terms), collapse = " + ")
            }
          }
          
          crd<-build_split_plot_design(
            trt.main = levels_vec_main(),
            trt.sub = levels_vec_sub(),
            replicates = input$num_rep,
            formula= as.formula(paste0('~',fac_formula,'+(1|mainplot)')),
            means = values_mean,
            vcomp = values_vcomp,
            sigma2 = values_sigma2
          )
        }
      }else if(input$design_title=='General Design'){
        req(input$uploaded_file, input$Formula_general)
        df<-datavalues$uploaded_data
        
        cols <- colnames(df)
        types <- factor_types_number()
        
        for (i in seq_along(cols)) {
          if (types[i] == "Categorical") {
            df[[cols[i]]] <- as.factor(df[[cols[i]]])
          } else if (types[i] == "Continuous") {
            df[[cols[i]]] <- as.numeric(df[[cols[i]]])
          }
        }
        formula_general <- validate_model_formula(input$Formula_general, df)
        
        # Build correlation object safely (no eval/parse)
        cor_result <- build_correlation(df, input$cor_type)
        df_use   <- if (!is.null(cor_result)) cor_result$df_model else df
        cor_obj  <- if (!is.null(cor_result)) cor_result$cor     else NULL
        
        variance_list <- values$variance
        values_variance <- c()
        
        for(i in seq_along(variance_list)){
          table_id <- paste0("design_variance_table_", i)
          
          table_data <- hot_to_r(input[[table_id]])
          
          n_row <- nrow(table_data)
          n_col <- ncol(table_data)
          
          for(c in 1:n_col){
            for(r in c:n_row){ 
              cell_value <- as.numeric(as.character(table_data[r, c]))
              if(cell_value != "" && cell_value != "--" && !is.na(cell_value)){
                values_variance <- c(values_variance, as.numeric(cell_value))
              }
            }
          }
        }
        
        values_sigma2<-values_variance[length(values_variance)]
        
        mk_args <- list(
          formula = formula_general,
          data    = df_use,
          means   = values_mean,
          sigma2  = values_sigma2
        )
        if (length(values_variance) > 1)
          mk_args$vcomp <- values_variance[1:(length(values_variance) - 1)]
        if (!is.null(cor_obj))
          mk_args$correlation <- cor_obj
        
        crd <- do.call(build_general_design, mk_args)
      }
      
      if(input$Type=='F-test'){
        results <- reactiveValues(
          omnibus = NULL
        )
        convert_type <- function(input) {
          if(input=='Type I'){
            return(1)
          }else if(input=='Type II'){
            return(2)
          }else if(input=='Type III'){
            return(3)
          }
        }
        typess<-convert_type(input$Type_ss)
        pvalue<-as.numeric(input$p_value1)
        results$omnibus <- as.data.frame(calculate_power_results(
          crd,
          test_type = "F-test",
          type_ss = typess,
          sig_level_f = pvalue
        )$omnibus)
        results$omnibus <- format_omnibus_result(results$omnibus)
        output$power_omnibus_test <- renderTable({
          results$omnibus
        }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
      }else if(input$Type=='t-test'){
        results <- reactiveValues(
          contrast = NULL
        )
        if((!input$design_title%in%c('Split Plot Design','General Design')&&input$num_trt==1)){
          if(input$Contrast!='Contrast vector'){
            results$contrast<-calculate_contrast_power(crd,
                                           which =  "trt", 
                                           contrast = input$Contrast,
                                           sig.level = input$p_value2,
                                           p.adj = input$p.adj,
                                           alternative=input$alternative)
            contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
            colnames(contrast_result)[1]<-' Contrast '
            results$contrast<-contrast_result
            output$power_contrast <- renderTable({
              results$contrast
            }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
          }else if(input$Contrast=='Contrast vector'){
            req(input$custom_contrast)
            numbers_custom_contrast <- parse_custom_contrast(input$custom_contrast)
            
            results$contrast<-calculate_contrast_power(crd,
                                           which =  "trt", 
                                           contrast = list(numbers_custom_contrast),
                                           sig.level = input$p_value2,
                                           p.adj = input$p.adj,
                                           alternative=input$alternative)
            contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
            colnames(contrast_result)[1]<-' Contrast '
            contrast_result[1,1]<-'Contrast vector'
            results$contrast<-contrast_result
            output$power_contrast <- renderTable({
              results$contrast
            }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
          }
        }else if((!input$design_title%in%c('Split Plot Design','General Design')&&input$num_trt>1)||input$design_title%in%c('Split Plot Design','General Design')){
          which_para<-as.character(input$which_para)
          by_para<-as.character(input$by_para)
          if(by_para=='NULL'){
            
            if(input$Contrast!='Contrast vector'){
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             contrast = input$Contrast,
                                             sig.level = input$p_value2,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
              colnames(contrast_result)[1]<-' Contrast '
              results$contrast<-contrast_result
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }else if(input$Contrast=='Contrast vector'){
              req(input$custom_contrast)
              numbers_custom_contrast <- parse_custom_contrast(input$custom_contrast)
              
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             contrast = list(numbers_custom_contrast),
                                             sig.level = input$p_value2,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              
              contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
              colnames(contrast_result)[1]<-' Contrast '
              contrast_result[1,1]<-'Contrast vector'
              results$contrast<-contrast_result
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }
            
          }else{
            if(input$Contrast!='Contrast vector'){
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             by=by_para,
                                             contrast = input$Contrast,
                                             sig.level = input$p_value2,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              contrast_result<-results$contrast
              nnn<-length(contrast_result)
              contrast_result2<-data.frame()
              for(multi_by in 1:nnn){
                dff<-as.data.frame(contrast_result[[multi_by]])
                name_con<-names(contrast_result[multi_by])
                dff<-as.data.frame(cbind(name_con,row.names(dff),dff))
                colnames(dff)[c(1,2)]<-c(' Variable ',' Contrast ')
                contrast_result2<-rbind(contrast_result2,dff)
              }
              results$contrast<-contrast_result2
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }else if(input$Contrast=='Contrast vector'){
              req(input$custom_contrast)
              numbers_custom_contrast <- parse_custom_contrast(input$custom_contrast)
              
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             by=by_para,
                                             contrast = list(numbers_custom_contrast),
                                             sig.level = input$p_value2,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              contrast_result<-results$contrast
              nnn<-length(contrast_result)
              contrast_result2<-data.frame()
              for(multi_by in 1:nnn){
                dff<-as.data.frame(contrast_result[[multi_by]])
                name_con<-names(contrast_result[multi_by])
                dff<-as.data.frame(cbind(name_con,row.names(dff),dff))
                colnames(dff)[c(1,2)]<-c(' Variable ',' Contrast ')
                dff[1,2]<-'Contrast vector'
                contrast_result2<-rbind(contrast_result2,dff)
              }
              results$contrast<-contrast_result2
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }
          }
        }
      }else if(input$Type=='F-test & t-test'){
        results <- reactiveValues(
          omnibus = NULL,
          contrast = NULL,
          all=NULL
        )
        convert_type <- function(input) {
          if(input=='Type I'){
            return(1)
          }else if(input=='Type II'){
            return(2)
          }else if(input=='Type III'){
            return(3)
          }
        }
        typess<-convert_type(input$Type_ss)
        pvalue<-as.numeric(input$p_value1)
        results$omnibus <- as.data.frame(calculate_power_results(
          crd,
          test_type = "F-test",
          type_ss = typess,
          sig_level_f = pvalue
        )$omnibus)
        results$omnibus <- format_omnibus_result(results$omnibus)
        output$power_omnibus_test <- renderTable({
          results$omnibus
        }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
        
        if((!input$design_title%in%c('Split Plot Design','General Design')&&input$num_trt==1)){
          if(input$Contrast!='Contrast vector'){
            results$contrast<-calculate_contrast_power(crd,
                                           which =  "trt", 
                                           contrast = input$Contrast,
                                           sig.level = input$p_value1,
                                           p.adj = input$p.adj,
                                           alternative=input$alternative)
            contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
            colnames(contrast_result)[1]<-' Contrast '
            results$contrast<-contrast_result
            output$power_contrast <- renderTable({
              results$contrast
            }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
          }else if(input$Contrast=='Contrast vector'){
            req(input$custom_contrast)
            numbers_custom_contrast <- parse_custom_contrast(input$custom_contrast)
            
            results$contrast<-calculate_contrast_power(crd,
                                           which =  "trt", 
                                           contrast = list(numbers_custom_contrast),
                                           sig.level = input$p_value1,
                                           p.adj = input$p.adj,
                                           alternative=input$alternative)
            contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
            colnames(contrast_result)[1]<-' Contrast '
            contrast_result[1,1]<-'Contrast vector'
            results$contrast<-contrast_result
            output$power_contrast <- renderTable({
              results$contrast
            }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
          }
        }else if((!input$design_title%in%c('Split Plot Design','General Design')&&input$num_trt>1)||input$design_title%in%c('Split Plot Design','General Design')){
          which_para<-as.character(input$which_para)
          by_para<-as.character(input$by_para)
          if(by_para=='NULL'){
            
            if(input$Contrast!='Contrast vector'){
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             contrast = input$Contrast,
                                             sig.level = input$p_value1,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
              colnames(contrast_result)[1]<-' Contrast '
              results$contrast<-contrast_result
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }else if(input$Contrast=='Contrast vector'){
              req(input$custom_contrast)
              numbers_custom_contrast <- parse_custom_contrast(input$custom_contrast)
              
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             contrast = list(numbers_custom_contrast),
                                             sig.level = input$p_value1,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              
              contrast_result<-as.data.frame(cbind(row.names(results$contrast),results$contrast))
              colnames(contrast_result)[1]<-' Contrast '
              contrast_result[1,1]<-'Contrast vector'
              results$contrast<-contrast_result
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }
            
          }else{
            if(input$Contrast!='Contrast vector'){
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             by=by_para,
                                             contrast = input$Contrast,
                                             sig.level = input$p_value1,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              contrast_result<-results$contrast
              nnn<-length(contrast_result)
              contrast_result2<-data.frame()
              for(multi_by in 1:nnn){
                dff<-as.data.frame(contrast_result[[multi_by]])
                name_con<-names(contrast_result[multi_by])
                dff<-as.data.frame(cbind(name_con,row.names(dff),dff))
                colnames(dff)[c(1,2)]<-c(' Variable ',' Contrast ')
                contrast_result2<-rbind(contrast_result2,dff)
              }
              results$contrast<-contrast_result2
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }else if(input$Contrast=='Contrast vector'){
              req(input$custom_contrast)
              numbers_custom_contrast <- parse_custom_contrast(input$custom_contrast)
              
              results$contrast<-calculate_contrast_power(crd,
                                             which=which_para,
                                             by=by_para,
                                             contrast = list(numbers_custom_contrast),
                                             sig.level = input$p_value1,
                                             p.adj = input$p.adj,
                                             alternative=input$alternative)
              contrast_result<-results$contrast
              nnn<-length(contrast_result)
              contrast_result2<-data.frame()
              for(multi_by in 1:nnn){
                dff<-as.data.frame(contrast_result[[multi_by]])
                name_con<-names(contrast_result[multi_by])
                dff<-as.data.frame(cbind(name_con,row.names(dff),dff))
                colnames(dff)[c(1,2)]<-c(' Variable ',' Contrast ')
                dff[1,2]<-'Contrast vector'
                contrast_result2<-rbind(contrast_result2,dff)
              }
              results$contrast<-contrast_result2
              output$power_contrast <- renderTable({
                results$contrast
              }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
            }
          }
        }
        
        result1<-results$omnibus
        result2<-results$contrast
        result1<-rbind(colnames(result1),result1)
        result2<-rbind(colnames(result2),result2)
        nn<-max(length(result1),length(result2))
        if(ncol(result1)<nn){
          result1[,(ncol(result1)+1):nn]<-''
        }
        if(ncol(result2)<nn){
          result2[,(ncol(result2)+1):nn]<-''
        }
        colnames(result1)<-paste0('V',seq(1:nn))
        colnames(result2)<-paste0('V',seq(1:nn))
        results$all<-rbind(c(input$design_title,rep('',nn-1)),c('Results for overall F-test',rep('',nn-1)),result1,'',c('Results for specific contrasts',rep('',nn-1)),result2)
      }
      
      if(input$Type=='F-test'){
        output$download_all <- downloadHandler(
          filename = function() {
            paste("Power_F_test_", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
            req(results$omnibus)
            result1<-as.data.frame(results$omnibus)
            result1<-rbind(c(input$design_title,rep('',length(result1)-1)),c('Results for overall F-test',rep('',length(result1)-1)),colnames(result1),result1)
            fwrite(result1,file,row.names = F,col.names = F)
          }
        )
      }else if(input$Type=='t-test'){
        output$download_all <- downloadHandler(
          filename = function() {
            paste("Power_T_test_", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
            req(results$contrast)
            result1<-as.data.frame(results$contrast)
            result1<-rbind(c(input$design_title,rep('',length(result1)-1)),c('Results for t-test',rep('',length(result1)-1)),colnames(result1),result1)
            fwrite(result1,file,row.names = F,col.names = F)
          }
        )
      }else if(input$Type=='F-test & t-test'){
        output$download_all <- downloadHandler(
          filename = function() {
            paste("Power_results_", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
            req(results$all)
            fwrite(results$all,file,row.names = F,col.names = F)
          }
        )
      }
      results_generated(TRUE)
    }, error = function(e) {
      showNotification(
        paste("Power could not be calculated. Review the design assumptions and test settings.", e$message),
        type = "error",
        duration = 8
      )
    }
    )
  })
  
  observeEvent({
    list(
      tryCatch(levels_vec(), error = function(e) NULL),
      tryCatch(levels_num_trt(), error = function(e) NULL),
      tryCatch(input$num_rep, error = function(e) NULL),
      tryCatch(interaction_option_number(), error = function(e) NULL),
      tryCatch(interaction_formula_number(), error = function(e) NULL),
      tryCatch(hot_to_r(input$design_table), error = function(e) NULL),
      tryCatch(hot_to_r(input$design_variance_table), error = function(e) NULL),
      tryCatch(input$Type, error = function(e) NULL),
      tryCatch(input$Type_ss, error = function(e) NULL),
      tryCatch(input$p_value1, error = function(e) NULL),
      tryCatch(input$which_para, error = function(e) NULL),
      tryCatch(input$by_para, error = function(e) NULL),
      tryCatch(input$Contrast, error = function(e) NULL),
      tryCatch(input$custom_contrast, error = function(e) NULL),
      tryCatch(input$alternative, error = function(e) NULL),
      tryCatch(input$p_value2, error = function(e) NULL),
      tryCatch(input$p.adj, error = function(e) NULL),
      tryCatch(levels_num_trt_main(), error = function(e) NULL),
      tryCatch(levels_num_trt_sub(), error = function(e) NULL),
      tryCatch(levels_vec_main(), error = function(e) NULL),
      tryCatch(levels_vec_sub(), error = function(e) NULL),
      tryCatch(input$Formula_general,error=function(e) NULL),
      tryCatch(factor_types_number(),error=function(e) NULL),
      tryCatch(datavalues$uploaded_data,error=function(e) NULL),
      tryCatch(input$cor_type,error=function(e) NULL),
      tryCatch(input$design_title,error=function(e) NULL)
    )
  }, {
    req(page_started())
    stale_results <- results_generated()
    if (stale_results) results_generated(FALSE)
    output$results_display <- renderUI({
      result_empty_state(stale_results)
    })
  })
  
}


shinyApp(ui = ui, server = server)

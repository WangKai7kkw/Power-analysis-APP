#### 
packages <- c('pwr4exp','rhandsontable','shiny','bslib','data.table','shinyBS','bsplus','readxl','rsconnect','nlme')
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

ui <- page_fillable(
  theme = bs_theme(preset = "cosmo"),
  tags$head(
    tags$style(HTML("
    .modal-backdrop.show {
        backdrop-filter: blur(5px); 
        opacity: 0.6 !important;
    }
    .modal-dialog {
        margin-top: 10vh !important;
        width: 50% !important;
        max-width: 700px !important;
    }
  "))
  ),
  uiOutput("main_ui")
)

server<-function(input,output,session) {
  
  page_started <- reactiveVal(FALSE)
  
  ui_main<-function(){
    div(
      tags$style(HTML("
          .card-header h4 {
              text-align: center !important;
             width: 100%;
             font-weight: 700 !important;
            }
         ")),
      div(
        style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
        tags$div(class = "dropdown",
                 tags$button(
                   "About pwr4exp",
                   class = "btn btn-light dropdown-toggle",
                   type = "button",
                   `data-bs-toggle` = "dropdown",
                   `aria-expanded` = "false",
                   style = "
                        font-size:16px;
                        color:#000; 
                        background-color:#fff; 
                        border: 1px solid #ccc;
                        box-shadow: none;
                      "
                 ),
                 tags$ul(
                   class = "dropdown-menu",
                   tags$li(
                     tags$a("Source code",rel="noopener noreferrer", class="dropdown-item", href="https://github.com/an-ethz/pwr4exp", target="_blank")
                   ),
                   tags$li(
                     tags$a("Docs",rel="noopener noreferrer", class="dropdown-item", href="https://an-ethz.github.io/pwr4exp/articles/pwr4exp.html", target="_blank")
                   )
                 )
        )
      ),
      div(
        style = "display: flex; align-items: flex-start; gap: 20px; margin-bottom: 5px; width: 500px;margin-top:-10px;",
        tags$span("Select Design", 
                  style = "font-size:22px; 
                          font-weight:800; 
                          white-space: nowrap;
                          margin-top: 4px;
                          padding-left: 10px;
                          border-left: 4px solid #0074D9;"),
        div(
          style = "flex-grow: 1;margin-top: 0px;",
          selectInput(
            inputId = "design_title",
            label = NULL,
            choices = c(
              "Completely Randomized Design", 
              "Randomized Complete Block Design",
              "Latin Square Design",
              "Split Plot Design",
              "General Design"
            ),
            selected = "Completely Randomized Design",
            width = '100%'
          )
        ),
        tags$style(HTML("
    #design_title + .selectize-control .selectize-input {
      font-size: 18px !important;
      height: 30px;       
      line-height: 30px;
    }
  "))
      ),
      
      div(
        style = "display: flex; gap: 25px; width: 100%;height: 90vh;",
        
        div(
          style = "flex: 0 0 28%; min-width: 200px;display: flex; flex-direction: column; height: 100%;",
          card(
            style = "flex: 1; display: flex; flex-direction: column;",
            card_header(HTML("<h4>Design</h4>")),
            div(
              style = "flex: 1; margin-top: 2px; padding-top: 2px;",
              uiOutput("dynamic_sidebar")
            ),
            full_screen = TRUE
          )
        ),
        
        div(
          style = "flex: 0 0 28%; min-width: 200px;display: flex; flex-direction: column; height: 100%;",
          card(
            style = "flex: 1; display: flex; flex-direction: column;",
            card_header(HTML("<h4>Power Calculation</h4>")),
            div(
              style = "flex: 1; overflow-y: auto; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = 'Type',
                  label = 'Type of test',
                  choices = c('F-test', 't-test', 'F-test & t-test'),
                  selected = 'F-test',
                  width = "100%"
                )
              ),
              uiOutput('test_options_ui')
            ),
            full_screen = TRUE
          )
        ),
        
        div(
          style = "flex: 0 0 44%; min-width: 300px;display: flex; flex-direction: column; height: 100%;",
          card(
            style = "flex: 1; display: flex; flex-direction: column;",
            card_header(HTML("<h4>Power Analysis Results</h4>")),
            div(
              style = "flex: 1;height: 100%; overflow-y: auto;",
              uiOutput("results_display")
            ),
            div(
              style = "flex: 0 0 auto; display: flex; justify-content: center; margin-top: 10px;",
              downloadButton("download_all", "Download Results",
                             class = "btn-primary",
                             style = "width: 100%;")
            ),
            full_screen = TRUE
          )
        )
      ),
      
      tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
    )
  }
  
  output$main_ui <- renderUI({
    ui_main()
  })
  
  observe({
    if (!page_started()) {
      
      showModal(
        modalDialog(
          size = "l",
          easyClose = FALSE,
          fade = TRUE,
          
          div(
            style = "
            width:100%; 
            height:100%; 
            text-align:center;
            padding:20px;
          ",
            
            h2(HTML("Welcome to <b><span style='font-weight:900;'>pwr4exp</span></b> app")),
            
            p(
              "Power Analysis for Research Experiments",
              style='margin-top:20px; font-size:20px;'
            ),
            
            p(
              "Provides tools for calculating statistical power for experiments analyzed using linear mixed models. It supports standard designs, including randomized block, split-plot, and Latin Square designs, while offering flexibility to accommodate a variety of other complex study designs.",
              style="
              max-width:700px;
              margin:auto;
              margin-top:20px;
              font-size:16px;
              line-height:1.5;
            "
            ),
            
            actionButton(
              "start_btn", "Start",
              class = "btn btn-primary",
              style="margin-top:30px; font-size:20px; padding:10px 30px;"
            ),
            
            p(
              "Author: ...",
              style="margin-top:40px; font-size:24px; font-weight:500;"
            )
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
  
  generate_factor_combinations <- function(n){
    letters_vec <- LETTERS[1:n]
    letters_vec<-paste0('fac',letters_vec)
    all_combinations <- unlist(
      lapply(1:n, function(k) {
        combn(letters_vec, k, simplify = FALSE)
      }),
      recursive = FALSE
    )
    result <- sapply(all_combinations, function(x) {
      paste(x, collapse = ":")
    })
    return(result)
  }
  
  filter_combinations <- function(all_combinations, exclude_factors) {
    if (length(exclude_factors) == 0) return(all_combinations)
    exclude_factors <- unlist(strsplit(exclude_factors, "\\:"))
    filtered <- sapply(all_combinations, function(comb) {
      factors <- unlist(strsplit(comb, "\\:"))
      !any(factors %in% exclude_factors)
    })
    c("NULL", all_combinations[filtered])
  }
  
  generate_spd_factors <- function(num_trt_main, num_trt_sub) {
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
          tags$p("Treatments", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
        
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
                numericInput("num_trt", "Number of treatment factors", 
                           value = 1, min = 1,width = '100%')
              )
          ),
          div(style="margin-bottom: 2px;",
            uiOutput("level_number_ui")
          )
        ),
         
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          tags$p("Sample Size", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_rep", "Number of replicates per treatment", 
                           value = 8,min=1,width='100%')
              )
          )
          ),
          
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          tags$p("Model", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
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
          tags$p("Treatments", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
          div(style = "display: flex; align-items: center;width:100%;;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_trt", "Number of treatment factors", 
                           value = 1, min = 1,width = "100%")
              )
          ),
          div(style="margin-bottom: 2px;",
              uiOutput("level_number_ui")
          )
        ),
          
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          tags$p("Sample Size", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_block", "Number of blocks", value = 8,min=1,width="100%")
              )
            )
          ),
          
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          tags$p("Model", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
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
          tags$p("Treatments", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_trt", "Number of treatment factors", 
                           value = 1, min = 1,width = "100%")
              )
          ),
          div(style="margin-bottom: 2px;",
              uiOutput("level_number_ui")
          )
        ),
          
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          tags$p("Sample Size", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_squares", "Number of replicated squares", value = 4,min=1,width = '100%')
              )
          ),
          
          div(
            style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "value_reuse",
                  label = "Reuse blocks",
                  choices = c('row','col','none'),
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
          tags$p("Model", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
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
          tags$p("Treatments", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_trt_main", "Number of main plot factors", 
                           value = 1, min = 1,width="100%")
              )
          ),
          
          div(style="margin-bottom: 2px;",
            uiOutput("level_numbers_main_ui")
          ),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_trt_sub", "Number of sub plot factors",
                           value = 1, min = 1,width="100%")
              )
          ),
          div(style="margin-bottom: 2px;",
            uiOutput("level_numbers_sub_ui")
          )
          ),
          
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          tags$p("Sample Size", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
          div(style = "display: flex; align-items: center;width:100%;margin-bottom: 2px;",
              div(style="flex:1;",
              numericInput("num_rep", "Number of replicates (main plots) per main plot treatment", 
                           value = 10,min=1,width="100%")
              )
          )
          ),
        
        bslib::card(
          style = "background-color: #f8f9fa; border: 1px solid #ddd; 
               border-radius: 6px; padding: 2px; margin-top: 2px;",
          tags$p("Model", 
                 style='font-weight: bold; font-size:20px; color: #333; margin: 0 0 2px 0;'),
          
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
        div(
          style = "display: flex; flex-direction: column;width:100%;",
          
          div(
            "Upload data file (.csv, .xlsx, .xls, .txt, .tsv)",
            style = "font-weight: bold; margin-bottom: 2px;"
          ),
          
          div(
            "Note: A data frame with all independent variables in columns, matching the design's data structure.",
            style = "font-size: 14px; color: #555; margin-bottom: 4px;"
          )
        ),
        
        div(
          style = "display: flex; align-items: center;width:100%;",
          div(
            style='flex:1;',
            fileInput(
              inputId = "uploaded_file",
              label = NULL,
              accept = c(".csv", ".xlsx",".xls", ".txt", ".tsv"),
              buttonLabel = "Browse...",
              placeholder = "No file selected",
              width = "100%")
          )
        ),
          
        uiOutput("file_feedback"),
        
        uiOutput("file_type_check"),
          
        div(style = "font-weight: bold;display: flex; align-items: center;width:100%;",
            div(style="flex:1;",
            textInput("Formula_general", "Input Formula",
                      placeholder = "e.g., ~ fA + fB",width = "100%")
            ),
            tags$span(
              `data-toggle` = "tooltip",
              title = "Note: A right-hand-side formula specifying the model, with terms on the right of ~ , following lme4::lmer syntax for random effects.",
              style = "margin-left: 5px; cursor: pointer;",
              icon("question-circle")
            )
        ),
        
          uiOutput("level_numbers_validation3"),
          
        div(style = "font-weight: bold;display: flex; align-items: center;width:100%;",
            div(style="flex:1;",
            selectInput(
              "cor_type",
              "Residual correlation structure",
              choices = c(
                "None" = "none",
                "corAR1 \u2013 First-order autoregressive" = "corAR1",
                "corARMA \u2013 Autoregressive moving average" = "corARMA",
                "corCAR1 \u2013 Continuous-time autoregressive" = "corCAR1",
                "corCompSymm \u2013 Compound symmetry" = "corCompSymm",
                "corExp \u2013 Exponential spatial correlation" = "corExp",
                "corGaus \u2013 Gaussian spatial correlation" = "corGaus",
                "corLin \u2013 Linear spatial correlation" = "corLin",
                "corSymm \u2013 Unstructured correlation" = "corSymm",
                "corRatio \u2013 Ratio-based spatial correlation" = "corRatio",
                "corSpher \u2013 Spherical spatial correlation" = "corSpher"
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
        uiOutput("cor_params_ui"),
        uiOutput("cor_validation_ui"),
        uiOutput('input_data_ui'),
        uiOutput('input_variance_ui'),
        tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
          
      )
    }
  })
  
  output$level_number_ui <- renderUI({
    req(page_started())
    req(input$num_trt)
    num_inputs <- lapply(seq_len(input$num_trt), function(i) {
      factor_name <- paste0("Factor ", LETTERS[i])
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        numericInput(
          inputId = paste0("factor_", i),
          label = factor_name,
          value = 1,
          min = 1,
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
        value = paste(rep(1, input$num_trt), collapse = ",")
      )
    )
    
    observeEvent(
      lapply(seq_len(input$num_trt), function(i) input[[paste0("factor_", i)]]),
      {
        factor_values <- sapply(seq_len(input$num_trt), function(i) {
          input[[paste0("factor_", i)]] %||% 1
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
      tags$label("Levels of each factor",
                 style = "margin-bottom: 10px; display: block;"),
      div(
        style = "font-size: 12px;font-weight: bold;display: flex; gap: 10px; overflow-x: auto; align-items: flex-end;",
        num_inputs
      ),
      hidden_text_input
    )
  })
  
  output$level_numbers_main_ui <- renderUI({
    req(page_started())
    req(input$num_trt_main)
    
    num_inputs_main <- lapply(seq_len(input$num_trt_main), function(i) {
      factor_name_main <- paste0("Factor ", LETTERS[i])
      
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        numericInput(
          inputId = paste0("factor_main_", i),
          label = factor_name_main,
          value = 1,
          min = 1,
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
        value = paste(rep(1, input$num_trt_main), collapse = ",")
      )
    )
    
    observeEvent(
      lapply(seq_len(input$num_trt_main), function(i) input[[paste0("factor_main_", i)]]),
      {
        factor_values_main <- sapply(seq_len(input$num_trt_main), function(i) {
          input[[paste0("factor_main_", i)]] %||% 1
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
      tags$label("Levels of each main-plot factor",
                 style = "margin-bottom: 10px; display: block;"),
      div(
        style = "font-size: 12px;font-weight: bold;display: flex; gap: 10px; overflow-x: auto; align-items: flex-end;",
        num_inputs_main
      ),
      hidden_text_input_main
    )
  })
  
  output$level_numbers_sub_ui <- renderUI({
    req(page_started())
    req(input$num_trt_sub)
    
    num_inputs_sub <- lapply(seq_len(input$num_trt_sub), function(i) {
      factor_name_sub <- paste0("Factor ", LETTERS[i])
      
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center;",
        numericInput(
          inputId = paste0("factor_sub_", i),
          label = factor_name_sub,
          value = 1,
          min = 1,
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
        value = paste(rep(1, input$num_trt_sub), collapse = ",")
      )
    )
    
    observeEvent(
      lapply(seq_len(input$num_trt_sub), function(i) input[[paste0("factor_sub_", i)]]),
      {
        factor_values_sub <- sapply(seq_len(input$num_trt_sub), function(i) {
          input[[paste0("factor_sub_", i)]] %||% 1
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
      tags$label("Levels of each sub-plot factor",
                 style = "margin-bottom: 10px; display: block;"),
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
    
    file <- input$uploaded_file$datapath
    ext <- tools::file_ext(file)
    
    supported_ext <- c("csv", "xlsx", "xls", "txt","tsv")
    
    if (!(ext %in% supported_ext)) {
      showNotification(
        paste("Unsupported file type:", ext, 
              "Please upload an Excel file (.csv, .xlsx, .xls,.txt, .tsv)."),
        type = "error",
        duration = 6
      )
      return(NULL)
    }
    
    df<-tryCatch({
      if (ext == "csv") {
        read.csv(file, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(file)
      }else if (ext %in% c("txt", "tsv")) {
        read.delim(file, stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      showNotification(
        paste("Failed to read file:", e$message),
        type = "error",
        duration = 6
      )
      return(NULL)
    })
    if (is.null(df) || nrow(df) == 0) {
      showNotification(
        "The uploaded file is empty or could not be read properly.",
        type = "warning", duration = 6
      )
      return(NULL)
    }
    datavalues$uploaded_data <- df
  })
  
  observeEvent(input$design_title, {
    req(page_started())
    if (input$design_title == "General Design") {
      
        output$file_feedback <- renderUI({
          if (is.null(input$uploaded_file)) {
            return(NULL)
          }
          req(input$uploaded_file)
          
          df <- datavalues$uploaded_data
          
          if (is.null(df)) {
            tags$div(style = "color: red; font-weight: bold;margin-bottom: 10px;",
                     "Please upload file.")
          } else {
            tags$div(style = "color: green; font-weight: bold;margin-bottom: 10px;",
                     paste("File successfully uploaded with", nrow(df), "rows and", ncol(df), "columns."))
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
            choices = c("Categorical", "Continuous"),
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
      tags$label("Type of factor",
                 style = "margin-bottom: 10px; display: block;font-weight: bold"),
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
            paste("Warning: variable(s) not found in uploaded data →",
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
    if (input$design_title == "General Design") {
      values$data <- NULL
      values$variance <- NULL
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$design_title, {
    req(page_started())
    if (input$design_title != "General Design") {
      datavalues$uploaded_data <- NULL
    }
  })
  
  output$interaction_exist_ui <- renderUI({
    req(page_started())
    if(!input$design_title%in%c('Split Plot Design','General Design')){
      req(input$num_trt)
      if (is.null(input$num_trt) || input$num_trt == "") {
        return(NULL)
      }
      num_trt_value <- suppressWarnings(as.numeric(input$num_trt))
      if (is.na(num_trt_value) || num_trt_value <= 1) {
        return(NULL)
      }else {
        div(
          style = "flex: 1; padding-top: 2px;width:100%;",
          div(
            style='flex:1;',
            selectInput(
              inputId = 'interaction_option',
              label = 'Model with interaction between factors?',
              choices = c('Yes','No'),
              selected = 'No',
              width = "100%")
          )
        )
      }
    }else if(input$design_title=='Split Plot Design'){
      req(input$num_trt_main)
      req(input$num_trt_sub)
      
      div(
        style = "flex: 1; padding-top: 2px;width:100%;",
        div(
          style='flex:1;',
          selectInput(
            inputId = 'interaction_option',
            label = 'Model with interaction between factors?',
            choices = c('Yes','No'),
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
      req(input$num_trt)
      if(input$num_trt>1){
        fac_names <- paste0("fac", LETTERS[1:input$num_trt])
        all_combinations <- unlist(
          lapply(2:input$num_trt, function(k) {
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
                label = "Interaction Option",
                choices = all_combinations,
                multiple = TRUE,
                selected = NULL,
                options = list(placeholder = 'Select one or more combinations'),
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
      req(input$num_trt_main)
      req(input$num_trt_sub)
      
      if(input$num_trt_main==1){
        fac_names_main<-'trt.main'
      }else if(input$num_trt_main>1){
        fac_names_main <- paste0("fac", LETTERS[1:input$num_trt_main],'.main')
      }
      
      if(input$num_trt_sub==1){
        fac_names_sub<-'trt.sub'
      }else if(input$num_trt_sub>1){
        fac_names_sub <- paste0("fac", LETTERS[1:input$num_trt_sub],'.sub')
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
              label = "Interaction Option",
              choices = all_combinations,
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = 'Select one or more combinations'),
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
          'Formula: ~ ', fac_formula,' + e', '<br>',
          'Error variance: &sigma;<sup>2</sup> (e)'
        )
      } else if(input$design_title=='Randomized Complete Block Design'){
        paste0(
          'Formula: ~ ', fac_formula,' + e', '<br>',
          'Block variance: &sigma;<sup>2</sup> (block)','<br>',
          'Error variance: &sigma;<sup>2</sup> (e)'
        )
      } else if(input$design_title=='Latin Square Design'){
        paste0(
          'Formula: ~ ', fac_formula,' + e', '<br>',
          'Row variance: &sigma;<sup>2</sup> (row)','<br>',
          'Col variance: &sigma;<sup>2</sup> (col)','<br>',
          'Error variance: &sigma;<sup>2</sup> (e)'
        )
      } else if(input$design_title=='Split Plot Design'){
        paste0(
          'Formula: ~ ', fac_formula,' + e', '<br>',
          'Mainplot variance: &sigma;<sup>2</sup> (mainplot)','<br>',
          'Error variance: &sigma;<sup>2</sup> (e)'
        )
      } 
    bslib::card(
      style = "background-color: #f8f9fa; border: 1px solid #ddd; 
             border-radius: 8px; padding: 6px; margin-top: 4px;",
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        
        # 左侧黑色竖条
        div(
          style = "width: 4px; align-self: stretch; background-color: #000; border-radius: 2px;"
        ),
        
        # note_text 内容
        div(
          HTML(paste0(
            '<p style="font-size: 16px; color: #333; margin: 0;">',
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
          "Please select at least one interaction option.",
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
    req(input$num_trt)
    as.numeric(input$num_trt)
  })
  
  levels_num_trt_main<-reactive({
    req(page_started())
    req(input$num_trt_main)
    as.numeric(input$num_trt_main)
  })
  
  levels_num_trt_sub<-reactive({
    req(page_started())
    req(input$num_trt_sub)
    as.numeric(input$num_trt_sub)
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
      req(input$num_trt)
      if(min(levels_vec())==1){
          values$data<-NULL
          values$variance<-NULL
      }else{
        if(levels_num_trt()==1&min(levels_vec())>1){
          crd<-designCRD(
            treatments = levels_vec(),
            replicates = input$num_rep,
            template = T
          )
        }else if(levels_num_trt()>1&interaction_option_number()=='No'){
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd<-designCRD(
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
          
          crd<-designCRD(
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
      req(input$num_trt)
      if(min(levels_vec())==1){
        values$data<-NULL
        values$variance<-NULL
      }else{
        if(levels_num_trt()==1&min(levels_vec())>1){
          crd<-designRCBD(
            treatments = levels_vec(),
            blocks = input$num_block,
            template = T
          )
        }else if(levels_num_trt()>1&interaction_option_number()=='No'){
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd<-designRCBD(
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
          
          crd<-designRCBD(
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
      req(input$num_trt)
      if(min(levels_vec())==1){
        values$data<-NULL
        values$variance<-NULL
      }else{
        if(levels_num_trt()==1&min(levels_vec())>1){
          crd<-designLSD(
            treatments = levels_vec(),
            squares = input$num_squares,
            reuse=input$value_reuse,
            template = T
          )
        }else if(levels_num_trt()>1&interaction_option_number()=='No'){
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd<-designLSD(
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
          
          crd<-designLSD(
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
          
          crd<-designSPD(
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
          
          crd<-designSPD(
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
        
        crd<-mkdesign(
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
      req(input$num_trt)
      note_text <- if (input$num_trt>=2) {
        req(input$interaction_option)
        if(input$interaction_option=='Yes'){
          "Please provide cell means of all combinations."
        } else if (input$interaction_option == "No") {
          "Please provide marginal means of each factor."
        } 
      } else if(input$num_trt<=1) {
        "Please provide marginal means of each factor."
      }

    }else if(input$design_title=='Split Plot Design'){
      req(input$num_trt_main)
      req(input$num_trt_sub)
      req(input$interaction_option)
    
      note_text <- if(input$interaction_option=='Yes'){
          "Please provide cell means of all combinations."
        } else if (input$interaction_option == "No") {
          "Please provide marginal means of each factor."
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
      tags$p("Means",style='font-weight: bold;font-size:20px;color: #333; margin: 0;'),
      tags$p(note_text, style = "font-size: 15px; color: #333; margin: 0;"),
      uiOutput('design_table_ui')
    )
    }else if(input$design_title=='General Design'){
      bslib::card(
        style = "background-color: #f8f9fa; border: 1px solid #ddd; 
                border-radius: 8px; padding: 10px; margin-top: 8px;
                height: 300px; overflow-y: auto;",
        tags$p("Means",style='font-weight: bold;font-size:20px;color: #333; margin: 0;'),
        tags$p("Please provide means.", style = "font-size: 15px; color: #333; margin: 0;"),
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
          tags$em("No data available")
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
          tags$p("Variance",style='font-weight: bold;font-size:20px;color: #333; margin: 0;'),
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
          tags$em("No data available")
        )
      )
    }else{
      if(input$design_title!="General Design"){
        rhandsontable::rHandsontableOutput("design_variance_table")
      }else if(input$design_title=="General Design"){
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

  # Safe formula builder (no eval/parse)
  make_cor_form <- function(rhs, group) {
    if (!is.null(group) && nzchar(group))
      stats::as.formula(paste0("~ ", rhs, " | ", group))
    else
      stats::as.formula(paste0("~ ", rhs))
  }

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

    group_sel <- var_sel("cor_group", "Grouping variable (optional; for ~ … | group)", extra_none = TRUE)

    rho_slider <- sliderInput("cor_rho", "Correlation parameter (\u03c1)",
                              min = -0.99, max = 0.99, value = 0.3, step = 0.01, width = "100%")

    if (input$cor_type %in% c("corAR1", "corCAR1", "corCompSymm")) {
      return(tagList(
        var_sel("cor_time", "Time / order variable"),
        group_sel,
        rho_slider
      ))
    }

    if (input$cor_type == "corARMA") {
      return(tagList(
        var_sel("cor_time", "Time / order variable"),
        group_sel,
        numericInput("cor_p", "AR order p", value = 1, min = 0, step = 1, width = "100%"),
        numericInput("cor_q", "MA order q", value = 0, min = 0, step = 1, width = "100%"),
        uiOutput("cor_arma_params_ui")
      ))
    }

    if (input$cor_type %in% c("corExp", "corGaus", "corLin", "corRatio", "corSpher")) {
      return(tagList(
        selectInput("cor_dim", "Spatial coordinates",
                    choices = c("1D: ~ x" = "1", "2D: ~ x + y" = "2", "3D: ~ x + y + z" = "3"),
                    selected = "1", width = "100%"),
        var_sel("cor_x", "Coordinate x"),
        conditionalPanel("input.cor_dim >= '2'", var_sel("cor_y", "Coordinate y")),
        conditionalPanel("input.cor_dim == '3'", var_sel("cor_z", "Coordinate z")),
        group_sel,
        numericInput("cor_range", "Range parameter", value = 1, min = 1e-6, step = 0.1, width = "100%"),
        checkboxInput("cor_nugget", "Use nugget effect", value = FALSE)
      ))
    }

    if (input$cor_type == "corSymm") {
      return(tagList(
        var_sel("cor_time", "Index variable (defines correlation levels)"),
        group_sel,
        tags$div(
          style = "margin-top:6px; font-size:13px; color:#555;",
          "Enter lower-triangle correlations in the table below (values strictly between -1 and 1, exclusive)."
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
      return(tags$div(style = "color:#d9534f;", "p and q must be non-negative integers."))

    k <- p + q
    if (k == 0)
      return(tags$div(style = "margin-top:6px; color:#555;",
                      "p = 0 and q = 0: no ARMA parameters required."))

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
                      "Need at least 2 levels in the index variable."))
    if (m > 15)
      return(tags$div(style = "color:#d9534f; margin-top:6px;",
                      "Too many levels (> 15) for manual unstructured input. Consider another structure."))

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
        msgs <- c(msgs, "Please select a time/index variable.")
      else if (!(t %in% cols))
        msgs <- c(msgs, paste0("Time/index variable not found in data: ", t))
    }

    # Spatial coordinate checks
    if (input$cor_type %in% c("corExp", "corGaus", "corLin", "corRatio", "corSpher")) {
      x <- tryCatch(input$cor_x, error = function(e) NULL)
      if (is.null(x) || !(x %in% cols))
        msgs <- c(msgs, "Coordinate x not found in data.")
      dim_val <- tryCatch(input$cor_dim, error = function(e) "1")
      if (!is.null(dim_val) && dim_val >= "2") {
        y <- tryCatch(input$cor_y, error = function(e) NULL)
        if (is.null(y) || !(y %in% cols))
          msgs <- c(msgs, "Coordinate y not found in data.")
      }
      if (!is.null(dim_val) && dim_val == "3") {
        z <- tryCatch(input$cor_z, error = function(e) NULL)
        if (is.null(z) || !(z %in% cols))
          msgs <- c(msgs, "Coordinate z not found in data.")
      }
    }

    # Grouping variable (optional)
    grp <- tryCatch(input$cor_group, error = function(e) NULL)
    if (!is.null(grp) && nzchar(grp) && !(grp %in% cols))
      msgs <- c(msgs, paste0("Grouping variable not found in data: ", grp))

    # ARMA: p+q check
    if (input$cor_type == "corARMA") {
      p <- tryCatch(as.integer(input$cor_p), error = function(e) NA_integer_)
      q <- tryCatch(as.integer(input$cor_q), error = function(e) NA_integer_)
      if (is.na(p) || is.na(q) || p < 0 || q < 0)
        msgs <- c(msgs, "p and q must be non-negative integers.")
      else if (p == 0 && q == 0)
        msgs <- c(msgs, "At least one of p or q must be > 0 for corARMA.")
    }

    if (length(msgs) > 0)
      div(style = "color:#d9534f; margin-top:4px; font-size:13px;", paste(msgs, collapse = "  "))
    else
      div(style = "color:#28a745; margin-top:4px; font-size:13px;", "\u2713 Correlation settings look valid.")
  })

  # ── Build correlation object from safe whitelist ────────────────────────────
  build_correlation <- function(df, type) {
    if (is.null(type) || type == "none") return(NULL)

    grp <- tryCatch(input$cor_group, error = function(e) "")
    if (is.null(grp)) grp <- ""

    # ── Time-series / non-spatial types ──────────────────────────────────────
    if (type %in% c("corAR1", "corARMA", "corCAR1", "corCompSymm", "corSymm")) {
      cov <- input$cor_time
      df_model <- df
      # Support factor time variable for AR1/ARMA by mapping to integer index
      if (type %in% c("corAR1", "corARMA")) {
        x <- df_model[[cov]]
        if (!is.numeric(x)) {
          df_model[[".cor_time_index"]] <- as.integer(factor(x))
          time_rhs <- ".cor_time_index"
        } else {
          time_rhs <- cov
        }
      } else {
        time_rhs <- cov
      }

      form <- make_cor_form(time_rhs, grp)

      if (type == "corAR1") {
        return(list(df_model = df_model,
                    cor = nlme::corAR1(value = input$cor_rho, form = form)))
      }
      if (type == "corCAR1") {
        return(list(df_model = df_model,
                    cor = nlme::corCAR1(value = input$cor_rho, form = form)))
      }
      if (type == "corCompSymm") {
        return(list(df_model = df_model,
                    cor = nlme::corCompSymm(value = input$cor_rho, form = form)))
      }
      if (type == "corARMA") {
        p <- as.integer(input$cor_p)
        q <- as.integer(input$cor_q)
        k <- p + q
        vals <- if (k == 0) numeric(0) else {
          sapply(seq_len(k), function(i) {
            v <- input[[paste0("cor_arma_", i)]]
            if (is.null(v)) 0 else as.numeric(v)
          })
        }
        return(list(df_model = df_model,
                    cor = nlme::corARMA(value = vals, p = p, q = q, form = form)))
      }
      if (type == "corSymm") {
        # Read lower-triangle from rhandsontable
        tbl <- tryCatch(rhandsontable::hot_to_r(input$cor_symm_table),
                        error = function(e) NULL)
        if (is.null(tbl))
          stop("corSymm table not yet filled.")

        lev <- unique(df_model[[cov]])
        lev <- as.character(lev[!is.na(lev)])
        m   <- length(lev)

        mat <- matrix(0, nrow = m, ncol = m)
        diag(mat) <- 1
        for (r in seq_len(m)) {
          for (c in seq_len(m)) {
            if (r > c) {
              val <- suppressWarnings(as.numeric(as.character(tbl[r, c])))
              if (is.na(val) || abs(val) >= 1)
                stop(paste0("Invalid correlation at (", lev[r], ", ", lev[c], "): must be strictly between -1 and 1."))
              mat[r, c] <- val
              mat[c, r] <- val
            }
          }
        }
        # Positive-definiteness check
        eigs <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
        if (any(eigs <= 0))
          stop("The specified correlation matrix is not positive definite.")

        # nlme::corSymm expects a vector of the lower triangle (col-major, below diagonal)
        value_vec <- mat[lower.tri(mat)]

        return(list(df_model = df_model,
                    cor = nlme::corSymm(value = value_vec, form = form)))
      }
    }

    # ── Spatial types ─────────────────────────────────────────────────────────
    if (type %in% c("corExp", "corGaus", "corLin", "corRatio", "corSpher")) {
      rng <- as.numeric(input$cor_range)
      nug <- isTRUE(input$cor_nugget)
      dim_val <- tryCatch(input$cor_dim, error = function(e) "1")
      if (is.null(dim_val)) dim_val <- "1"

      rhs <- if (dim_val == "3") {
        paste0(input$cor_x, " + ", input$cor_y, " + ", input$cor_z)
      } else if (dim_val == "2") {
        paste0(input$cor_x, " + ", input$cor_y)
      } else {
        input$cor_x
      }

      form <- make_cor_form(rhs, grp)

      cor_fun <- switch(type,
        corExp   = nlme::corExp,
        corGaus  = nlme::corGaus,
        corLin   = nlme::corLin,
        corRatio = nlme::corRatio,
        corSpher = nlme::corSpher
      )
      return(list(df_model = df, cor = cor_fun(value = rng, form = form, nugget = nug)))
    }

    stop(paste0("Unsupported correlation type: ", type))
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
              label = "Type of sum of square",
              choices = c('Type I','Type II','Type III'),
              selected = "Type III",
              width = '100%'),
            sliderInput("p_value1",'Significance level',min=0,max=0.2,value=0.05,step=0.005,width = "100%"),
            actionButton('create_result','Power Calculation',
                         class = "btn-primary",
                         style = "width: 100%;",width = "100%")
          )
        )
      )
    }else if(input$Type=='t-test'){
      if(!input$design_title%in%c("Split Plot Design",'General Design')){
        req(input$num_trt,input$level_numbers)
        
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
          if(input$num_trt>1){
            selected_which <- if (is.null(input$which_para)) "facA" else input$which_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "which_para",
                  label = "The factor of interest",
                  choices = generate_factor_combinations(input$num_trt),
                  selected = selected_which,
                  width = "100%")
              )
            )
          },
          if(input$num_trt>1){
            selected_by <- if (is.null(input$by_para)) "NULL" else input$by_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "by_para",
                  label='The variable to condition on',
                  choices = filter_combinations(generate_factor_combinations(input$num_trt), input$which_para),
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
                label = "Contrast Method",
                choices = contrast_choices,
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
                              "Contrast vector",
                              placeholder = "e.g., 1,-1",
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
                label = "Alternative",
                choices = c('one.sided','two.sided'),
                selected = "two.sided",
                width = "100%"),
              sliderInput("p_value2",'Significance level',min=0,max=0.2,value=0.05,step=0.005,width = "100%"),
              checkboxInput("p.adj", HTML("P-value adjustment ( Bonferroni <i>t</i> )"), value = F,width = "100%"),
              actionButton('create_result','Power Calculation',
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
                  label = "The factor of interest",
                  choices = generate_spd_factors(num_trt_main,num_trt_sub),
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
                  label='The variable to condition on',
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
                label = "Contrast Method",
                choices = contrast_choices,
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
                              "Contrast vector",
                              placeholder = "e.g., 1,-1",
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
                label = "Alternative",
                choices = c('one.sided','two.sided'),
                selected = "two.sided",
                width = "100%"),
              sliderInput("p_value2",'Significance level',min=0,max=0.2,value=0.05,step=0.005,width = "100%"),
              checkboxInput("p.adj", HTML("P-value adjustment ( Bonferroni <i>t</i> )"), value = F),
              actionButton('create_result','Power Calculation',
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
                textInput("which_para", "The factor of interest",
                          placeholder = "e.g., fA or fA:fB",width = "100%")
              )
          ),
          tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
          uiOutput("which_validation"),
          
          div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                textInput("by_para", "The variable to condition on",
                          value = "",
                          placeholder = "e.g., fB",width = "100%")
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
                              "Contrast vector",
                              placeholder = "e.g., 1,-1",
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
                label = "Alternative",
                choices = c('one.sided','two.sided'),
                selected = "two.sided",width = "100%"),
              sliderInput("p_value2",'Significance level',min=0,max=0.2,value=0.05,step=0.005,width = "100%"),
              checkboxInput("p.adj", HTML("P-value adjustment ( Bonferroni <i>t</i> )"), value = F,width = "100%"),
              actionButton('create_result','Power Calculation',
                           class = "btn-primary",
                           style = "width: 100%;",width = "100%")
            )
          )
        )
      }
    }else if(input$Type=='F-test & t-test'){
      if(!input$design_title%in%c('Split Plot Design','General Design')){
        req(input$num_trt, input$level_numbers)
        
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
                label = "Type of sum of square",
                choices = c('Type I','Type II','Type III'),
                selected = "Type III",
                width = "100%"),
              sliderInput("p_value1",'Significance level',min=0,max=0.2,value=0.05,step=0.005,width = "100%")
              )
            ),
          if(input$num_trt>1){
            selected_which <- if (is.null(input$which_para)) "facA" else input$which_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "which_para",
                  label = "The factor of interest",
                  choices = generate_factor_combinations(input$num_trt),
                  selected = selected_which,width = "100%")
              )
            )
          },
          if(input$num_trt>1){
            selected_by <- if (is.null(input$by_para)) "NULL" else input$by_para
            div(
              style = "flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                selectInput(
                  inputId = "by_para",
                  label='The variable to condition on',
                  choices = filter_combinations(generate_factor_combinations(input$num_trt), input$which_para),
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
                label = "Contrast Method",
                choices = contrast_choices,
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
                              "Contrast vector",
                              placeholder = "e.g., 1,-1",
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
                label = "Alternative",
                choices = c('one.sided','two.sided'),
                selected = "two.sided",
                width = "100%"),
              checkboxInput("p.adj", HTML("P-value adjustment ( Bonferroni <i>t</i> )"), value = F,width = "100%"),
              actionButton('create_result','Power Calculation',
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
                label = "Type of sum of square",
                choices = c('Type I','Type II','Type III'),
                selected = "Type III",
                width = "100%"),
              sliderInput("p_value1",'Significance level',min=0,max=0.2,value=0.05,step=0.005,width = "100%")
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
                  label = "The factor of interest",
                  choices = generate_spd_factors(num_trt_main,num_trt_sub),
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
                  label='The variable to condition on',
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
                label = "Contrast Method",
                choices = contrast_choices,
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
                              "Contrast vector",
                              placeholder = "e.g., 1,-1",
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
                label = "Alternative",
                choices = c('one.sided','two.sided'),
                selected = "two.sided",
                width = "100%"),
              checkboxInput("p.adj", HTML("P-value adjustment ( Bonferroni <i>t</i> )"), value = F,width = "100%"),
              actionButton('create_result','Power Calculation',
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
                label = "Type of sum of square",
                choices = c('Type I','Type II','Type III'),
                selected = "Type III",
                width = "100%"),
              sliderInput("p_value1",'Significance level',min=0,max=0.2,value=0.05,step=0.005,width = "100%")
              )
            ),
          
          div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                textInput("which_para", "The factor of interest",
                          placeholder = "e.g., fA or fA:fB",width = "100%")
              )
          ),
          tags$script(HTML('$(document).ready(function(){$("[data-toggle=\'tooltip\']").tooltip();});')),
          uiOutput("which_validation"),
          
          div(style = "display: flex; align-items: center;flex: 1; padding-top: 2px;width:100%;",
              div(
                style='flex:1;',
                textInput("by_para", "The variable to condition on",
                          value = "",
                          placeholder = "e.g., fB",width = "100%")
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
                              "Contrast vector",
                              placeholder = "e.g., 1,-1",
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
                label = "Alternative",
                choices = c('one.sided','two.sided'),
                selected = "two.sided",
                width = "100%"),
              checkboxInput("p.adj", HTML("P-value adjustment ( Bonferroni <i>t</i> )"), value = F,width = "100%"),
              actionButton('create_result','Power Calculation',
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
      msgs <- c(msgs, paste0("Variable(s) not found in uploaded data → ", 
                             paste(missing_cols, collapse = ", ")))
    }
    if (length(duplicated_factors) > 0) {
      msgs <- c(msgs, paste0("Variable(s) already used in 'The factor of interest' → ",
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
          label = "Contrast Method",
          choices = contrast_choices(),
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
                      "Invalid input. Please enter comma-separated numbers only."))
    }
    
    if (sum(vec) != 0) {
      return(tags$div(style = "color: red;", 
                      paste0("The sum of all contrast coefficients must equal 0. (Current sum = ", sum(vec), ")")))
    }
    
    level_nums <- tryCatch({
      as.numeric(unlist(strsplit(input$level_numbers, ",")))
    }, error = function(e) return(NULL))
    
    if (is.null(level_nums) || any(is.na(level_nums))) {
      return(tags$div(style = "color: red;", 
                      "Invalid 'level_numbers' input. Please enter numeric levels separated by commas."))
    }
    
    expected_length <- NA 
    
    if(input$design_title!='Split Plot Design'){
      if (input$num_trt == 1) {
        expected_length <- level_nums[1]
      } else {  
        factors <- paste0("fac", LETTERS[1:input$num_trt]) 
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
                      paste0("The number of coefficients (", length(vec),
                             ") does not match the expected number (", expected_length, 
                             ") for factor '", input$which_para, "'.")
      ))
    }
    
    tags$div(style = "color: green;", "Contrast vector input is valid.")
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
                      "Invalid input. Please enter comma-separated numbers only."))
    }
    
    if (sum(vec) != 0) {
      return(tags$div(style = "color: red;", 
                      paste0("The sum of all contrast coefficients must equal 0. (Current sum = ", sum(vec), ")")))
    }
    
    factors <- unlist(strsplit(input$which_para, "\\:"))
    factors <- trimws(factors)
    
    missing_cols <- setdiff(factors, colnames(df))
    if (length(missing_cols) > 0) {
      return(tags$div(style = "color: red;", 
                      paste("Variable(s) not found in uploaded data →", 
                            paste(missing_cols, collapse = ", "))))
    }
    
    level_counts <- sapply(factors, function(fac) length(unique(df[[fac]])))
    expected_length <- prod(level_counts)
    
    if (length(vec) != expected_length) {
      return(tags$div(style = "color: red;", 
                      paste0("The number of coefficients (", length(vec),
                             ") does not match the expected number (", expected_length, 
                             ") for factor '", input$which_para, "'.")
      ))
    }
    
    tags$div(style = "color: green;", "Contrast vector input is valid.")
  })
  
  observeEvent(input$which_para, {
    req(page_started())
    updateSelectInput(session=getDefaultReactiveDomain(), "by_para",
                      choices = filter_combinations(generate_factor_combinations(input$num_trt), input$which_para),
                      selected = if (input$by_para %in% filter_combinations(generate_factor_combinations(input$num_trt), input$which_para)) {
                        input$by_para
                      } else {
                        "NULL"
                      }
    )
  })
  
  output$level_numbers_validation3 <- renderUI({
    req(page_started())
    req(datavalues$uploaded_data)
    req(input$Formula_general)
    
    df <- datavalues$uploaded_data
    formula_text <- input$Formula_general
    
    if (!grepl("^~", formula_text)) {
      return(div(style = "color: #d9534f; margin-top: 4px;",
                 "Error: Formula must start with '~'. Example: ~fA or ~fA:fB"))
    }
    
    vars <- unlist(regmatches(formula_text, gregexpr("[A-Za-z0-9_]+", formula_text)))
    vars <- vars[!vars %in% c("", "1")]
    
    missing_vars <- setdiff(vars, colnames(df))
    
    if (length(missing_vars) > 0) {
      return(div(style = "color: #d9534f; margin-top: 4px;",
                 paste0("Warning: The following variable(s) not found in uploaded data → ",
                        paste(missing_vars, collapse = ", "))))
    }
    
    return(div(style = "color: #28a745; margin-top: 4px;",
               "Validation complete: all variables exist in the input data."))
  })
  
  observeEvent(input$create_result,{
    req(page_started())
    tryCatch({
    output$results_display <- renderUI({
      req(input$Type)
      table_style <- "display: flex; justify-content: center; align-items: center; 
                  padding: 20px; margin: 15px auto; width: 90%; 
                  background-color: #f9f9f9; border-radius: 10px; 
                  box-shadow: 0 2px 6px rgba(0,0,0,0.1);"
      
      if (input$Type == "F-test") {
        return(
          div(style = "display: flex; justify-content: center;",
              div(style = table_style,
                  tableOutput("power_omnibus_test"))
          )
        )
      } else if (input$Type == "t-test") {
        return(
          div(style = "display: flex; justify-content: center;",
              div(style = table_style,
                  tableOutput("power_contrast"))
          )
        )
      } else if (input$Type == "F-test & t-test") {
        return(
          div(style = "display: flex; flex-direction: column; align-items: center;",
              div(style = table_style,
                  tableOutput("power_omnibus_test")),
              div(style = table_style,
                  tableOutput("power_contrast"))
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
        showNotification("Please fill in all cells in both mean and variance tables before calculating results.",
                         type = "error", duration = 5)
        return(NULL)
      }else{
      
      if(levels_num_trt()==1){
        crd <- designCRD(
          treatments = levels_vec(),
          replicates = input$num_rep,
          means = values_mean,
          sigma2 = values_variance
        )
      }else if(levels_num_trt()>1&interaction_option_number()=='No'){
        fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
        fac_formula <- paste(fac_names, collapse = "+")
        crd <- designCRD(
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
        crd <- designCRD(
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
        showNotification("Please fill in all cells in both mean and variance tables before calculating results.",
                         type = "error", duration = 5)
        return(NULL)
      }else{
        if(levels_num_trt()==1){
          crd <- designRCBD(
            treatments = levels_vec(),
            blocks = input$num_block,
            means = values_mean,
            vcomp = values_vcomp,
            sigma2 = values_sigma2
          )
        }else if(levels_num_trt()>1&interaction_option_number()=='No'){
          fac_names <- paste0("fac", LETTERS[1:levels_num_trt()])
          fac_formula <- paste(fac_names, collapse = "+")
          
          crd <- designRCBD(
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
          crd <- designRCBD(
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
        showNotification("Please fill in all cells in both mean and variance tables before calculating results.",
                         type = "error", duration = 5)
        return(NULL)
      }else{
        if(levels_num_trt()==1){
          crd <- designLSD(
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
          crd <- designLSD(
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
          crd <- designLSD(
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
        showNotification("Please fill in all cells in both mean and variance tables before calculating results.",
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
        
        crd<-designSPD(
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
      formula_general<-as.formula(input$Formula_general)

      # Build correlation object safely (no eval/parse)
      cor_result <- tryCatch(
        build_correlation(df, input$cor_type),
        error = function(e) {
          showNotification(paste("Correlation error:", conditionMessage(e)), type = "error")
          NULL
        }
      )
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

      crd <- do.call(mkdesign, mk_args)
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
      results$omnibus<-as.data.frame(pwr.anova(crd,sig.level = pvalue,type = typess))
      omnibus_result<-as.data.frame(cbind(row.names(results$omnibus),results$omnibus))
      colnames(omnibus_result)[1]<-'F-test'
      results$omnibus<-omnibus_result
      output$power_omnibus_test <- renderTable({
        results$omnibus
      }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
    }else if(input$Type=='t-test'){
      results <- reactiveValues(
        contrast = NULL
      )
      if((!input$design_title%in%c('Split Plot Design','General Design')&&input$num_trt==1)){
        if(input$Contrast!='Contrast vector'){
          results$contrast<-pwr.contrast(crd, 
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
          num_custom_contrast<-input$custom_contrast
          numbers_custom_contrast<-unlist(strsplit(num_custom_contrast, ","))
          numbers_custom_contrast<-trimws(numbers_custom_contrast)
          numbers_custom_contrast<-numbers_custom_contrast[numbers_custom_contrast!=""]
          numbers_custom_contrast<-as.numeric(numbers_custom_contrast)
          
          results$contrast<-pwr.contrast(crd, 
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
            results$contrast<-pwr.contrast(crd, 
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
            num_custom_contrast<-input$custom_contrast
            numbers_custom_contrast<-unlist(strsplit(num_custom_contrast, ","))
            numbers_custom_contrast<-trimws(numbers_custom_contrast)
            numbers_custom_contrast<-numbers_custom_contrast[numbers_custom_contrast!=""]
            numbers_custom_contrast<-as.numeric(numbers_custom_contrast)
            
            results$contrast<-pwr.contrast(crd, 
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
            results$contrast<-pwr.contrast(crd, 
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
            num_custom_contrast<-input$custom_contrast
            numbers_custom_contrast<-unlist(strsplit(num_custom_contrast, ","))
            numbers_custom_contrast<-trimws(numbers_custom_contrast)
            numbers_custom_contrast<-numbers_custom_contrast[numbers_custom_contrast!=""]
            numbers_custom_contrast<-as.numeric(numbers_custom_contrast)
            
            results$contrast<-pwr.contrast(crd, 
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
      results$omnibus<-as.data.frame(pwr.anova(crd,sig.level = pvalue,type = typess))
      omnibus_result<-as.data.frame(cbind(row.names(results$omnibus),results$omnibus))
      colnames(omnibus_result)[1]<-'F-test'
      results$omnibus<-omnibus_result
      output$power_omnibus_test <- renderTable({
        results$omnibus
      }, striped = TRUE, hover = TRUE, width = "100%", align = 'c')
      
      if((!input$design_title%in%c('Split Plot Design','General Design')&&input$num_trt==1)){
        if(input$Contrast!='Contrast vector'){
          results$contrast<-pwr.contrast(crd, 
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
          num_custom_contrast<-input$custom_contrast
          numbers_custom_contrast<-unlist(strsplit(num_custom_contrast, ","))
          numbers_custom_contrast<-trimws(numbers_custom_contrast)
          numbers_custom_contrast<-numbers_custom_contrast[numbers_custom_contrast!=""]
          numbers_custom_contrast<-as.numeric(numbers_custom_contrast)
          
          results$contrast<-pwr.contrast(crd, 
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
            results$contrast<-pwr.contrast(crd, 
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
            num_custom_contrast<-input$custom_contrast
            numbers_custom_contrast<-unlist(strsplit(num_custom_contrast, ","))
            numbers_custom_contrast<-trimws(numbers_custom_contrast)
            numbers_custom_contrast<-numbers_custom_contrast[numbers_custom_contrast!=""]
            numbers_custom_contrast<-as.numeric(numbers_custom_contrast)
            
            results$contrast<-pwr.contrast(crd, 
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
            results$contrast<-pwr.contrast(crd, 
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
            num_custom_contrast<-input$custom_contrast
            numbers_custom_contrast<-unlist(strsplit(num_custom_contrast, ","))
            numbers_custom_contrast<-trimws(numbers_custom_contrast)
            numbers_custom_contrast<-numbers_custom_contrast[numbers_custom_contrast!=""]
            numbers_custom_contrast<-as.numeric(numbers_custom_contrast)
            
            results$contrast<-pwr.contrast(crd, 
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
  }, error = function(e) {
    showNotification(
      paste("An error occurred:", e$message),
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
    output$results_display <- renderUI({
      div(
        style = "text-align:center; padding: 40px; color: #777;",
        "Please click 'Power Calculation' to generate results."
      )
    })
  })

}


shinyApp(ui = ui, server = server)


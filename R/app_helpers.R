MAX_TREATMENT_FACTORS <- 8L
MAX_INTERACTION_TERMS <- 2^MAX_TREATMENT_FACTORS - 1L
SUPPORTED_UPLOAD_EXTENSIONS <- c("csv", "xlsx", "xls", "txt", "tsv")

value_or_default <- function(value, default) {
  if (is.null(value) || length(value) == 0L) default else value
}

validate_factor_count <- function(n, max_factors = MAX_TREATMENT_FACTORS) {
  message <- sprintf(
    "Choose a whole number of treatment factors; it must be an integer between 1 and %d.",
    max_factors
  )
  if (length(n) != 1L) {
    stop(message, call. = FALSE)
  }

  numeric_n <- suppressWarnings(as.numeric(as.character(n)))
  if (
    length(numeric_n) != 1L || is.na(numeric_n) || !is.finite(numeric_n) ||
    numeric_n != floor(numeric_n) || numeric_n < 1 || numeric_n > max_factors
  ) {
    stop(message, call. = FALSE)
  }
  as.integer(numeric_n)
}

generate_factor_combinations_safe <- function(
  n,
  prefix = "fac",
  max_terms = MAX_INTERACTION_TERMS
) {
  n <- validate_factor_count(n)
  factors <- paste0(prefix, LETTERS[seq_len(n)])
  term_count <- 2^n - 1
  if (term_count > max_terms) {
    stop(
      sprintf(
        "This selection would generate %d factor terms; the limit is %d.",
        term_count,
        max_terms
      ),
      call. = FALSE
    )
  }

  unlist(
    lapply(seq_len(n), function(k) {
      vapply(
        combn(factors, k, simplify = FALSE),
        paste,
        collapse = ":",
        FUN.VALUE = character(1)
      )
    }),
    use.names = FALSE
  )
}

uploaded_file_extension <- function(upload) {
  if (is.null(upload) || is.null(upload$name) || !nzchar(upload$name)) {
    stop("Choose a study-data file before continuing.", call. = FALSE)
  }
  tolower(tools::file_ext(upload$name))
}

read_uploaded_data <- function(upload) {
  if (is.null(upload$datapath) || !file.exists(upload$datapath)) {
    stop("The uploaded file is no longer available. Choose the file again.", call. = FALSE)
  }

  extension <- uploaded_file_extension(upload)
  if (!(extension %in% SUPPORTED_UPLOAD_EXTENSIONS)) {
    stop(
      sprintf(
        "Unsupported file type: .%s. Choose one of these supported formats: %s.",
        extension,
        paste(SUPPORTED_UPLOAD_EXTENSIONS, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data <- switch(
    extension,
    csv = utils::read.csv(upload$datapath, stringsAsFactors = FALSE),
    xlsx = readxl::read_excel(upload$datapath),
    xls = readxl::read_excel(upload$datapath),
    txt = utils::read.delim(upload$datapath, stringsAsFactors = FALSE),
    tsv = utils::read.delim(upload$datapath, stringsAsFactors = FALSE)
  )

  data <- as.data.frame(data)
  if (nrow(data) == 0L) {
    stop("The uploaded file is empty. Add a header row and at least one observation, then upload it again.", call. = FALSE)
  }
  data
}

parse_manual_column_names <- function(value) {
  if (is.null(value) || !nzchar(trimws(value))) {
    stop("Enter at least one column name.", call. = FALSE)
  }

  columns <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  if (any(!nzchar(columns))) {
    stop("Every column needs a name; remove extra commas or fill in the missing name.", call. = FALSE)
  }
  if (anyDuplicated(columns)) {
    duplicated_names <- unique(columns[duplicated(columns)])
    stop(
      sprintf("Column names must be unique. Duplicated: %s.", paste(duplicated_names, collapse = ", ")),
      call. = FALSE
    )
  }

  columns
}

new_manual_design_table <- function(column_names, row_count, max_rows = 1000L) {
  columns <- parse_manual_column_names(column_names)
  rows <- suppressWarnings(as.numeric(as.character(row_count)))
  if (
    length(rows) != 1L || is.na(rows) || !is.finite(rows) ||
    rows != floor(rows) || rows < 1L || rows > max_rows
  ) {
    stop(
      sprintf("Choose a whole number of rows between 1 and %d.", max_rows),
      call. = FALSE
    )
  }

  table <- as.data.frame(
    matrix("", nrow = as.integer(rows), ncol = length(columns)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(table) <- columns
  table
}

manual_design_data_error <- function(data) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) < 1L || ncol(data) < 1L) {
    return("Create a table with at least one row and one column.")
  }
  if (any(!nzchar(trimws(names(data)))) || anyDuplicated(names(data))) {
    return("Use a unique, non-empty name for every column.")
  }

  empty_cells <- vapply(
    data,
    function(column) is.na(column) | !nzchar(trimws(as.character(column))),
    logical(nrow(data))
  )
  if (any(empty_cells)) {
    return("Fill every cell or remove unused rows before continuing.")
  }

  NULL
}

parse_custom_contrast <- function(value, expected_length = NULL, tolerance = 1e-10) {
  if (is.null(value) || !nzchar(trimws(value))) {
    stop("Enter contrast coefficients separated by commas, such as 1, -1.", call. = FALSE)
  }

  fields <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  if (any(!nzchar(fields))) {
    stop("Every contrast coefficient needs a value; remove extra commas or fill the empty coefficient.", call. = FALSE)
  }

  coefficients <- suppressWarnings(as.numeric(fields))
  if (anyNA(coefficients) || any(!is.finite(coefficients))) {
    stop("Contrast coefficients must all be finite numbers separated by commas.", call. = FALSE)
  }
  if (!is.null(expected_length) && length(coefficients) != expected_length) {
    stop(
      sprintf(
        "The contrast has %d coefficients; %d are required for the selected effect.",
        length(coefficients),
        expected_length
      ),
      call. = FALSE
    )
  }
  if (abs(sum(coefficients)) > tolerance) {
    stop("Contrast coefficients must sum to zero. Adjust the values and try again.", call. = FALSE)
  }
  coefficients
}

validate_model_formula <- function(value, data) {
  if (is.null(value) || !nzchar(trimws(value))) {
    stop("Enter a model formula using columns from the design data.", call. = FALSE)
  }
  if (!grepl("^\\s*~", value)) {
    stop("The model formula must start with '~', for example ~ treatment + (1 | block).", call. = FALSE)
  }

  formula <- tryCatch(
    stats::as.formula(value),
    error = function(error) {
      stop(sprintf("Invalid model formula: %s", conditionMessage(error)), call. = FALSE)
    }
  )
  missing_variables <- setdiff(all.vars(formula), names(data))
  if (length(missing_variables)) {
    stop(
      sprintf(
        "These variables were not found in the design data: %s. Check spelling and capitalization.",
        paste(missing_variables, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  formula
}

normalise_by_factor <- function(by) {
  if (is.null(by) || length(by) == 0L || !nzchar(by) || identical(by, "NULL")) {
    return(NULL)
  }
  by
}

parse_treatment_labels <- function(value) {
  if (is.null(value) || !nzchar(trimws(value))) return(character(0))
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

generated_treatment_level_names <- function(internal, level_count) {
  level_count <- suppressWarnings(as.integer(level_count))
  if (is.na(level_count) || level_count < 1L) return(character(0))
  paste0(internal, seq_len(level_count))
}

reconcile_treatment_level_names <- function(value, internal, level_count) {
  defaults <- generated_treatment_level_names(internal, level_count)
  if (is.null(value)) return(defaults)
  current <- parse_treatment_labels(value)
  if (!length(current)) return(character(0))

  generated_level_pattern <- "^(trt|fac[A-Z])(\\.(main|sub))?[0-9]+$"
  if (all(grepl(generated_level_pattern, current))) {
    return(defaults)
  }

  # Customized names survive a reduction in the configured level count. The UI
  # explains that the extra names remain available and asks the user to either
  # restore the level count or remove them deliberately.
  if (length(current) >= length(defaults)) return(current)
  c(current, defaults[seq.int(length(current) + 1L, length(defaults))])
}

treatment_factor_validation_messages <- function(factor, spec = list()) {
  messages <- character(0)
  factor_name <- trimws(value_or_default(factor$name, ""))
  level_count <- suppressWarnings(as.numeric(factor$level_count))

  if (!nzchar(factor_name)) {
    messages <- c(messages, "Enter a factor name.")
  } else if (grepl(",", factor_name, fixed = TRUE)) {
    messages <- c(messages, "Factor names cannot contain commas.")
  } else if (length(spec)) {
    all_names <- tolower(trimws(vapply(spec, `[[`, character(1), "name")))
    if (sum(all_names == tolower(factor_name)) > 1L) {
      messages <- c(messages, "Factor names must be unique.")
    }
  }

  if (
    length(level_count) != 1L || is.na(level_count) || !is.finite(level_count) ||
      level_count != floor(level_count) || level_count < 2L
  ) {
    messages <- c(messages, "Number of levels must be a whole number of 2 or greater.")
    return(unique(messages))
  }

  entered <- length(factor$levels)
  if (entered < level_count) {
    messages <- c(messages, sprintf(
      "Add %d more level %s.",
      level_count - entered,
      if (level_count - entered == 1L) "name" else "names"
    ))
  } else if (entered > level_count) {
    messages <- c(messages, sprintf(
      "%d extra level %s retained. Restore the level count or remove %s deliberately.",
      entered - level_count,
      if (entered - level_count == 1L) "name is" else "names are",
      if (entered - level_count == 1L) "it" else "them"
    ))
  }
  if (any(!nzchar(factor$levels))) {
    messages <- c(messages, "Every configured level needs a name.")
  }
  if (anyDuplicated(tolower(factor$levels))) {
    messages <- c(messages, "Level names must be unique within this factor.")
  }
  unique(messages)
}

build_treatment_factor_spec <- function(internal, display_name, level_labels, level_count) {
  list(
    internal = internal,
    name = trimws(value_or_default(display_name, "")),
    levels = parse_treatment_labels(level_labels),
    defaults = generated_treatment_level_names(internal, level_count),
    level_count = suppressWarnings(as.numeric(level_count))
  )
}

validate_treatment_label_spec <- function(spec) {
  if (is.null(spec) || length(spec) == 0L) return(NULL)

  factor_names <- vapply(spec, `[[`, character(1), "name")
  if (any(!nzchar(factor_names))) {
    return("Enter a name for every treatment factor.")
  }
  if (any(grepl(",", factor_names, fixed = TRUE))) {
    return("Factor names cannot contain commas.")
  }
  if (anyDuplicated(tolower(factor_names))) {
    return("Each treatment factor needs a unique name.")
  }

  for (factor in spec) {
    numeric_count <- suppressWarnings(as.numeric(factor$level_count))
    if (
      length(numeric_count) != 1L || is.na(numeric_count) ||
        !is.finite(numeric_count) || numeric_count != floor(numeric_count) ||
        numeric_count < 2L
    ) {
      return(sprintf("Number of levels for %s must be a whole number of 2 or greater.", factor$name))
    }
    if (length(factor$levels) != factor$level_count) {
      return(sprintf(
        "%s needs %d comma-separated level names; %d were provided.",
        factor$name,
        factor$level_count,
        length(factor$levels)
      ))
    }
    if (any(!nzchar(factor$levels))) {
      return(sprintf("Every level of %s needs a name.", factor$name))
    }
    if (anyDuplicated(tolower(factor$levels))) {
      return(sprintf("Level names for %s must be unique.", factor$name))
    }
  }

  NULL
}

treatment_labels_customised <- function(spec) {
  if (is.null(spec) || length(spec) == 0L) return(FALSE)
  any(vapply(spec, function(factor) {
    !identical(factor$name, factor$internal) ||
      !identical(factor$levels, factor$defaults)
  }, logical(1)))
}

translate_design_labels <- function(labels, spec) {
  if (!treatment_labels_customised(spec)) return(labels)

  vapply(labels, function(label) {
    components <- strsplit(label, ":", fixed = TRUE)[[1]]
    translated <- vapply(components, function(component) {
      for (factor in spec) {
        if (!startsWith(component, factor$internal)) next
        suffix <- substring(component, nchar(factor$internal) + 1L)
        if (!grepl("^[0-9]+$", suffix)) next
        level <- suppressWarnings(as.integer(suffix))
        if (is.na(level) || level < 1L || level > length(factor$levels)) next
        if (length(spec) == 1L && length(components) == 1L) {
          return(factor$levels[[level]])
        }
        return(paste0(factor$name, ": ", factor$levels[[level]]))
      }
      component
    }, character(1))
    paste(translated, collapse = " × ")
  }, character(1), USE.NAMES = FALSE)
}

translate_model_term <- function(value, spec) {
  if (!treatment_labels_customised(spec) || is.na(value) || !nzchar(value)) return(value)
  parts <- trimws(strsplit(value, ":", fixed = TRUE)[[1]])
  internals <- vapply(spec, `[[`, character(1), "internal")
  if (!all(parts %in% internals)) return(value)
  names_by_internal <- setNames(vapply(spec, `[[`, character(1), "name"), internals)
  paste(unname(names_by_internal[parts]), collapse = " × ")
}

translate_model_formula_text <- function(value, spec) {
  if (!treatment_labels_customised(spec) || is.na(value) || !nzchar(value)) return(value)

  internals <- vapply(spec, `[[`, character(1), "internal")
  display_names <- vapply(spec, `[[`, character(1), "name")
  replacement_order <- order(nchar(internals), decreasing = TRUE)
  placeholders <- paste0("<<PWR4EXP_FACTOR_", seq_along(internals), ">>")
  for (index in replacement_order) {
    value <- gsub(internals[[index]], placeholders[[index]], value, fixed = TRUE)
  }
  for (index in seq_along(placeholders)) {
    value <- gsub(placeholders[[index]], display_names[[index]], value, fixed = TRUE)
  }
  value
}

translate_free_treatment_text <- function(value, spec) {
  if (!treatment_labels_customised(spec) || is.na(value) || !nzchar(value)) return(value)

  tokens <- unlist(lapply(spec, function(factor) {
    paste0(factor$internal, seq_along(factor$levels))
  }), use.names = FALSE)
  replacements <- unlist(lapply(spec, function(factor) {
    if (length(spec) == 1L) {
      factor$levels
    } else {
      paste0(factor$name, ": ", factor$levels)
    }
  }), use.names = FALSE)
  order <- order(nchar(tokens), decreasing = TRUE)
  placeholders <- paste0("<<PWR4EXP_LEVEL_", seq_along(tokens), ">>")
  for (index in order) {
    value <- gsub(tokens[[index]], placeholders[[index]], value, fixed = TRUE)
  }
  for (index in seq_along(placeholders)) {
    value <- gsub(placeholders[[index]], replacements[[index]], value, fixed = TRUE)
  }
  value
}

translate_power_result_labels <- function(result, spec) {
  if (is.null(result) || !treatment_labels_customised(spec)) return(result)
  result <- as.data.frame(result, check.names = FALSE)
  columns <- trimws(names(result))

  for (index in which(columns == "F-test")) {
    result[[index]] <- vapply(
      as.character(result[[index]]),
      translate_model_term,
      character(1),
      spec = spec
    )
  }
  for (index in which(columns %in% c("Contrast", "Variable"))) {
    result[[index]] <- vapply(
      as.character(result[[index]]),
      translate_free_treatment_text,
      character(1),
      spec = spec
    )
  }
  result
}

translate_power_export <- function(result, spec) {
  if (is.null(result) || !treatment_labels_customised(spec)) return(result)
  result_matrix <- as.matrix(result)
  translated <- vapply(as.character(result_matrix), function(value) {
    term <- translate_model_term(value, spec)
    if (!identical(term, value)) term else translate_free_treatment_text(value, spec)
  }, character(1))
  dim(translated) <- dim(result_matrix)
  dimnames(translated) <- dimnames(result_matrix)
  if (is.data.frame(result)) {
    return(as.data.frame(translated, stringsAsFactors = FALSE, check.names = FALSE))
  }
  translated
}

build_crd_design <- function(...) pwr4exp::designCRD(...)
build_rcbd_design <- function(...) pwr4exp::designRCBD(...)
build_latin_square_design <- function(...) pwr4exp::designLSD(...)
build_split_plot_design <- function(...) pwr4exp::designSPD(...)
build_general_design <- function(...) pwr4exp::mkdesign(...)

calculate_power_results <- function(
  design,
  test_type = c("F-test", "t-test", "F-test & t-test"),
  type_ss = 3L,
  sig_level_f = 0.05,
  which = NULL,
  by = NULL,
  contrast = "pairwise",
  sig_level_t = 0.05,
  p_adj = FALSE,
  alternative = "two.sided"
) {
  test_type <- match.arg(test_type)
  type_ss <- as.integer(type_ss)
  if (!(type_ss %in% 1:3)) {
    stop("Choose Type I, II, or III for the sum-of-squares method.", call. = FALSE)
  }
  for (level in c(sig_level_f, sig_level_t)) {
    if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
      stop("Set each significance threshold to a value greater than 0 and less than 1.", call. = FALSE)
    }
  }

  result <- list(omnibus = NULL, contrast = NULL)
  if (test_type %in% c("F-test", "F-test & t-test")) {
    result$omnibus <- pwr4exp::pwr.anova(
      design,
      sig.level = sig_level_f,
      type = type_ss
    )
  }

  if (test_type %in% c("t-test", "F-test & t-test")) {
    if (is.null(which) || !nzchar(which)) {
      stop("Choose an effect to compare before calculating comparison-specific power.", call. = FALSE)
    }
    if (is.numeric(contrast)) {
      contrast <- list(`Contrast vector` = contrast)
    }

    args <- list(
      design,
      which = which,
      contrast = contrast,
      sig.level = sig_level_t,
      p.adj = isTRUE(p_adj),
      alternative = alternative
    )
    by <- normalise_by_factor(by)
    if (!is.null(by)) {
      args$by <- by
    }
    result$contrast <- do.call(pwr4exp::pwr.contrast, args)
  }
  result
}

calculate_contrast_power <- function(
  design,
  which,
  contrast = "pairwise",
  sig.level = 0.05,
  p.adj = FALSE,
  alternative = "two.sided",
  by = NULL
) {
  calculate_power_results(
    design,
    test_type = "t-test",
    which = which,
    by = by,
    contrast = contrast,
    sig_level_t = sig.level,
    p_adj = p.adj,
    alternative = alternative
  )$contrast
}

format_omnibus_result <- function(result) {
  table <- as.data.frame(result)
  data.frame(
    `F-test` = row.names(table),
    table,
    check.names = FALSE,
    row.names = NULL
  )
}

format_contrast_result <- function(result, custom = FALSE) {
  if (is.list(result) && !is.data.frame(result)) {
    tables <- lapply(seq_along(result), function(i) {
      table <- as.data.frame(result[[i]])
      label <- rep(names(result)[i], nrow(table))
      contrast_label <- row.names(table)
      if (custom && length(contrast_label)) contrast_label[1] <- "Contrast vector"
      data.frame(
        ` Variable ` = label,
        ` Contrast ` = contrast_label,
        table,
        check.names = FALSE,
        row.names = NULL
      )
    })
    return(do.call(rbind, tables))
  }

  table <- as.data.frame(result)
  contrast_label <- row.names(table)
  if (custom && length(contrast_label)) contrast_label[1] <- "Contrast vector"
  data.frame(
    ` Contrast ` = contrast_label,
    table,
    check.names = FALSE,
    row.names = NULL
  )
}

make_correlation_formula <- function(rhs, group = NULL) {
  if (!is.character(rhs) || length(rhs) != 1L || !nzchar(rhs)) {
    stop("A correlation covariate is required.", call. = FALSE)
  }
  if (!is.null(group) && nzchar(group)) {
    stats::as.formula(sprintf("~ %s | %s", rhs, group))
  } else {
    stats::as.formula(sprintf("~ %s", rhs))
  }
}

build_correlation_spec <- function(df, type = "none", params = list(), symm = NULL) {
  if (is.null(type) || identical(type, "none")) {
    return(list(df_model = df, cor = NULL))
  }

  supported <- c(
    "corAR1", "corARMA", "corCAR1", "corCompSymm", "corSymm",
    "corExp", "corGaus", "corLin", "corRatio", "corSpher"
  )
  if (!(type %in% supported)) {
    stop(sprintf("Unsupported correlation type: %s", type), call. = FALSE)
  }

  group <- value_or_default(params$group, "")
  if (nzchar(group) && !(group %in% names(df))) {
    stop(sprintf("Grouping variable not found: %s", group), call. = FALSE)
  }

  if (type %in% c("corAR1", "corARMA", "corCAR1", "corCompSymm", "corSymm")) {
    covariate <- params$time
    if (is.null(covariate) || !(covariate %in% names(df))) {
      stop("The selected time/index variable was not found.", call. = FALSE)
    }
    df_model <- df
    time_rhs <- covariate
    if (type %in% c("corAR1", "corARMA") && !is.numeric(df_model[[covariate]])) {
      df_model[[".cor_time_index"]] <- as.integer(factor(df_model[[covariate]]))
      time_rhs <- ".cor_time_index"
    }
    form <- make_correlation_formula(time_rhs, group)

    if (type == "corAR1") {
      return(list(df_model = df_model, cor = nlme::corAR1(params$rho, form = form)))
    }
    if (type == "corCAR1") {
      return(list(df_model = df_model, cor = nlme::corCAR1(params$rho, form = form)))
    }
    if (type == "corCompSymm") {
      return(list(df_model = df_model, cor = nlme::corCompSymm(params$rho, form = form)))
    }
    if (type == "corARMA") {
      p <- as.integer(value_or_default(params$p, 0L))
      q <- as.integer(value_or_default(params$q, 0L))
      if (is.na(p) || is.na(q) || p < 0L || q < 0L || p + q == 0L) {
        stop("For corARMA, p and q must be non-negative and at least one must be positive.", call. = FALSE)
      }
      values <- as.numeric(params$values)
      if (length(values) != p + q || anyNA(values) || any(abs(values) >= 1)) {
        stop("Provide one valid parameter in (-1, 1) for every AR and MA term.", call. = FALSE)
      }
      return(list(
        df_model = df_model,
        cor = nlme::corARMA(value = values, p = p, q = q, form = form)
      ))
    }

    levels <- unique(as.character(df_model[[covariate]]))
    levels <- levels[!is.na(levels)]
    if (is.null(symm) || !is.matrix(symm) || any(dim(symm) != length(levels))) {
      stop("The unstructured correlation matrix is incomplete.", call. = FALSE)
    }
    if (any(abs(symm[lower.tri(symm)]) >= 1) || anyNA(symm[lower.tri(symm)])) {
      stop("Unstructured correlations must be strictly between -1 and 1.", call. = FALSE)
    }
    if (any(eigen(symm, symmetric = TRUE, only.values = TRUE)$values <= 0)) {
      stop("The specified correlation matrix is not positive definite.", call. = FALSE)
    }
    return(list(
      df_model = df_model,
      cor = nlme::corSymm(value = symm[lower.tri(symm)], form = form)
    ))
  }

  coordinates <- params$coordinates
  if (is.null(coordinates) || !length(coordinates) || any(!(coordinates %in% names(df)))) {
    stop("Every selected spatial coordinate must exist in the data.", call. = FALSE)
  }
  range <- as.numeric(params$range)
  if (length(range) != 1L || !is.finite(range) || range <= 0) {
    stop("The spatial range parameter must be positive.", call. = FALSE)
  }
  form <- make_correlation_formula(paste(coordinates, collapse = " + "), group)
  constructor <- switch(
    type,
    corExp = nlme::corExp,
    corGaus = nlme::corGaus,
    corLin = nlme::corLin,
    corRatio = nlme::corRatio,
    corSpher = nlme::corSpher
  )
  list(
    df_model = df,
    cor = constructor(value = range, form = form, nugget = isTRUE(params$nugget))
  )
}

MAX_TREATMENT_FACTORS <- 8L
MAX_INTERACTION_TERMS <- 2^MAX_TREATMENT_FACTORS - 1L
SUPPORTED_UPLOAD_EXTENSIONS <- c("csv", "xlsx", "xls", "txt", "tsv")

value_or_default <- function(value, default) {
  if (is.null(value) || length(value) == 0L) default else value
}

validate_factor_count <- function(n, max_factors = MAX_TREATMENT_FACTORS) {
  message <- sprintf(
    "The number of treatment factors must be an integer between 1 and %d.",
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
    stop("No uploaded filename was provided.", call. = FALSE)
  }
  tolower(tools::file_ext(upload$name))
}

read_uploaded_data <- function(upload) {
  if (is.null(upload$datapath) || !file.exists(upload$datapath)) {
    stop("The uploaded temporary file is unavailable.", call. = FALSE)
  }

  extension <- uploaded_file_extension(upload)
  if (!(extension %in% SUPPORTED_UPLOAD_EXTENSIONS)) {
    stop(
      sprintf(
        "Unsupported file type: .%s. Supported types are: %s.",
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
    stop("The uploaded file is empty.", call. = FALSE)
  }
  data
}

parse_custom_contrast <- function(value, expected_length = NULL, tolerance = 1e-10) {
  if (is.null(value) || !nzchar(trimws(value))) {
    stop("Enter a comma-separated contrast vector.", call. = FALSE)
  }

  fields <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  if (any(!nzchar(fields))) {
    stop("Contrast coefficients cannot be empty.", call. = FALSE)
  }

  coefficients <- suppressWarnings(as.numeric(fields))
  if (anyNA(coefficients) || any(!is.finite(coefficients))) {
    stop("Contrast coefficients must all be finite numbers.", call. = FALSE)
  }
  if (!is.null(expected_length) && length(coefficients) != expected_length) {
    stop(
      sprintf(
        "The contrast has %d coefficients; %d are required.",
        length(coefficients),
        expected_length
      ),
      call. = FALSE
    )
  }
  if (abs(sum(coefficients)) > tolerance) {
    stop("Contrast coefficients must sum to zero.", call. = FALSE)
  }
  coefficients
}

validate_model_formula <- function(value, data) {
  if (is.null(value) || !nzchar(trimws(value))) {
    stop("Enter a model formula.", call. = FALSE)
  }
  if (!grepl("^\\s*~", value)) {
    stop("The formula must start with '~'.", call. = FALSE)
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
        "Variables not found in the uploaded data: %s.",
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
    stop("The sum-of-squares type must be 1, 2, or 3.", call. = FALSE)
  }
  for (level in c(sig_level_f, sig_level_t)) {
    if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
      stop("Significance levels must be strictly between 0 and 1.", call. = FALSE)
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
      stop("A factor of interest is required for contrast power.", call. = FALSE)
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

get_label <- function(data, var) {
  label <- attr(data[[var]], "label")
  if (is.null(label)) {
    label <- var
  }
  label
}


extract_var_info <- function(x, data) {
  # Helper: generate coefficient name part for one variable
  make_part <- function(var, dat) {
    v <- dat[[var]]
    if (is.factor(v)) {
      paste0(var, levels(v)[-1])
    } else {
      var
    }
  }

  # Helper: expand categorical variable to show all levels
  expand_categorical <- function(var, label, v) {
    lvls <- if (is.factor(v))
      levels(v)
    else
      sort(unique(v))
    data.frame(
      term     = c(var, paste0(var, lvls)),
      Variable = c(label, paste0(strrep(" ", 3), lvls)),
      stringsAsFactors = FALSE
    )
  }

  # Interaction term
  if (grepl(":", x)) {
    vars <- strsplit(x, split = ":", fixed = TRUE)[[1]]
    part1 <- make_part(vars[1], data)
    part2 <- make_part(vars[2], data)

    interaction_names <- apply(expand.grid(part1, part2, stringsAsFactors = FALSE),
                               1,
                               paste,
                               collapse = ":")

    return(
      data.frame(
        term     = interaction_names,
        Variable = interaction_names,
        stringsAsFactors = FALSE
      )
    )
  }

  # Single variable
  v <- data[[x]]
  label <- get_label(data, x)

  # Categorical
  if (is.factor(v) || is.character(v)) {
    return(expand_categorical(x, label, v))
  }

  # Numeric/continuous
  data.frame(term     = x,
             Variable = label,
             stringsAsFactors = FALSE)
}

format_pvalue <- function(x, digits = 3, ...) {
  rounded <- round(x, digits)
  ifelse(x < 10^-digits,
         paste0("<", format(10^-digits, scientific = FALSE)),
         ifelse(
           rounded >= 1,
           paste0(">", format(1 - 10^-digits, scientific = FALSE)),
           format(rounded, scientific = FALSE)
         ))
}


tidy_vars <- function(data, vars, intercept = TRUE, ...) {
  # Apply extract_var_info to each variable and combine results
  res <- lapply(vars, extract_var_info, data = data)
  res <- do.call(rbind, res)

  # Optionally prepend (Intercept) row at the top
  if (intercept) {
    res <- rbind(
      data.frame(
        term     = "(Intercept)",
        Variable = "Intercept",
        stringsAsFactors = FALSE
      ),
      res
    )
  }

  res
}


# Format numeric columns
format_numeric <- function(data, cols, digits = 2) {
  cols <- intersect(cols, names(data))

  data[cols] <- lapply(data[cols], function(x)
    sprintf(paste0("%.", digits, "f"), x))

  data
}


# Format estimate and confidence interval
format_estimate_ci <- function(data, digits = 2) {
  data <- format_numeric(
    data,
    cols = c(
      "estimate",
      "std.error",
      "statistic",
      "conf.low",
      "conf.high"
    ),
    digits = digits
  )

  data$`estimate (95% CI)` <- paste0(data$estimate, " (", data$conf.low, ", ", data$conf.high, ")")

  data
}

# Select statistics to display
select_stats <- function(stats) {
  switch(
    match.arg(stats, c("estp", "full", "est")),

    full = c("estimate (95% CI)", "std.error", "statistic", "p.value"),

    estp = c("estimate (95% CI)", "p.value"),

    est = "estimate (95% CI)"
  )
}

# Get column labels
get_stats_labels <- function(lang = c("en", "cn")) {
  switch(
    match.arg(lang),

    en = c(
      "estimate (95% CI)" = "Estimate (95% CI)",
      "std.error"         = "Std. error",
      "statistic"         = "Statistic",
      "p.value"           = "P value"
    ),

    cn = c(
      "estimate (95% CI)" = "\u4f30\u8ba1\u503c (95% CI)",
      "std.error"         = "\u6807\u51c6\u8bef",
      "statistic"         = "\u7edf\u8ba1\u91cf",
      "p.value"           = "P \u503c"
    )
  )
}


# Get variable label
get_variable_label <- function(lang = c("en", "cn")) {
  switch(match.arg(lang), en = "Variable", cn = "\u53d8\u91cf")
}


add_estimate_reference <- function(data, lang = "en") {
  ref.label <- switch(lang, en = "Reference", cn = "\u53c2\u7167\u7ec4")

  ci_cols <- grep("95% CI", colnames(data), fixed = TRUE)

  if (length(ci_cols) == 0) {
    return(data)
  }

  if (any(grepl("^\\s{6,}", data[[1]]))) {
    space_pattern <- "^\\s{6,}"
  } else{
    space_pattern <- "^\\s{3,}"
  }

  index_row <- which(grepl(space_pattern, data[[1]]))

  for (j in ci_cols) {
    index_col <- which(is.na(data[[j]]))
    index <- intersect(index_row, index_col)
    data[index, j] <- ref.label
  }

  data
}

flatten_list <- function(items) {
  morelists <- vapply(items, function(x) {
    identical(class(x)[1], "list")
  }, logical(1))

  if (!any(morelists)) {
    return(items)
  }

  out <- list()

  for (i in seq_along(items)) {
    if (morelists[i]) {
      out <- c(out, items[[i]])

    } else {
      out[[length(out) + 1L]] <- items[[i]]
    }
  }

  Recall(out)
}


#' Set specific values to NA in a data frame
#'
#' This function replaces specified values with NA in selected columns or all columns of a data frame.
#' Optionally converts the affected columns to numeric type after replacement.
#'
#' @param data A data frame to process.
#' @param ... Values to be replaced with NA. Can be multiple values separated by commas.
#' @param cols Character vector specifying column names to apply the replacement. If NULL, applies to all columns.
#' @param to_num Logical flag indicating whether to convert affected columns to numeric. Default is FALSE.
#'
#' @return A data frame with specified values replaced by NA and optionally converted to numeric.
#'
#' @examples
#' # Example 1: Replace single value in all columns
#' df <- data.frame(x = c(1, 999, 3), y = c(999, 2, 3))
#' set_na(df, 999)
#'
#' # Example 2: Replace multiple values in specific columns
#' df <- data.frame(a = c("N/A", "OK", "N/A"), b = c("OK", "N/A", "OK"))
#' set_na(df, "N/A", cols = "a")
#'
#' # Example 3: Replace values and convert to numeric
#' df <- data.frame(x = c("1", "NA", "3"), y = c("NA", "2", "3"))
#' set_na(df, "NA", cols = "x", to_num = TRUE)
#'
#' # Example 4: Replace multiple different values at once
#' df <- data.frame(x = c(-99, 0, 5), y = c(0, -99, 10))
#' set_na(df, -99, 0)
#'
#' # Example 5: Mixed types with conversion
#' df <- data.frame(x = c("NULL", "5", "NULL"), y = c("3", "NULL", "7"))
#' set_na(df, "NULL", to_num = TRUE)
#'
#' @importFrom dplyr mutate across everything where any_of
#' @export
set_na <- function(data,
                   ...,
                   cols = NULL,
                   to_num = FALSE) {
  # Collect values to be replaced as NA
  na_vals <- list(...)

  # Apply NA replacement to either all columns or specified columns
  if (is.null(cols)) {
    res <- dplyr::mutate(data, dplyr::across(dplyr::everything(), ~ ifelse(.x %in% unlist(na_vals), NA, .x)))
  } else {
    res <- dplyr::mutate(data, dplyr::across(dplyr::any_of(cols), ~ ifelse(.x %in% unlist(na_vals), NA, .x)))
  }

  # Optionally convert character columns to numeric
  if (to_num) {
    if (is.null(cols)) {
      res <- dplyr::mutate(res, dplyr::across(where(is.character), ~ suppressWarnings(as.numeric(.))))
    } else {
      res <- dplyr::mutate(res, dplyr::across(dplyr::any_of(cols), ~ suppressWarnings(as.numeric(
        as.character(.)
      ))))
    }
  }

  # Return the modified data frame
  return(res)
}

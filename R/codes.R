#' Read codebook from file or validate data frame
#' @param codes A data.frame or file path (.xlsx, .xls, .csv)
#' @return A data.frame containing the codebook
#' @keywords internal
read_codes <- function(codes) {
  if (is.character(codes) && length(codes) == 1) {
    ext <- tolower(tools::file_ext(codes))
    codes <- switch(
      ext,
      xlsx = readxl::read_excel(codes),
      xls  = readxl::read_excel(codes),
      csv  = read.csv(codes, stringsAsFactors = FALSE),
      stop("Unsupported file format. Use .xlsx, .xls, or .csv")
    )
  }
  if (!is.data.frame(codes))
    stop("codes must be a data.frame or a file path")
  codes
}

#' Replace Chinese punctuation with ASCII comma
#' @param s A string
#' @return A cleaned string
#' @keywords internal
clean_sep <- function(s) {
  gsub("[\uff0c\uff1b;,]", ",", s)
}

#' Parse "1=Male, 2=Female" into numeric values and character labels
#' @param s A string with value-label pairs
#' @return A list with values (numeric) and labels (character)
#' @keywords internal
parse_value_pairs <- function(s) {
  s <- clean_sep(s)
  pairs <- strsplit(trimws(s), ",\\s*")[[1]]
  pairs <- pairs[grepl("=", pairs)]
  if (length(pairs) == 0)
    return(list(values = numeric(), labels = character()))

  parts <- strsplit(pairs, "\\s*=\\s*")
  vals <- sapply(parts, function(p)
    suppressWarnings(as.numeric(trimws(p[1]))))
  lbls <- sapply(parts, function(p)
    trimws(p[2]))
  ok <- !is.na(vals)
  list(values = vals[ok], labels = lbls[ok])
}

#' Parse "999, 998" into numeric vector of missing codes
#' @param s A string with missing value codes
#' @return A numeric vector
#' @keywords internal
parse_missing_values <- function(s) {
  s <- clean_sep(s)
  nums <- strsplit(trimws(s), ",\\s*")[[1]]
  nums <- suppressWarnings(as.numeric(nums))
  nums[!is.na(nums)]
}

#' Convert column to factor using value-label mapping
#' @param col A vector
#' @param s A string with value-label pairs
#' @return A factor with labeled values
#' @keywords internal
apply_value_labels <- function(col, s) {
  p <- parse_value_pairs(s)
  if (length(p$values) == 0)
    return(col)
  ok <- p$values %in% unique(col)
  if (!any(ok))
    return(col)
  factor(col, levels = p$values[ok], labels = p$labels[ok])
}

#' Replace missing codes with NA, handling factor columns
#' @param col A vector
#' @param mis_s A string with missing value codes
#' @param val_s A string with value-label pairs
#' @return A vector with missing values replaced by NA
#' @keywords internal
apply_missing_values <- function(col, mis_s, val_s) {
  nums <- parse_missing_values(mis_s)
  if (length(nums) == 0)
    return(col)

  if (is.factor(col)) {
    p <- parse_value_pairs(val_s)
    drop <- p$labels[p$values %in% nums]
    col[col %in% drop] <- NA
    droplevels(col)
  } else {
    col[col %in% nums] <- NA
    col
  }
}

#' Attach label attribute to a column
#' @param col A vector
#' @param s A string for the label
#' @return A vector with label attribute set
#' @keywords internal
set_label <- function(col, s) {
  if (is.na(s) || nchar(trimws(s)) == 0)
    return(col)
  attr(col, "label") <- trimws(s)
  col
}

#' Apply codebook to a data frame
#'
#' Three steps per variable: value labels -> missing values -> variable labels
#'
#' @param data A data frame to be processed
#' @param codes A codebook data.frame or file path (.xlsx, .xls, .csv).
#'   The codebook must have at least 3 columns:
#'   Column 1: Variable name
#'   Column 2: Variable label
#'   Column 3: Value-label pairs (format: "1=A, 2=B")
#'   Column 4 (optional): Missing values (format: "999, 998")
#'
#' @return A data frame with applied labels and missing value handling
#'
#' @examples
#' \dontrun{
#' codes <- data.frame(
#'   c("gender", "age"),
#'   c("Gender", "Age"),
#'   c("1=Male, 2=Female", NA),
#'   c(NA, "999, 998")
#' )
#' df <- data.frame(gender = c(1, 2), age = c(25, 999))
#' apply_codes(df, codes)
#' }
apply_codes <- function(data, codes) {
  codes <- read_codes(codes)
  df <- data
  nc <- ncol(codes)

  # Step 1 & 2: Apply value labels and handle missing values
  for (i in seq_len(nrow(codes))) {
    var <- as.character(codes[i, 1])
    if (!var %in% names(df))
      next

    val_s <- as.character(codes[i, 3])
    mis_s <- if (nc >= 4)
      as.character(codes[i, 4])
    else
      NA

    col <- df[[var]]
    if (!is.na(val_s) &&
        nchar(trimws(val_s)) > 0)
      col <- apply_value_labels(col, val_s)
    if (!is.na(mis_s) &&
        nchar(trimws(mis_s)) > 0)
      col <- apply_missing_values(col, mis_s, val_s)
    df[[var]] <- col
  }

  # Step 3: Set variable labels (separate loop to avoid losing them during factor conversion)
  for (i in seq_len(nrow(codes))) {
    var <- as.character(codes[i, 1])
    if (!var %in% names(df))
      next
    df[[var]] <- set_label(df[[var]], as.character(codes[i, 2]))
  }

  df
}

#' Generate codebook from a labeled data frame
#'
#' Reverse operation of apply_codes: extract labels and factor levels
#'
#' @param data A data frame with potential label attributes and factors
#' @param lang Language for column names. Either "en" or "cn".
#' @param file Optional file path to save the codebook (.xlsx, .xls, .csv)
#'
#' @return A data frame representing the codebook
#'
#' @examples
#' \dontrun{
#' df <- data.frame(gender = factor(c(1, 2), levels = 1:2, labels = c("M", "F")))
#' attr(df$gender, "label") <- "Gender"
#' get_codes(df)
#' get_codes(df, lang = "cn")
#' get_codes(df, file = "codebook.xlsx")
#' }
get_codes <- function(data,
                      lang = c("en", "cn"),
                      file = NULL) {
  lang <- match.arg(lang)

  # Build one row per variable
  rows <- lapply(names(data), function(var) {
    lbl <- attr(data[[var]], "label")
    if (is.null(lbl))
      lbl <- ""
    val <- ""
    if (is.factor(data[[var]])) {
      levs <- levels(data[[var]])
      val <- paste0(seq_along(levs), "=", levs, collapse = ", ")
    }
    data.frame(
      variable = var,
      label = lbl,
      value = val,
      missing = "",
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, rows)
  rownames(df) <- NULL

  # Set column names based on language
  if (lang == "en") {
    names(df) <- c("Variable", "Label", "Value", "Missing")
  } else {
    names(df) <- c("\u53d8\u91cf",
                   "\u6807\u7b7e",
                   "\u53d6\u503c",
                   "\u7f3a\u5931\u503c")
  }

  # Save to file if requested
  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    switch(
      ext,
      xlsx = writexl::write_xlsx(df, file),
      xls  = writexl::write_xlsx(df, file),
      csv  = write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8"),
      stop("Unsupported file format. Use .xlsx, .xls, or .csv")
    )
  }

  df
}

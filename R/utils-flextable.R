#' Format a flextable with grouped headers and three-line style
#'
#' Format a data frame as a flextable with support for grouped headers,
#' three-line table borders, superscript annotations, cell notes, table
#' footnotes, column widths, alignment, and separate fonts for Western
#' and East Asian characters.
#'
#' Column names containing `sep` are interpreted as grouped columns.
#' The text before the first `sep` is used as the group label, while the
#' remaining text is used as the second-level column label.
#' The original column order is preserved, and a spacer column is
#' automatically inserted between adjacent column groups.
#'
#' If no column names contain `sep`, the data are treated as an
#' ordinary table with a single-level header. No grouping or spacer
#' columns are created.
#'
#' Superscripts can be specified directly in cell values or headers
#' using the `^` notation. For example, `"1.25^a"` is formatted as
#' `1.25` with `a` displayed as a superscript.
#'
#' Notes can be added to body cells or header cells using the `notes`
#' argument. Both numeric column indices and column names are supported,
#' and multiple rows or columns can be specified within a single note.
#'
#' The `fontname` argument controls the Western font, including
#' English letters, numbers, and other Latin-based characters.
#' The `fontname_eastasia` argument controls the East Asian font,
#' including Chinese characters.
#'
#' @param data A data frame containing the data to be formatted.
#' @param sep A character string used to separate group names from
#'   column labels in column names. Defaults to `"__"`.
#' @param fontname A character string specifying the Western font
#'   used for English letters, numbers, and other Latin-based
#'   characters. Defaults to `"Times New Roman"`.
#' @param fontname_eastasia A character string specifying the East Asian
#'   font used for Chinese and other East Asian characters.
#'   Defaults to `"Songti SC"`.
#' @param fontsize A numeric value specifying the font size in points.
#'   Defaults to `11`.
#' @param column_width A numeric value specifying the width of regular
#'   columns in inches. Defaults to `0.8`.
#' @param spacer_width A numeric value specifying the width of spacer
#'   columns in inches. Defaults to `0.2`.
#' @param border_width A numeric value specifying the width of table
#'   borders. Defaults to `1`.
#' @param notes A list of note specifications. Each note must contain
#'   `mark` and `col`, and must contain either `row` for body-cell notes
#'   or `header` for header-cell notes. Column specifications can be
#'   numeric indices or column names. Multiple rows and columns are
#'   supported.
#'
#' @return A `flextable` object.
#'
#' @details
#' The function creates a two-level header when grouped columns are
#' detected. Grouped columns are merged horizontally within the first
#' header level, while ungrouped columns retain an empty first-level
#' header.
#'
#' When no grouped columns are detected, the function creates a
#' standard one-level header without parsing column names.
#'
#' The table uses a three-line layout consisting of a top border,
#' a border below the header, and a bottom border below the table body.
#' All table text is regular weight by default, and the first body
#' column is left-aligned while other cells are centered.
#'
#' Superscripts specified with `^` are automatically parsed in both
#' body cells and header cells. Explicit notes added through `notes`
#' are displayed as superscripts and their corresponding descriptions
#' are added to the table footer.
#'
#' Western and East Asian fonts are specified independently. This allows
#' English and numeric characters to use one font while Chinese
#' characters use another font when the table is exported to Word.
#'
#' If `fontname_eastasia` is `NULL`, the East Asian font defaults to
#' the value supplied to `fontname`.
#'
#'
#' @examples
#' # Ordinary table: no "__" in column names
#' dat1 <- data.frame(
#'   Variable = c("BMI", "Age", "Smoking"),
#'   Estimate = c("1.25^a", "1.10", "0.85"),
#'   P = c("0.03", "0.12", "0.01"),
#'   check.names = FALSE
#' )
#'
#' ft1 <- format_flextable(dat1)
#'
#' # Grouped table: "__" is present in column names
#' dat2 <- data.frame(
#'   Variable = c("BMI", "Age", "Smoking"),
#'   `Model 1__Estimate` = c("1.25^a", "1.10", "0.85"),
#'   `Model 1__P` = c("0.03", "0.12", "0.01"),
#'   `Model 2__Estimate` = c("1.20", "1.08", "0.88"),
#'   `Model 2__P` = c("0.04", "0.15", "0.02"),
#'   check.names = FALSE
#' )
#'
#' ft2 <- format_flextable(dat2)
#'
#' notes <- list(
#'   list(
#'     row = c(1, 3),
#'     col = "Model 1__P",
#'     mark = "d",
#'     text = "P < 0.05"
#'   ),
#'   list(
#'     header = 2,
#'     col = 2:3,
#'     mark = "e",
#'     text = "Adjusted model"
#'   )
#' )
#'
#' ft3 <- format_flextable(
#'   dat2,
#'   notes = notes
#' )
#'
#' @export
format_flextable <- function(
    data,
    sep = "__",
    fontname = "Times New Roman",
    fontname_eastasia = "SimSun",
    fontsize = 11,
    column_width = 0.8,
    spacer_width = 0.2,
    border_width = 1,
    notes = NULL
) {

  # Check input
  stopifnot(is.data.frame(data))

  # Validate notes
  validate_notes(notes = notes, data = data)

  # Get column names
  data_names <- names(data)

  # Check whether grouped columns exist
  has_group <- any(grepl(sep, data_names, fixed = TRUE))

  # Build group structure
  if (has_group) {

    structure <- build_group_structure(
      data_names = data_names,
      sep = sep
    )

    group <- structure$group
    label <- structure$label
    column_index <- structure$column_index
    spacer <- structure$spacer

  } else {

    group <- rep(NA_character_, length(data_names))
    label <- data_names
    column_index <- seq_along(data_names)
    spacer <- rep(FALSE, length(data_names))
  }

  # Build table data
  out <- build_table_data(
    data = data,
    column_index = column_index
  )

  internal_names <- names(out)

  # Build headers
  if (has_group) {

    headers <- build_headers(
      column_index = column_index,
      group = group,
      label = label
    )

    header1 <- headers$header1
    header2 <- headers$header2

  } else {

    header1 <- character(0)
    header2 <- data_names
  }

  # Set internal column names
  names(header2) <- internal_names
  if (has_group) names(header1) <- internal_names

  # Create flextable
  ft <- flextable::flextable(out, col_keys = internal_names)

  # Set second-level header
  ft <- flextable::set_header_labels(ft, values = header2)

  # Add first-level header
  if (has_group) {

    ft <- flextable::add_header_row(
      ft,
      values = header1,
      colwidths = rep(1, length(header1))
    )

    ft <- merge_group_headers(
      ft = ft,
      column_index = column_index,
      group = group
    )
  }

  # Remove all borders
  ft <- flextable::border_remove(ft)

  # Define border
  border <- officer::fp_border(color = "black", width = border_width)

  # Add borders
  if (has_group) {

    ft <- add_table_borders(
      ft = ft,
      column_index = column_index,
      group = group,
      border = border
    )

  } else {

    ft <- flextable::hline_top(ft, border = border, part = "header")
    ft <- flextable::hline_bottom(ft, border = border, part = "header")
    ft <- flextable::hline_bottom(ft, border = border, part = "body")
  }

  # Automatic superscript
  ft <- add_automatic_superscripts(
    ft = ft,
    data = data,
    column_index = column_index,
    header1 = header1,
    header2 = header2,
    fontname = fontname,
    fontname_eastasia = fontname_eastasia,
    fontsize = fontsize
  )

  # Explicit notes
  ft <- add_explicit_notes(
    ft = ft,
    notes = notes,
    data = data,
    data_names = data_names,
    column_index = column_index,
    header1 = header1,
    header2 = header2,
    fontname = fontname,
    fontname_eastasia = fontname_eastasia,
    fontsize = fontsize
  )

  # Column widths
  spacer_col <- which(spacer)
  normal_col <- which(!spacer)

  if (length(normal_col) > 0) {
    ft <- flextable::width(ft, j = normal_col, width = column_width)
  }

  if (length(spacer_col) > 0) {
    ft <- flextable::width(ft, j = spacer_col, width = spacer_width)
  }

  # Footer notes
  ft <- add_note_footer(
    ft = ft,
    notes = notes,
    fontname = fontname,
    fontname_eastasia = fontname_eastasia,
    fontsize = fontsize
  )

  # Set fonts for Western and East Asian characters separately
  ft <- flextable::style(
    ft,
    pr_t = officer::fp_text(
      font.family = fontname,
      eastasia.family = fontname_eastasia,
      hansi.family = fontname,
      font.size = fontsize
    ),
    part = "all"
  )

  # Cancel bold formatting
  ft <- flextable::bold(ft, bold = FALSE, part = "all")

  # Center alignment
  ft <- flextable::align(ft, align = "center", part = "all")

  # Left-align first column
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")

  # Line spacing
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  ft <- flextable::line_spacing(ft, space = 1.5, part = "footer")

  # Vertical alignment
  ft <- flextable::valign(ft, valign = "center", part = "all")

  # Cell padding
  ft <- flextable::padding(
    ft,
    padding.top = 2, padding.bottom = 2,
    padding.left = 2, padding.right = 2,
    part = "all"
  )

  # Fit to page width
  ft <- flextable::set_table_properties(
    ft,
    width = 1,
    layout = "autofit"
  )

  ft
}



# Parse superscript text
parse_superscript <- function(x) {

  x <- as.character(x)

  if (is.na(x)) {

    return(
      list(
        normal = "",
        superscript = ""
      )
    )
  }

  pos <- regexpr(
    "\\^",
    x
  )

  if (pos[1] == -1) {

    return(
      list(
        normal = x,
        superscript = ""
      )
    )
  }

  list(
    normal = substr(
      x,
      1,
      pos[1] - 1
    ),
    superscript = substr(
      x,
      pos[1] + 1,
      nchar(x)
    )
  )
}


# Add superscript text to a flextable cell
add_superscript_cell <- function(ft,
                                 i,
                                 j,
                                 value,
                                 part,
                                 fontname,
                                 fontname_eastasia,
                                 fontsize) {

  parsed <- parse_superscript(
    value
  )

  # Normal text
  normal_chunk <- flextable::as_chunk(
    parsed$normal,
    fontname = fontname,
    props = officer::fp_text(
      font.family = fontname,
      font.size = fontsize,
      bold = FALSE
    )
  )

  # No superscript
  if (!nzchar(parsed$superscript)) {

    return(
      flextable::compose(
        ft,
        i = i,
        j = j,
        value = flextable::as_paragraph(
          normal_chunk
        ),
        part = part
      )
    )
  }

  # Superscript text
  superscript_chunk <- flextable::as_chunk(
    parsed$superscript,
    fontname = fontname,
    props = officer::fp_text(
      font.family = fontname,
      cs.family = fontname,
      eastasia.family = fontname_eastasia,
      font.size = fontsize,
      bold = FALSE,
      vertical.align = "superscript"
    )
  )

  # Compose cell
  flextable::compose(
    ft,
    i = i,
    j = j,
    value = flextable::as_paragraph(
      normal_chunk,
      superscript_chunk
    ),
    part = part
  )
}


# Resolve column names or indices
resolve_note_columns <- function(cols,
                                 data_names) {

  vapply(
    cols,
    function(col) {

      if (is.numeric(col)) {

        if (
          length(col) != 1 ||
          is.na(col) ||
          col < 1 ||
          col > length(data_names) ||
          col != as.integer(col)
        ) {

          stop(
            "Invalid note column index: ",
            col
          )
        }

        return(
          as.integer(col)
        )
      }

      if (is.character(col)) {

        if (
          length(col) != 1 ||
          is.na(col)
        ) {

          stop(
            "Invalid note column name."
          )
        }

        idx <- match(
          col,
          data_names
        )

        if (is.na(idx)) {

          stop(
            "Column not found in data: ",
            col
          )
        }

        return(idx)
      }

      stop(
        "Note column must be numeric or character."
      )
    },
    integer(1)
  )
}


# Validate notes
validate_notes <- function(notes,
                           data) {

  if (is.null(notes)) {
    return(invisible(NULL))
  }

  stopifnot(is.list(notes))

  for (i in seq_along(notes)) {

    note <- notes[[i]]

    # Check mark
    if (!"mark" %in% names(note)) {

      stop(
        "Note ", i,
        " must contain 'mark'."
      )
    }

    if (
      length(note$mark) != 1 ||
      is.na(note$mark)
    ) {

      stop(
        "Note ", i,
        " has an invalid 'mark'."
      )
    }

    # Check row/header
    has_row <- "row" %in% names(note)
    has_header <- "header" %in% names(note)

    if (!has_row && !has_header) {

      stop(
        "Note ", i,
        " must contain either 'row' or 'header'."
      )
    }

    if (has_row && has_header) {

      stop(
        "Note ", i,
        " cannot contain both 'row' and 'header'."
      )
    }

    # Check body row
    if (has_row) {

      rows <- note$row

      if (
        !is.numeric(rows) ||
        length(rows) == 0 ||
        any(is.na(rows)) ||
        any(rows < 1) ||
        any(rows > nrow(data)) ||
        any(rows != as.integer(rows))
      ) {

        stop(
          "Invalid row in note ",
          i,
          ": ",
          paste(
            rows,
            collapse = ", "
          )
        )
      }
    }

    # Check header
    if (has_header) {

      header <- note$header

      if (
        !is.numeric(header) ||
        length(header) != 1 ||
        is.na(header) ||
        header < 1 ||
        header > 2 ||
        header != as.integer(header)
      ) {

        stop(
          "Header must be 1 or 2 in note ",
          i,
          "."
        )
      }
    }

    # Check columns
    if (!"col" %in% names(note)) {

      stop(
        "Note ", i,
        " must contain 'col'."
      )
    }

    if (length(note$col) == 0) {

      stop(
        "Note ", i,
        " must contain at least one column."
      )
    }

    # Check text
    if ("text" %in% names(note)) {

      if (
        length(note$text) != 1 ||
        is.null(note$text)
      ) {

        stop(
          "Note ", i,
          " has invalid 'text'."
        )
      }
    }
  }

  invisible(NULL)
}


# Build grouped-column structure
build_group_structure <- function(data_names,
                                  sep) {

  is_grouped <- grepl(
    sep,
    data_names,
    fixed = TRUE
  )

  group <- rep(
    NA_character_,
    length(data_names)
  )

  label <- data_names

  if (any(is_grouped)) {

    parts <- strsplit(
      data_names[is_grouped],
      sep,
      fixed = TRUE
    )

    group[is_grouped] <- vapply(
      parts,
      `[`,
      character(1),
      1
    )

    label[is_grouped] <- vapply(
      parts,
      function(x) {
        paste(
          x[-1],
          collapse = sep
        )
      },
      character(1)
    )
  }

  column_index <- integer(0)
  spacer <- logical(0)

  for (i in seq_along(data_names)) {

    column_index <- c(
      column_index,
      i
    )

    spacer <- c(
      spacer,
      FALSE
    )

    if (i < length(data_names)) {

      add_spacer <- (
        !is.na(group[i]) &&
          !is.na(group[i + 1]) &&
          group[i] != group[i + 1]
      )

      if (add_spacer) {

        column_index <- c(
          column_index,
          NA_integer_
        )

        spacer <- c(
          spacer,
          TRUE
        )
      }
    }
  }

  list(
    group = group,
    label = label,
    column_index = column_index,
    spacer = spacer
  )
}


# Create table data
build_table_data <- function(data,
                             column_index) {

  out <- lapply(
    column_index,
    function(idx) {

      if (is.na(idx)) {

        rep(
          "  ",
          nrow(data)
        )

      } else {

        data[[idx]]
      }
    }
  )

  out <- as.data.frame(
    out,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  names(out) <- paste0(
    ".col",
    seq_along(column_index)
  )

  out
}


# Create header labels
build_headers <- function(column_index,
                          group,
                          label) {

  header1 <- vapply(
    column_index,
    function(idx) {

      if (
        is.na(idx) ||
        is.na(group[idx])
      ) {

        ""

      } else {

        group[idx]
      }
    },
    character(1)
  )

  header2 <- vapply(
    column_index,
    function(idx) {

      if (is.na(idx)) {

        ""

      } else {

        label[idx]
      }
    },
    character(1)
  )

  list(
    header1 = header1,
    header2 = header2
  )
}


# Merge grouped headers
merge_group_headers <- function(ft,
                                column_index,
                                group) {

  i <- 1

  while (
    i <= length(column_index)
  ) {

    idx <- column_index[i]

    if (is.na(idx)) {

      i <- i + 1
      next
    }

    current_group <- group[idx]

    if (is.na(current_group)) {

      i <- i + 1
      next
    }

    j <- i

    while (
      j <= length(column_index) &&
      !is.na(column_index[j]) &&
      !is.na(group[column_index[j]]) &&
      group[column_index[j]] == current_group
    ) {

      j <- j + 1
    }

    if (j - i > 1) {

      ft <- flextable::merge_at(
        ft,
        i = 1,
        j = i:(j - 1),
        part = "header"
      )
    }

    i <- j
  }

  ft
}


# Add table borders
add_table_borders <- function(ft,
                              column_index,
                              group,
                              border) {

  # Top border
  ft <- flextable::hline_top(
    ft,
    border = border,
    part = "header"
  )

  # Group separator
  i <- 1

  while (
    i <= length(column_index)
  ) {

    idx <- column_index[i]

    if (is.na(idx)) {

      i <- i + 1
      next
    }

    current_group <- group[idx]

    if (is.na(current_group)) {

      i <- i + 1
      next
    }

    j <- i

    while (
      j <= length(column_index) &&
      !is.na(column_index[j]) &&
      !is.na(group[column_index[j]]) &&
      group[column_index[j]] == current_group
    ) {

      j <- j + 1
    }

    ft <- flextable::hline(
      ft,
      i = 1,
      j = i:(j - 1),
      border = border,
      part = "header"
    )

    i <- j
  }

  # Header bottom border
  ft <- flextable::hline_bottom(
    ft,
    border = border,
    part = "header"
  )

  # Body bottom border
  ft <- flextable::hline_bottom(
    ft,
    border = border,
    part = "body"
  )

  ft
}


# Add automatic superscripts from ^ syntax
add_automatic_superscripts <- function(ft,
                                       data,
                                       column_index,
                                       header1,
                                       header2,
                                       fontname,
                                       fontname_eastasia,
                                       fontsize) {

  # Body cells
  for (j in seq_along(column_index)) {

    original_col <- column_index[j]

    if (is.na(original_col)) {
      next
    }

    for (i in seq_len(nrow(data))) {

      value <- data[[original_col]][i]

      if (
        !is.na(value) &&
        grepl(
          "\\^",
          as.character(value)
        )
      ) {

        ft <- add_superscript_cell(
          ft = ft,
          i = i,
          j = j,
          value = as.character(value),
          part = "body",
          fontname = fontname,
          fontname_eastasia = fontname_eastasia,
          fontsize = fontsize
        )
      }
    }
  }

  # Header level 1
  if (length(header1) > 0) {

    for (j in seq_along(header1)) {

      if (
        !is.na(header1[j]) &&
        nzchar(header1[j]) &&
        grepl(
          "\\^",
          header1[j]
        )
      ) {

        ft <- add_superscript_cell(
          ft = ft,
          i = 1,
          j = j,
          value = header1[j],
          part = "header",
          fontname = fontname,
          fontname_eastasia = fontname_eastasia,
          fontsize = fontsize
        )
      }
    }
  }

  # Header level 2
  for (j in seq_along(header2)) {

    if (
      !is.na(header2[j]) &&
      nzchar(header2[j]) &&
      grepl(
        "\\^",
        header2[j]
      )
    ) {

      # Header row depends on whether a first-level header exists
      header_row <- if (length(header1) > 0) {
        2
      } else {
        1
      }

      ft <- add_superscript_cell(
        ft = ft,
        i = header_row,
        j = j,
        value = header2[j],
        part = "header",
        fontname = fontname,
        fontsize = fontsize
      )
    }
  }

  ft
}


# Add explicit notes
add_explicit_notes <- function(ft,
                               notes,
                               data,
                               data_names,
                               column_index,
                               header1,
                               header2,
                               fontname,
                               fontname_eastasia,
                               fontsize) {

  if (is.null(notes) ||
      length(notes) == 0) {

    return(ft)
  }

  for (note in notes) {

    # Resolve columns
    original_cols <- resolve_note_columns(
      note$col,
      data_names
    )

    original_cols <- unique(
      original_cols
    )

    # Find table columns
    table_cols <- vapply(
      original_cols,
      function(col) {

        idx <- which(
          column_index == col
        )

        if (length(idx) != 1) {

          stop(
            "Unable to resolve note column: ",
            data_names[col]
          )
        }

        idx
      },
      integer(1)
    )

    mark <- note$mark

    # Body note
    if ("row" %in% names(note)) {

      for (row in note$row) {

        for (k in seq_along(original_cols)) {

          original_col <- original_cols[k]
          table_col <- table_cols[k]

          value <- data[[original_col]][row]

          if (is.na(value)) {

            value <- ""

          } else {

            value <- as.character(value)
          }

          # Add explicit mark
          value <- paste0(
            value,
            "^",
            mark
          )

          ft <- add_superscript_cell(
            ft = ft,
            i = row,
            j = table_col,
            value = value,
            part = "body",
            fontname = fontname,
            fontname_eastasia = fontname_eastasia,
            fontsize = fontsize
          )
        }
      }
    }

    # Header note
    if ("header" %in% names(note)) {

      header_row <- as.integer(
        note$header
      )

      # In an ordinary table there is only one header row
      if (length(header1) == 0 &&
          header_row == 2) {

        stop(
          "header = 2 is not available when no grouped columns exist."
        )
      }

      for (table_col in table_cols) {

        value <- if (header_row == 1) {
          header1[table_col]
        } else {
          header2[table_col]
        }

        if (
          is.na(value) ||
          !nzchar(value)
        ) {
          next
        }

        value <- paste0(
          value,
          "^",
          mark
        )

        ft <- add_superscript_cell(
          ft = ft,
          i = header_row,
          j = table_col,
          value = value,
          part = "header",
          fontname = fontname,
          fontname_eastasia = fontname_eastasia,
          fontsize = fontsize
        )
      }
    }
  }

  ft
}


# Add note footer
add_note_footer <- function(ft, notes,
                            fontname,
                            fontname_eastasia,
                            fontsize) {
  if (is.null(notes) ||
      length(notes) == 0) {
    return(ft)
  }

  # Get unique marks
  marks <- unique(vapply(notes, function(x) {
    as.character(x$mark)
  }, character(1)))

  # Get note text for each mark
  note_text <- vapply(marks, function(mark) {
    # Find notes with the same mark
    matched <- notes[vapply(notes, function(x) {
      identical(as.character(x$mark), mark)
    }, logical(1))]

    # Extract note text
    texts <- vapply(matched, function(x) {
      if ("text" %in% names(x) &&
          !is.null(x$text)) {
        as.character(x$text)

      } else {
        ""
      }
    }, character(1))

    # Remove empty text
    texts <- texts[nzchar(trimws(texts))]

    # Combine multiple texts belonging
    # to the same mark
    if (length(texts) == 0) {
      ""

    } else {
      txt <- paste(unique(texts), collapse = "\r\n")

      paste0(txt, ".")
    }
  }, character(1))

  # Keep notes with text
  keep <- nzchar(trimws(note_text))

  if (!any(keep)) {
    return(ft)
  }

  marks <- marks[keep]
  note_text <- note_text[keep]

  # Add one footer row
  ft <- flextable::add_footer_lines(ft, values = "")

  # Build footer chunks
  chunks <- list()

  for (i in seq_along(marks)) {
    # Superscript mark
    chunks[[length(chunks) + 1]] <-
      flextable::as_chunk(
        marks[i],
        fontname = fontname,
        props = officer::fp_text(
          font.size = fontsize,
          font.family = fontname,
          eastasia.family = fontname_eastasia,
          bold = FALSE,
          vertical.align = "superscript"
        )
      )

    # Note text
    chunks[[length(chunks) + 1]] <-
      flextable::as_chunk(
        paste0(" ", note_text[i]),
        fontname = fontname,
        props = officer::fp_text(
          font.size = fontsize,
          font.family = fontname,
          eastasia.family = fontname_eastasia,
          bold = FALSE,
          vertical.align = "baseline"
        )
      )

    # Separator between different marks
    if (i < length(marks)) {
      chunks[[length(chunks) + 1]] <-
        flextable::as_chunk(
          "\r\n",
          fontname = fontname,
          props = officer::fp_text(
            font.size = fontsize,
            font.family = fontname,
            eastasia.family = fontname_eastasia,
            bold = FALSE
          )
        )
    }
  }

  # Convert chunks to paragraph
  footer_paragraph <- do.call(flextable::as_paragraph, chunks)

  # Compose footer
  ft <- flextable::compose(
    ft,
    i = 1,
    j = 1,
    value = footer_paragraph,
    part = "footer"
  )

  ft
}

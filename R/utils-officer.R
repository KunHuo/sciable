PACKAGE.NANE <- "sciable"

#' Write objects to a Word document
#'
#' Create a Word document from multiple R objects and save it as a
#' Microsoft Word `.docx` file. Supported objects can be supplied directly
#' or nested within lists. Objects are written to the document in their
#' original order.
#'
#' Page orientation can be specified globally using `landscape` or
#' individually for each object using a `landscape` attribute. A section
#' break is added only when the orientation changes between consecutive
#' objects.
#'
#' @param ... Objects to be written to the Word document. Supported objects
#'   include `data.frame`, `flextable`, `ggplot`, character strings, and
#'   nested lists containing supported objects.
#' @param path Character string specifying the path of the output `.docx`
#'   file.
#' @param landscape Logical value indicating whether the default page
#'   orientation should be landscape. The default is `FALSE`. An individual
#'   object can override this setting by specifying a `landscape` attribute.
#'
#' @return Invisibly returns the `officer` Word document object.
#'
#' @details
#' The Word document is created from the `default.docx` template returned by
#' `get_template("default.docx")`.
#'
#' For `data.frame` objects, `add_docx_item()` handles conversion to a
#' `flextable`. If a `data.frame` has a `title` or `note` attribute, these
#' are added above and below the table, respectively.
#'
#' Ordinary lists are recursively flattened by `flatten_docx_items()` while
#' preserving the original order of objects. Objects such as `data.frame`,
#' `flextable`, and `ggplot` are treated as individual document items.
#'
#' @examples
#' \dontrun{
#' # Write individual objects
#' write_word(
#'   head(iris),
#'   head(mtcars),
#'   path = "results.docx"
#' )
#'
#' # Write objects supplied as a list
#' tables <- list(
#'   head(iris, 2),
#'   head(iris, 3)
#' )
#'
#' write_word(
#'   tables,
#'   path = "tables.docx"
#' )
#'
#' # Mix lists and individual objects
#' write_word(
#'   tables,
#'   "Title",
#'   head(iris, 4),
#'   path = "results.docx"
#' )
#'
#' # Specify the page orientation globally
#' write_word(
#'   head(iris),
#'   path = "landscape.docx",
#'   landscape = TRUE
#' )
#'
#' # Specify the orientation for an individual object
#' x <- head(iris)
#' attr(x, "landscape") <- TRUE
#'
#' write_word(
#'   x,
#'   head(mtcars),
#'   path = "mixed_orientation.docx"
#' )
#' }
#'
#' @export
write_word <- function(..., path, landscape = FALSE) {
  # Collect all input objects
  items <- list(...)

  # Flatten ordinary lists while preserving order
  #items <- flatten_docx_items(items)

  # Create Word document
  doc <- officer::read_docx(get_template("default.docx"))

  # Track the previous page orientation
  previous_landscape <- isTRUE(landscape)

  # Add objects sequentially
  for (i in seq_along(items)) {
    item <- items[[i]]

    # Determine the orientation for the current object
    current_landscape <- get_item_landscape(item = item, default = landscape)

    # Change section only when orientation changes
    if (!identical(current_landscape, previous_landscape)) {
      doc <- set_docx_orientation(doc = doc, landscape = current_landscape)
    }

    # Add current object
    doc <- add_docx_item(
      doc = doc,
      item = item,
      landscape = current_landscape,
      title_style = "Normal",
      note_style = "Normal"
    )

    # Add spacing between objects
    if (i < length(items)) {
      doc <- officer::body_add_par(doc, value = "", style = "Normal")
    }

    # Update previous orientation
    previous_landscape <- current_landscape
  }

  # Save Word document
  exec_write_docx(x = doc, path = path)

  invisible(doc)
}


# Get file extension
file_ext <- function(path) {
  if (length(path) != 1L || is.na(path)) {
    return(character(0))
  }

  tools::file_ext(path)
}


# Add or replace file extension
file_path <- function(path, ext = "docx") {
  # Validate path
  if (length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))) {
    stop("Path cannot be empty.", call. = FALSE)
  }

  # Clean path
  path <- trimws(path)

  # Clean extension
  ext <- sub("^\\.", "", ext)

  if (length(ext) != 1L ||
      is.na(ext) ||
      !nzchar(ext)) {
    stop("Extension cannot be empty.", call. = FALSE)
  }

  # Get existing extension
  current_ext <- file_ext(path)

  # Add or replace extension
  if (!nzchar(current_ext)) {
    path <- paste0(path, ".", ext)

  } else if (!identical(tolower(current_ext), tolower(ext))) {
    path <- paste0(tools::file_path_sans_ext(path), ".", ext)
  }

  path
}


# Create parent directory
dir_create <- function(path) {
  path_name <- dirname(path)

  if (!identical(path_name, ".") &&
      !dir.exists(path_name)) {
    dir.create(path_name, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(path)
}


# Write an officer document
exec_write_docx <- function(x, path, ...) {
  # Validate path
  if (length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))) {
    stop("Path cannot be empty.", call. = FALSE)
  }

  # Validate document
  if (!inherits(x, "rdocx")) {
    stop("'x' must be an officer rdocx object.", call. = FALSE)
  }

  # Prepare path
  path <- file_path(path)

  # Create directory
  dir_create(path)

  # Write document
  print(x, target = path, ...)

  invisible(path)
}


get_template <- function(template) {
  template <- paste("templates", template, sep = "/")
  # package  <- methods::getPackageName()
  template <- file.path(system.file(package = PACKAGE.NANE), template)
  regression <- regexpr(paste("(\\.(?i)(docx))$", sep = ""), template)

  if (regression < 1) {
    stop("invalid template name, it must have extension.docx", call. = FALSE)
  }
  # template <- R.utils::getAbsolutePath(template, expandTilde = TRUE)

  if (!file.exists(template)) {
    stop(template , " can not be found.")
  }
  return(template)
}

# Flatten ordinary lists while preserving object order
flatten_docx_items <- function(items) {
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


# Get the landscape setting for an individual object
get_item_landscape <- function(item, default = FALSE) {
  value <- attr(item, "landscape", exact = TRUE)

  if (is.null(value)) {
    return(isTRUE(default))
  }

  isTRUE(value)
}


# Add a table title to a Word document
add_docx_title <- function(doc, title, style = "Normal") {
  if (is.null(title) || length(title) == 0) {
    return(doc)
  }

  title <- paste(as.character(title), collapse = "\n")

  officer::body_add_par(doc, value = title, style = style)
}



add_docx_note <- function(doc,
                          value,
                          style = "Normal",
                          fontname = "Times New Roman",
                          fontname_eastasia = "SimSun",
                          fontsize = 11) {

  stopifnot(
    is.character(value),
    length(value) == 1
  )

  # Split text into superscript, line break, and normal text
  parts <- regmatches(
    value,
    gregexpr(
      "\\^[^\\^\\n]+\\^|\\n|[^\\^\\n]+",
      value,
      perl = TRUE
    )
  )[[1]]

  # Create formatted text chunks
  chunks <- lapply(parts, function(x) {

    # Line break
    if (x == "\n") {

      officer::run_linebreak()

      # Superscript text
    } else if (grepl("^\\^[^\\^]+\\^$", x)) {

      text <- sub("^\\^", "", x)
      text <- sub("\\^$", "", text)

      officer::ftext(
        text,
        prop = officer::fp_text(
          font.family = fontname,
          cs.family = fontname,
          eastasia.family = fontname_eastasia,
          font.size = fontsize,
          vertical.align = "superscript"
        )
      )

      # Normal text
    } else {

      officer::ftext(
        x,
        prop = officer::fp_text(
          font.family = fontname,
          cs.family = fontname,
          eastasia.family = fontname_eastasia,
          font.size = fontsize
        )
      )
    }
  })

  # Combine text chunks
  fp <- do.call(
    officer::fpar,
    chunks
  )

  # Add paragraph to document
  officer::body_add_fpar(
    doc,
    fp,
    style = style
  )
}

# Add a data frame to a Word document
add_docx_table <- function(doc,
                           data,
                           title_style = "Normal",
                           note_style = "Normal") {
  title <- attr(data, "title", exact = TRUE)

  note <- attr(data, "note", exact = TRUE)

  # Add table title
  if (!is.null(title)) {
    doc <- add_docx_title(doc = doc,
                          title = title,
                          style = title_style)
  }

  # Format data frame as flextable
  ft <- format_flextable(data)

  # Add flextable
  doc <- flextable::body_add_flextable(doc, value = ft)

  # Add table note
  if (!is.null(note)) {
    doc <- add_docx_note(doc = doc,
                         value = note,
                         style = note_style)
  }

  doc
}


# Get plot dimensions for a Word document
get_docx_plot_size <- function(landscape = FALSE) {
  if (isTRUE(landscape)) {
    return(list(width = 9, height = 6))
  }

  list(width = 6, height = 4)
}


# Add a ggplot to a Word document
add_docx_plot <- function(doc,
                          plot,
                          landscape = FALSE,
                          dpi = 300) {
  size <- get_docx_plot_size(landscape = landscape)

  temp_file <- tempfile(pattern = "docx_plot_", fileext = ".tiff")

  on.exit(unlink(temp_file), add = TRUE)

  ggplot2::ggsave(
    filename = temp_file,
    plot = plot,
    width = size$width,
    height = size$height,
    dpi = dpi
  )

  officer::body_add_img(doc,
                        src = temp_file,
                        width = size$width,
                        height = size$height)
}


# Set the Word document section orientation
set_docx_orientation <- function(doc, landscape = FALSE) {
  section <- officer::prop_section(page_size = officer::page_size(orient = if (isTRUE(landscape)) {
    "landscape"
  } else {
    "portrait"
  }))

  doc <- officer::body_end_section_continuous(doc)

  officer::body_set_default_section(doc, section)
}


# Add an object to a Word document
add_docx_item <- function(doc,
                          item,
                          landscape = FALSE,
                          title_style = "Normal",
                          note_style = "Normal") {
  # Add flextable
  if (inherits(item, "flextable")) {
    return(flextable::body_add_flextable(doc, value = item))
  }

  # Add ggplot
  if (inherits(item, "ggplot")) {
    return(add_docx_plot(
      doc = doc,
      plot = item,
      landscape = landscape
    ))
  }

  # Add data frame
  if (is.data.frame(item)) {
    return(
      add_docx_table(
        doc = doc,
        data = item,
        title_style = title_style,
        note_style = note_style
      )
    )
  }

  # Add character text
  if (is.character(item)) {
    return(officer::body_add_par(
      doc,
      value = paste(item, collapse = "\n"),
      style = "Normal"
    ))
  }

  # Add other object types
  warning(
    "Unrecognized object type: ",
    paste(class(item), collapse = ", "),
    ". Attempting to convert to character."
  )

  officer::body_add_par(doc,
                        value = paste(utils::capture.output(print(item)), collapse = "\n"),
                        style = "Normal")
}

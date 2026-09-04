#' Save a ggplot with automatic PDF detection
#'
#' This function wraps \code{ggplot2::ggsave} to automatically detect PDF file
#' paths and use the \code{cairo_pdf} device for better PDF output quality.
#' For non-PDF formats, it falls back to the default ggsave behavior.
#'
#' @param plot A ggplot or patchwork object to save.
#' @param path Character string specifying the file path for saving.
#' @param width Numeric, width of the output plot. Default is 8.3.
#' @param height Numeric, height of the output plot. Default is width divided by 7.
#' @param units Character string for the unit of width and height. Default is "cm".
#' @param dpi Numeric, resolution in dots per inch. Default is 300.
#' @param family Character string specifying font family for text elements.
#'   Only applied when \code{plot} is a ggplot/patchwork object and \code{family} is not NULL.
#'   Default is NULL.
#' @param ... Additional arguments passed to \code{ggplot2::ggsave}.
#'
#' @return The saved file path invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::qplot(mpg, wt, data = mtcars)
#' gg_save(p, "my_plot.pdf")
#' gg_save(p, "my_plot.png", width = 10, height = 15, dpi = 600)
#' gg_save(p, "my_plot.pdf", family = "Times New Roman")
#'
#' # With patchwork
#' library(patchwork)
#' p1 <- ggplot2::qplot(mpg, wt, data = mtcars)
#' p2 <- ggplot2::qplot(hp, disp, data = mtcars)
#' combined <- p1 + p2
#' gg_save(combined, "combined.pdf", family = "Microsoft YaHei")
#' }
gg_save <- function(plot,
                    path,
                    width = 8.5,
                    height = width / 1.618,
                    units = "cm",
                    dpi = 300,
                    family = NULL,
                    ...) {

  # Apply font family if specified
  if (!is.null(family)) {
    if (inherits(plot, "patchwork")) {
      # Use patchwork's & operator to apply theme to all subplots
      plot <- plot & ggplot2::theme(text = ggplot2::element_text(family = family))
    } else if (inherits(plot, "ggplot")) {
      # Single ggplot object
      plot <- plot + ggplot2::theme(text = ggplot2::element_text(family = family))
    }
  }

  # Check if the file path indicates a PDF format
  is_pdf <- grepl("\\.pdf$", path, ignore.case = TRUE)

  if (is_pdf) {
    # Use cairo_pdf device for PDF output
    ggplot2::ggsave(
      filename = path,
      plot = plot,
      width = width,
      height = height,
      units = units,
      device = grDevices::cairo_pdf,
      create.dir = TRUE,
      ...
    )
  } else {
    # Use default device for other formats
    ggplot2::ggsave(
      filename = path,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      create.dir = TRUE,
      ...
    )
  }
}


#' Add a tag to a ggplot
#'
#' This function creates a list of ggplot2 components that adds a tag (e.g., "A",
#' "B", etc.) to the top-left corner of a plot. It combines a label with custom
#' text formatting and positioning. All ggplot2 functions are called with explicit
#' namespace prefix to avoid conflicts.
#'
#' @param label A character string specifying the tag text. Default is "A".
#' @param size Numeric value for the font size of the tag. Default is 9.
#' @param face Character string for the font face (e.g., "bold", "italic").
#'   Default is "bold".
#' @param color Character string for the text color. Default is "black".
#'
#' @return A list containing:
#'   \itemize{
#'     \item A `ggplot2::labs()` call to set the tag label.
#'     \item A `ggplot2::theme()` call to customize the tag appearance and
#'       position using `ggplot2::element_text()`.
#'   }
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p + add_tag("A", size = 14, color = "blue")
#'
#' @export
add_tag <- function(label = "A", size = 9, face = "bold", color = "black") {
  list(
    ggplot2::labs(tag = label),
    ggplot2::theme(
      plot.tag = ggplot2::element_text(size = size, face = face, color = color),
      plot.tag.position = "topleft"
    )
  )
}



#' Reverse x-axis order (works with +)
#'
#' Automatically handles both discrete (factor/character) and continuous (numeric) x variables.
#' No need to manually convert x to factor.
#'
#' @param ... Currently unused. Included for future compatibility.
#'
#' @return A ggplot object with reversed x-axis
#' @export
#'
#' @importFrom ggplot2 ggplot_add
#'
#' @examples
#' \donttest{
#' # Numeric x variable
#' ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
#'   ggplot2::geom_point() +
#'   reverse_x_axis()
#'
#' # Character x variable
#' ggplot2::ggplot(mtcars, ggplot2::aes(as.character(cyl), mpg)) +
#'   ggplot2::geom_boxplot() +
#'   reverse_x_axis()
#'
#' # Factor x variable
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_boxplot() +
#'   reverse_x_axis()
#'
#' # With bar plot
#' df <- data.frame(
#'   category = c("High", "Medium", "Low"),
#'   value = c(30, 50, 20)
#' )
#' ggplot2::ggplot(df, ggplot2::aes(category, value)) +
#'   ggplot2::geom_col() +
#'   reverse_x_axis()
#' }
reverse_x_axis <- function(...) {
  structure(list(), class = "gg_reverse_x")
}

#' @export
ggplot_add.gg_reverse_x <- function(object, plot, ...) {
  # First try scale_x_discrete(limits = rev) for discrete variables
  result <- tryCatch(
    plot + ggplot2::scale_x_discrete(limits = rev),
    error = function(e) NULL
  )

  # If failed, try scale_x_continuous(trans = "reverse") for numeric variables
  if (is.null(result)) {
    result <- tryCatch(
      plot + ggplot2::scale_x_continuous(trans = "reverse"),
      error = function(e) NULL
    )
  }

  if (is.null(result)) {
    stop("Cannot reverse x-axis: unsupported x variable type.")
  }

  result
}


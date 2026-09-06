#' Check Color Adjustment Amount
#'
#' Validate the `amount` argument used for color adjustment functions.
#'
#' @param amount A numeric vector specifying the magnitude of color
#'   adjustment.
#' @param range A numeric vector of length two specifying the inclusive
#'   lower and upper bounds allowed for `amount`.
#'
#' @return The input `amount`, invisibly.
#'
#' @keywords internal
.check_amount <- function(amount, range = c(0, 1)) {
  if (!is.numeric(amount)) {
    stop("`amount` must be a numeric vector.", call. = FALSE)
  }

  if (!length(amount)) {
    stop("`amount` must not be empty.", call. = FALSE)
  }

  if (any(!is.finite(amount))) {
    stop("`amount` must contain only finite values.", call. = FALSE)
  }

  if (length(range) != 2L ||
      !is.numeric(range) ||
      any(!is.finite(range)) ||
      range[1] > range[2]) {
    stop("Internal error: `range` must be a valid numeric range.",
         call. = FALSE)
  }

  if (any(amount < range[1] | amount > range[2])) {
    stop(sprintf("`amount` must be between %g and %g.", range[1], range[2]),
         call. = FALSE)
  }

  invisible(amount)
}


#' Lighten Colors
#'
#' Lighten one or more colors by a specified amount.
#'
#' @param colors A character vector of colors.
#' @param amount A numeric vector specifying the amount by which the colors
#'   should be lightened. Values must be between 0 and 1.
#' @param space A character string specifying the color space used for
#'   lightening. One of `"HCL"`, `"HLS"`, or `"combined"`.
#' @param plot Logical. If `TRUE`, display the resulting colors using
#'   [show_colors()]. Defaults to `FALSE`.
#'
#' @return If `plot = FALSE`, a character vector of lightened colors.
#'   If `plot = TRUE`, a ggplot object returned by [show_colors()].
#'   When `amount` contains multiple values, multiple colors are returned
#'   for each input color.
#'
#' @details
#' This function is a wrapper around [colorspace::lighten()]. The values of
#' `amount` are applied independently to each color in `colors`.
#'
#' @examples
#' lighten_colors("#336699", 0.2)
#'
#' lighten_colors(
#'   "#336699",
#'   c(0.1, 0.2, 0.3)
#' )
#'
#' lighten_colors(
#'   "#336699",
#'   c(0.1, 0.2, 0.3),
#'   plot = TRUE
#' )
#'
#' @seealso
#' [darken_colors()], [shade_colors()], [show_colors()]
#'
#' @export
lighten_colors <- function(colors,
                           amount,
                           space = c("HCL", "HLS", "combined"),
                           plot = FALSE) {
  space <- match.arg(space)

  .check_amount(amount)

  result <- unlist(lapply(colors, function(color) {
    colorspace::lighten(color, amount = amount, space = space)
  }), use.names = FALSE)

  if (plot) {
    show_colors(result)
  } else {
    result
  }
}


#' Darken Colors
#'
#' Darken one or more colors by a specified amount.
#'
#' @param colors A character vector of colors.
#' @param amount A numeric vector specifying the amount by which the colors
#'   should be darkened. Values must be between 0 and 1.
#' @param space A character string specifying the color space used for
#'   darkening.
#' @param plot Logical. If `TRUE`, display the resulting colors using
#'   [show_colors()]. Defaults to `FALSE`.
#'
#' @return If `plot = FALSE`, a character vector of darkened colors.
#'   If `plot = TRUE`, a ggplot object returned by [show_colors()].
#'   When `amount` contains multiple values, multiple colors are returned
#'   for each input color.
#'
#' @details
#' This function is a wrapper around [colorspace::darken()]. The values of
#' `amount` are applied independently to each color in `colors`.
#'
#' @examples
#' darken_colors("#336699", 0.2)
#'
#' darken_colors(
#'   "#336699",
#'   c(0.1, 0.2, 0.3)
#' )
#'
#' darken_colors(
#'   "#336699",
#'   c(0.1, 0.2, 0.3),
#'   plot = TRUE
#' )
#'
#' @seealso
#' [lighten_colors()], [shade_colors()], [show_colors()]
#'
#' @export
darken_colors <- function(colors,
                          amount,
                          space = "combined",
                          plot = FALSE) {
  .check_amount(amount)

  result <- unlist(lapply(colors, function(color) {
    colorspace::darken(color, amount = amount, space = space)
  }), use.names = FALSE)

  if (plot) {
    show_colors(result)
  } else {
    result
  }
}


#' Lighten or Darken Colors
#'
#' Lighten or darken colors according to the sign of the adjustment amount.
#'
#' @param colors A character vector of colors.
#' @param amount A numeric vector specifying the direction and magnitude of
#'   color adjustment. Negative values lighten colors, positive values darken
#'   colors, and zero values leave colors unchanged. Values must be between
#'   -1 and 1.
#' @param space A character string specifying the color space used for
#'   lightening. One of `"HCL"`, `"HLS"`, or `"combined"`. Positive values
#'   are always processed using `"combined"` for darkening.
#' @param plot Logical. If `TRUE`, display the resulting colors using
#'   [show_colors()]. Defaults to `FALSE`.
#'
#' @return If `plot = FALSE`, a character vector containing the adjusted
#'   colors. If `plot = TRUE`, a ggplot object returned by [show_colors()].
#'   For each input color, the results corresponding to `amount` are returned
#'   in the same order as supplied.
#'
#' @details
#' Negative values of `amount` are used to lighten colors, with their
#' absolute values passed to [colorspace::lighten()]. Positive values are
#' used to darken colors with [colorspace::darken()] using the
#' `"combined"` method. Zero values return the original color unchanged.
#'
#' @examples
#' shade_colors(
#'   "#336699",
#'   c(-0.3, -0.15, 0, 0.15, 0.3)
#' )
#'
#' shade_colors(
#'   "#336699",
#'   c(-0.3, -0.15, 0, 0.15, 0.3),
#'   plot = TRUE
#' )
#'
#' @seealso
#' [lighten_colors()], [darken_colors()], [show_colors()]
#'
#' @export
shade_colors <- function(colors,
                         amount,
                         space = c("HCL", "HLS", "combined"),
                         plot = FALSE) {
  space <- match.arg(space)

  .check_amount(amount, range = c(-1, 1))

  result <- unlist(lapply(colors, function(color) {
    vapply(amount, function(a) {
      if (a < 0) {
        colorspace::lighten(color, amount = abs(a), space = space)
      } else if (a > 0) {
        colorspace::darken(color, amount = a, space = "combined")
      } else {
        color
      }
    }, character(1))
  }), use.names = FALSE)

  if (plot) {
    show_colors(result)
  } else {
    result
  }
}


#' Display a Color Palette
#'
#' Display a vector of colors as a color palette using ggplot2.
#'
#' @param colors A character vector of colors.
#' @param labels Logical. Whether to display color codes below each color.
#'   Defaults to `TRUE`.
#' @param size Numeric. The text size of color labels. Defaults to `3.5`.
#'
#' @return A ggplot object displaying the supplied colors.
#'
#' @examples
#' show_colors(c("#336699", "#CC3333", "#66CC99"))
#'
#' show_colors(
#'   c("#336699", "#CC3333", "#66CC99"),
#'   labels = FALSE
#' )
#'
#' @export
show_colors <- function(colors,
                        labels = TRUE,
                        size = 3.5) {
  data <- data.frame(x = seq_along(colors), color = colors)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = 1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$color),
                       width = 1,
                       height = 1) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1.5),
                                breaks = NULL,
                                expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(30, 10, 30, 10))

  if (labels) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x = .data$x, y = 0.35, label = .data$color), size = size)
  }

  p
}

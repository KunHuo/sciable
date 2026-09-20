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

#' @title Get ggsci palette colors
#' @description Unified interface to get colors from various ggsci palettes.
#' This is the core function called by all pal_*() wrapper functions.
#' @param palette Palette name. Available options:
#'   \describe{
#'     \item{aaas}{AAAS journal color palettes}
#'     \item{atlassian}{Atlassian Design System palette}
#'     \item{bmj}{BMJ color palettes}
#'     \item{bs5}{Bootstrap 5 color palettes}
#'     \item{cosmic}{COSMIC color palettes}
#'     \item{d3}{D3.js color palettes}
#'     \item{flatui}{Flat UI color palettes}
#'     \item{frontiers}{Frontiers journal color palettes}
#'     \item{futurama}{Futurama color palettes}
#'     \item{gephi}{Gephi color palettes}
#'     \item{gsea}{The GSEA GenePattern color palettes}
#'     \item{igv}{Integrative Genomics Viewer (IGV) color palettes}
#'     \item{iterm}{iTerm color palettes}
#'     \item{jama}{Journal of the American Medical Association color palettes}
#'     \item{jco}{Journal of Clinical Oncology color palettes}
#'     \item{lancet}{Lancet journal color palettes}
#'     \item{locuszoom}{LocusZoom color palette}
#'     \item{material}{Material Design color palettes}
#'     \item{nejm}{NEJM color palettes}
#'     \item{npg}{NPG journal color palettes}
#'     \item{observable}{Observable 10 color palette}
#'     \item{primer}{Primer design system palette}
#'     \item{rickandmorty}{Rick and Morty color palettes}
#'     \item{simpsons}{The Simpsons color palettes}
#'     \item{startrek}{Star Trek color palettes}
#'     \item{tron}{Tron Legacy color palettes}
#'     \item{tw3}{Tailwind CSS color palettes}
#'     \item{uchicago}{The University of Chicago color palettes}
#'     \item{ucscgb}{UCSC Genome Browser color palette}
#'   }
#' @param n Number of colors. Defaults to the maximum number of the palette if NULL
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#'   Negative values lighten, positive values darken. Values must be between -1 and 1.
#'   Passed to \code{\link{shade_colors}}.
#' @param show Whether to display the color
#'
#' @export
#'
#' @examples
#' pal("jama")
#' pal("npg", 3)
#' pal("lancet", alpha = 0.5, show = TRUE)
#' pal("nejm", 5, amount = c(-0.3, 0, 0.3))
pal <- function(palette = "jama", n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  # Validate input
  if (!is.character(palette) || length(palette) != 1) {
    stop("'palette' must be a single character string")
  }

  # Construct the ggsci function name dynamically
  fun_name <- paste0("pal_", palette)

  # Check if ggsci is installed
  if (!requireNamespace("ggsci", quietly = TRUE)) {
    stop("Package 'ggsci' is required. Please install it.")
  }

  # Get the palette constructor function from ggsci namespace
  pal_constructor <- tryCatch(
    get(fun_name, envir = asNamespace("ggsci"), mode = "function"),
    error = function(e) {
      stop("Palette '", palette, "' not found in ggsci package")
    }
  )

  # Call the palette constructor to get the palette function
  pal_fun <- pal_constructor(alpha = alpha)

  # Default to the maximum number of colors if n is not specified
  if (is.null(n)) {
    n <- attr(pal_fun, "nlevels")
    if (is.null(n)) {
      n <- 10  # fallback default
    }
  }

  # Get the base colors
  colors <- pal_fun(n)

  # Apply shading if amount is provided
  if (!is.null(amount)) {
    colors <- shade_colors(colors, amount = amount)
  }

  # Display color swatch if requested
  if (show) {
    show_colors(colors)
  }

  # Return the color vector
  colors
}


#' @title AAAS journal color palettes
#' @rdname pal
#' @export
pal_aaas <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("aaas", n, alpha, amount, show)
}


#' @title Atlassian Design System palette
#' @rdname pal
#' @export
pal_atlassian <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("atlassian", n, alpha, amount, show)
}

#' @title BMJ color palettes
#' @rdname pal
#' @export
pal_bmj <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("bmj", n, alpha, amount, show)
}

#' @title Bootstrap 5 color palettes
#' @rdname pal
#' @export
pal_bs5 <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("bs5", n, alpha, amount, show)
}

#' @title COSMIC color palettes
#' @rdname pal
#' @export
pal_cosmic <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("cosmic", n, alpha, amount, show)
}

#' @title D3.js color palettes
#' @rdname pal
#' @export
pal_d3 <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("d3", n, alpha, amount, show)
}

#' @title Flat UI color palettes
#' @rdname pal
#' @export
pal_flatui <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("flatui", n, alpha, amount, show)
}

#' @title Frontiers journal color palettes
#' @rdname pal
#' @export
pal_frontiers <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("frontiers", n, alpha, amount, show)
}

#' @title Futurama color palettes
#' @rdname pal
#' @export
pal_futurama <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("futurama", n, alpha, amount, show)
}

#' @title Gephi color palettes
#' @rdname pal
#' @export
pal_gephi <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("gephi", n, alpha, amount, show)
}

#' @title The GSEA GenePattern color palettes
#' @rdname pal
#' @export
pal_gsea <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("gsea", n, alpha, amount, show)
}

#' @title Integrative Genomics Viewer (IGV) color palettes
#' @rdname pal
#' @export
pal_igv <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("igv", n, alpha, amount, show)
}

#' @title iTerm color palettes
#' @rdname pal
#' @export
pal_iterm <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("iterm", n, alpha, amount, show)
}

#' @title Journal of the American Medical Association color palettes
#' @rdname pal
#' @export
pal_jama <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("jama", n, alpha, amount, show)
}

#' @title Journal of Clinical Oncology color palettes
#' @rdname pal
#' @export
pal_jco <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("jco", n, alpha, amount, show)
}

#' @title Lancet journal color palettes
#' @rdname pal
#' @export
pal_lancet <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("lancet", n, alpha, amount, show)
}

#' @title LocusZoom color palette
#' @rdname pal
#' @export
pal_locuszoom <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("locuszoom", n, alpha, amount, show)
}

#' @title Material Design color palettes
#' @rdname pal
#' @export
pal_material <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("material", n, alpha, amount, show)
}

#' @title NEJM color palettes
#' @rdname pal
#' @export
pal_nejm <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("nejm", n, alpha, amount, show)
}

#' @title NPG journal color palettes
#' @rdname pal
#' @export
pal_npg <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("npg", n, alpha, amount, show)
}

#' @title Observable 10 color palette
#' @rdname pal
#' @export
pal_observable <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("observable", n, alpha, amount, show)
}

#' @title Primer design system palette
#' @rdname pal
#' @export
pal_primer <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("primer", n, alpha, amount, show)
}

#' @title Rick and Morty color palettes
#' @rdname pal
#' @export
pal_rickandmorty <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("rickandmorty", n, alpha, amount, show)
}

#' @title The Simpsons color palettes
#' @rdname pal
#' @export
pal_simpsons <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("simpsons", n, alpha, amount, show)
}

#' @title Star Trek color palettes
#' @rdname pal
#' @export
pal_startrek <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("startrek", n, alpha, amount, show)
}

#' @title Tron Legacy color palettes
#' @rdname pal
#' @export
pal_tron <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("tron", n, alpha, amount, show)
}

#' @title Tailwind CSS color palettes
#' @rdname pal
#' @export
pal_tw3 <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("tw3", n, alpha, amount, show)
}

#' @title The University of Chicago color palettes
#' @rdname pal
#' @export
pal_uchicago <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("uchicago", n, alpha, amount, show)
}

#' @title UCSC Genome Browser color palette
#' @rdname pal
#' @export
pal_ucscgb <- function(n = NULL, alpha = 1, amount = NULL, show = FALSE) {
  pal("ucscgb", n, alpha, amount, show)
}

#' @title JAMA color scale for ggplot2
#' @rdname scale_jama
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#'   Negative values lighten, positive values darken. Values must be between -1 and 1.
#'   Passed to \code{\link{shade_colors}}.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_jama <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_jama(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_jama
#' @export
scale_fill_jama <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_jama(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title NPG color scale for ggplot2
#' @rdname scale_npg
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_npg <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_npg(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_npg
#' @export
scale_fill_npg <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_npg(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Lancet color scale for ggplot2
#' @rdname scale_lancet
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_lancet <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_lancet(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_lancet
#' @export
scale_fill_lancet <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_lancet(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title NEJM color scale for ggplot2
#' @rdname scale_nejm
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_nejm <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_nejm(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_nejm
#' @export
scale_fill_nejm <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_nejm(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title AAAS color scale for ggplot2
#' @rdname scale_aaas
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_aaas <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_aaas(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_aaas
#' @export
scale_fill_aaas <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_aaas(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title JCO color scale for ggplot2
#' @rdname scale_jco
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_jco <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_jco(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_jco
#' @export
scale_fill_jco <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_jco(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title BMJ color scale for ggplot2
#' @rdname scale_bmj
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_bmj <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_bmj(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_bmj
#' @export
scale_fill_bmj <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_bmj(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title D3.js color scale for ggplot2
#' @rdname scale_d3
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_d3 <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_d3(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_d3
#' @export
scale_fill_d3 <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_d3(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Flat UI color scale for ggplot2
#' @rdname scale_flatui
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_flatui <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_flatui(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_flatui
#' @export
scale_fill_flatui <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_flatui(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Frontiers color scale for ggplot2
#' @rdname scale_frontiers
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_frontiers <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_frontiers(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_frontiers
#' @export
scale_fill_frontiers <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_frontiers(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Futurama color scale for ggplot2
#' @rdname scale_futurama
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_futurama <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_futurama(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_futurama
#' @export
scale_fill_futurama <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_futurama(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Simpsons color scale for ggplot2
#' @rdname scale_simpsons
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_simpsons <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_simpsons(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_simpsons
#' @export
scale_fill_simpsons <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_simpsons(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Rick and Morty color scale for ggplot2
#' @rdname scale_rickandmorty
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_rickandmorty <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_rickandmorty(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_rickandmorty
#' @export
scale_fill_rickandmorty <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_rickandmorty(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Star Trek color scale for ggplot2
#' @rdname scale_startrek
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_startrek <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_startrek(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_startrek
#' @export
scale_fill_startrek <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_startrek(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Tron Legacy color scale for ggplot2
#' @rdname scale_tron
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_tron <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_tron(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_tron
#' @export
scale_fill_tron <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_tron(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title UCSC Genome Browser color scale for ggplot2
#' @rdname scale_ucscgb
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_ucscgb <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_ucscgb(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_ucscgb
#' @export
scale_fill_ucscgb <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_ucscgb(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title UChicago color scale for ggplot2
#' @rdname scale_uchicago
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_uchicago <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_uchicago(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_uchicago
#' @export
scale_fill_uchicago <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_uchicago(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title IGV color scale for ggplot2
#' @rdname scale_igv
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_igv <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_igv(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_igv
#' @export
scale_fill_igv <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_igv(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title GSEA color scale for ggplot2
#' @rdname scale_gsea
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_gsea <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_gsea(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_gsea
#' @export
scale_fill_gsea <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_gsea(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Gephi color scale for ggplot2
#' @rdname scale_gephi
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_gephi <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_gephi(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_gephi
#' @export
scale_fill_gephi <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_gephi(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title iTerm color scale for ggplot2
#' @rdname scale_iterm
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_iterm <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_iterm(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_iterm
#' @export
scale_fill_iterm <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_iterm(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title LocusZoom color scale for ggplot2
#' @rdname scale_locuszoom
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_locuszoom <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_locuszoom(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_locuszoom
#' @export
scale_fill_locuszoom <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_locuszoom(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Material Design color scale for ggplot2
#' @rdname scale_material
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_material <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_material(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_material
#' @export
scale_fill_material <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_material(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Observable color scale for ggplot2
#' @rdname scale_observable
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_observable <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_observable(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_observable
#' @export
scale_fill_observable <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_observable(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Primer color scale for ggplot2
#' @rdname scale_primer
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_primer <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_primer(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_primer
#' @export
scale_fill_primer <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_primer(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title BS5 color scale for ggplot2
#' @rdname scale_bs5
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_bs5 <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_bs5(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_bs5
#' @export
scale_fill_bs5 <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_bs5(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Cosmic color scale for ggplot2
#' @rdname scale_cosmic
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_cosmic <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_cosmic(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_cosmic
#' @export
scale_fill_cosmic <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_cosmic(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Tailwind CSS v3 color scale for ggplot2
#' @rdname scale_tw3
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_tw3 <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_tw3(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_tw3
#' @export
scale_fill_tw3 <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_tw3(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @title Atlantis color scale for ggplot2
#' @rdname scale_atlassian
#' @param alpha Transparency level, 0 (transparent) to 1 (opaque)
#' @param amount Numeric vector for lightening/darkening colors.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_color_atlassian <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) pal_atlassian(n = n, alpha = alpha, amount = amount),
    ...
  )
}

#' @rdname scale_atlassian
#' @export
scale_fill_atlassian <- function(alpha = 1, amount = NULL, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) pal_atlassian(n = n, alpha = alpha, amount = amount),
    ...
  )
}

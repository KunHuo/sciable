#' Scientific / publication ggplot2 theme
#'
#' A flexible wrapper around [ggplot2::theme_bw()] tuned for compact,
#' journal-style figures. Most non-data appearance parameters are exposed
#' as arguments; anything not covered can still be overridden via `...`.
#'
#' @param palette          Palette name (e.g., "jama", "npg", "lancet") or a
#'   character vector of custom colors. See \code{\link{pal}} for all supported
#'   palette names. Set to NULL to skip setting default colors.
#' @param base.size        Base font size (points).
#' @param base.family      Base font family.
#' @param base.line.size   Default size for line elements (axis, ticks, borders).
#' @param base.rect.size   Default size for rect elements (borders, strips).
#' @param axis.text.size   Size for axis tick labels.
#' @param axis.title.size  Size for axis titles.
#' @param axis.text.color  Color for axis tick labels.
#' @param axis.title.color Color for axis titles.
#' @param axis.text.face   Font face for axis text ("plain", "bold", "italic", ...).
#' @param axis.title.face  Font face for axis titles.
#' @param axis.line.color  Color of axis lines.
#' @param axis.line.size   Size of axis lines.
#' @param axis.ticks.color Color of axis ticks.
#' @param axis.ticks.size  Size of axis ticks.
#' @param axis.ticks.length Length of axis ticks (grid units, e.g. cm).
#' @param show.axis.lines  Logical; draw axis lines.
#' @param show.axis.ticks  Logical; draw axis ticks.
#' @param x.ticks.length   Optional x-axis-specific tick length (overrides axis.ticks.length).
#' @param y.ticks.length   Optional y-axis-specific tick length (overrides axis.ticks.length).
#' @param panel.background Fill of panel background (use NA for transparent).
#' @param panel.border     Logical; draw panel border.
#' @param panel.border.color Color of panel border.
#' @param panel.border.size  Size of panel border.
#' @param show.grid.major  Logical; draw major gridlines.
#' @param show.grid.minor  Logical; draw minor gridlines.
#' @param grid.major.color Color of major gridlines.
#' @param grid.minor.color Color of minor gridlines.
#' @param grid.major.size  Size of major gridlines.
#' @param grid.minor.size  Size of minor gridlines.
#' @param grid.major.linetype Linetype for major gridlines.
#' @param grid.minor.linetype Linetype for minor gridlines.
#' @param panel.spacing    Spacing between facet panels (cm).
#' @param panel.spacing.x  Optional horizontal facet spacing (cm).
#' @param panel.spacing.y  Optional vertical facet spacing (cm).
#' @param panel.ontop      Logical; draw panel (gridlines) on top of geoms.
#' @param strip.background Fill color of facet strip background.
#' @param strip.color      Text color of facet strip labels.
#' @param strip.face       Font face of facet strip labels.
#' @param strip.size       Font size of facet strip labels.
#' @param strip.placement  "inside" or "outside".
#' @param legend.position  Legend position (e.g. "right", "bottom", "none", or c(x,y)).
#' @param legend.direction Direction of legend items ("horizontal"/"vertical").
#' @param legend.box       Arrangement of multiple legends ("horizontal"/"vertical").
#' @param legend.justification Justification for legend when positioned inside.
#' @param legend.key.size  Legend key size (lines).
#' @param legend.key.fill  Background fill of legend keys (NA = transparent).
#' @param legend.key.color Border color of legend keys.
#' @param legend.text.size Size of legend text.
#' @param legend.title.size Size of legend title.
#' @param legend.title.face Font face of legend title.
#' @param legend.margin    Margin around legend (cm, length-4 numeric).
#' @param legend.spacing   Spacing between legends (cm).
#' @param plot.title       Character; main title text (optional convenience).
#' @param plot.title.size  Size of plot title.
#' @param plot.title.face  Font face of plot title.
#' @param plot.title.hjust hjust of plot title.
#' @param plot.title.vjust vjust of plot title.
#' @param plot.title.position Position of plot title ("panel" or "plot").
#' @param plot.subtitle    Character; subtitle text (optional convenience).
#' @param subtitle.size    Size of subtitle.
#' @param plot.caption     Character; caption text (optional convenience).
#' @param caption.size     Size of caption.
#' @param caption.hjust    hjust of caption.
#' @param plot.caption.position Position of plot caption ("panel" or "plot").
#' @param plot.tag         Character; plot tag (e.g. "A"); optional convenience.
#' @param tag.size         Size of plot tag.
#' @param plot.margin      Numeric length-4 margin in cm: top, right, bottom, left.
#' @param plot.background  Fill of overall plot background (NA = transparent).
#' @param aspect.ratio     Optional numeric aspect ratio.
#' @param clip             Coordinate clipping. Either "on" (default) or "off".
#'                         When set to "off", plot elements (like text labels)
#'                         can extend beyond the panel boundaries.
#' @param complete         Logical; passed to [ggplot2::theme()] (usually FALSE).
#' @param ...              Additional elements passed to [ggplot2::theme()].
#' @return A ggplot2 theme object with an additional class for handling clip.
#' @export
theme_sci <- function(palette = "jama",
                      base.size = 7,
                      base.family = "sans",
                      base.line.size = 0.25,
                      base.rect.size = 0.25,
                      axis.text.size = base.size,
                      axis.title.size = base.size,
                      axis.text.color = "black",
                      axis.title.color = "black",
                      axis.text.face = "plain",
                      axis.title.face = "plain",
                      axis.line.color = "black",
                      axis.line.size = 0.25,
                      axis.ticks.color = "black",
                      axis.ticks.size = 0.25,
                      axis.ticks.length = 0.12,
                      show.axis.lines = TRUE,
                      show.axis.ticks = TRUE,
                      x.ticks.length = NULL,
                      y.ticks.length = NULL,
                      panel.background = NA,
                      panel.border = FALSE,
                      panel.border.color = "black",
                      panel.border.size = 0.25,
                      show.grid.major = FALSE,
                      show.grid.minor = FALSE,
                      grid.major.color = "gray90",
                      grid.minor.color = "gray90",
                      grid.major.size = 0.25,
                      grid.minor.size = 0.2,
                      grid.major.linetype = "solid",
                      grid.minor.linetype = "dashed",
                      panel.spacing = 0.6,
                      panel.spacing.x = NULL,
                      panel.spacing.y = NULL,
                      panel.ontop = FALSE,
                      strip.background = "gray90",
                      strip.color = "black",
                      strip.face = "plain",
                      strip.size = base.size,
                      strip.placement = "inside",
                      legend.position = "right",
                      legend.direction = "vertical",
                      legend.box = "vertical",
                      legend.justification = "center",
                      legend.key.size = 1.0,
                      legend.key.fill = NA,
                      legend.key.color = NA,
                      legend.text.size = base.size,
                      legend.title.size = base.size,
                      legend.title.face = "plain",
                      legend.margin = c(0, 0, 0, 0),
                      legend.spacing = 0.2,
                      plot.title = NULL,
                      plot.title.size = base.size + 2,
                      plot.title.face = "plain",
                      plot.title.hjust = 0,
                      plot.title.vjust = 1,
                      plot.title.position = "panel",
                      plot.subtitle = NULL,
                      subtitle.size = base.size,
                      plot.caption = NULL,
                      caption.size = base.size - 1,
                      caption.hjust = 1,
                      plot.caption.position = "panel",
                      plot.tag = NULL,
                      tag.size = base.size + 2,
                      plot.margin = c(0.2, 0.3, 0.2, 0.2),
                      plot.background = NA,
                      aspect.ratio = NULL,
                      clip = "off",
                      complete = FALSE,
                      ...) {
  # Resolve axis tick lengths (allow per-axis override)
  xtl <- if (is.null(x.ticks.length))
    axis.ticks.length
  else
    x.ticks.length
  ytl <- if (is.null(y.ticks.length))
    axis.ticks.length
  else
    y.ticks.length

  # Grid elements
  grid.major <- if (show.grid.major) {
    ggplot2::element_line(color = grid.major.color,
                          linewidth = grid.major.size,
                          linetype = grid.major.linetype)
  } else {
    ggplot2::element_blank()
  }

  grid.minor <- if (show.grid.minor) {
    ggplot2::element_line(color = grid.minor.color,
                          linewidth = grid.minor.size,
                          linetype = grid.minor.linetype)
  } else {
    ggplot2::element_blank()
  }

  # Panel border element
  panel.border.el <- if (panel.border) {
    ggplot2::element_rect(color = panel.border.color,
                          linewidth = panel.border.size,
                          fill = NA)
  } else {
    ggplot2::element_blank()
  }

  # Axis lines / ticks
  axis.line.el <- if (show.axis.lines) {
    ggplot2::element_line(color = axis.line.color,
                          linewidth = axis.line.size,
                          lineend = "square")
  } else {
    ggplot2::element_blank()
  }

  axis.ticks.el <- if (show.axis.ticks) {
    ggplot2::element_line(color = axis.ticks.color, linewidth = axis.ticks.size)
  } else {
    ggplot2::element_blank()
  }

  # Base theme
  th <- ggplot2::theme_bw(
    base_family  = base.family,
    base_line_size = base.line.size,
    base_rect_size = base.rect.size
  )

  # Resolve palette colors
  if (is.null(palette)) {
    pal_colors <- NULL
  } else if (is.character(palette) && length(palette) == 1 && !grepl("^#", palette)) {
    # Palette name string
    pal_colors <- pal(palette = palette)
  } else {
    # Custom color vector
    pal_colors <- palette
  }

  # Core custom theme
  th <- th + ggplot2::theme(
    # Panel
    panel.background = ggplot2::element_rect(fill = panel.background),
    panel.border     = panel.border.el,
    panel.ontop      = panel.ontop,
    panel.grid       = ggplot2::element_blank(),
    panel.grid.major = grid.major,
    panel.grid.minor = grid.minor,
    panel.spacing    = ggplot2::unit(panel.spacing, "cm"),
    panel.spacing.x  = if (!is.null(panel.spacing.x))
      ggplot2::unit(panel.spacing.x, "cm")
    else
      NULL,
    panel.spacing.y  = if (!is.null(panel.spacing.y))
      ggplot2::unit(panel.spacing.y, "cm")
    else
      NULL,

    # Axes
    axis.line        = axis.line.el,
    axis.ticks       = axis.ticks.el,
    axis.ticks.length.x = ggplot2::unit(xtl, "cm"),
    axis.ticks.length.y = ggplot2::unit(ytl, "cm"),
    axis.text        = ggplot2::element_text(
      color = axis.text.color,
      size = axis.text.size,
      face = axis.text.face
    ),
    axis.text.x      = ggplot2::element_text(
      color = axis.text.color,
      size = axis.text.size,
      face = axis.text.face
    ),
    axis.text.y      = ggplot2::element_text(
      color = axis.text.color,
      size = axis.text.size,
      face = axis.text.face
    ),
    axis.title       = ggplot2::element_text(
      color = axis.title.color,
      size = axis.title.size,
      face = axis.title.face
    ),
    axis.title.x     = ggplot2::element_text(
      color = axis.title.color,
      size = axis.title.size,
      face = axis.title.face
    ),
    axis.title.y     = ggplot2::element_text(
      color = axis.title.color,
      size = axis.title.size,
      face = axis.title.face
    ),

    # Strip / facets
    strip.background = ggplot2::element_rect(fill = strip.background, linewidth = base.rect.size),
    strip.placement  = strip.placement,
    strip.text       = ggplot2::element_text(
      color = strip.color,
      size = strip.size,
      face = strip.face
    ),
    strip.text.x     = ggplot2::element_text(
      color = strip.color,
      size = strip.size,
      face = strip.face
    ),
    strip.text.y     = ggplot2::element_text(
      color = strip.color,
      size = strip.size,
      face = strip.face
    ),

    # Legend
    legend.position        = legend.position,
    legend.direction       = legend.direction,
    legend.box             = legend.box,
    legend.justification   = legend.justification,
    legend.key             = ggplot2::element_rect(fill = legend.key.fill, color = legend.key.color),
    legend.key.size        = ggplot2::unit(legend.key.size, "lines"),
    legend.text            = ggplot2::element_text(color = "black", size = legend.text.size),
    legend.title           = ggplot2::element_text(size = legend.title.size, face = legend.title.face),
    legend.margin          = ggplot2::margin(
      legend.margin[1],
      legend.margin[2],
      legend.margin[3],
      legend.margin[4],
      "cm"
    ),
    legend.spacing        = ggplot2::unit(legend.spacing, "cm"),
    legend.background      = ggplot2::element_rect(fill = NA),

    # Plot-level text
    plot.title         = ggplot2::element_text(
      size = plot.title.size,
      face = plot.title.face,
      hjust = plot.title.hjust,
      vjust = plot.title.vjust
    ),
    plot.title.position = plot.title.position,
    plot.subtitle      = ggplot2::element_text(size = subtitle.size, face = "plain"),
    plot.caption       = ggplot2::element_text(size = caption.size, hjust = caption.hjust),
    plot.caption.position = plot.caption.position,
    plot.tag           = ggplot2::element_text(size = tag.size, face = "bold"),
    plot.background    = ggplot2::element_rect(fill = plot.background),
    plot.margin        = ggplot2::unit(plot.margin, "cm"),

    aspect.ratio = aspect.ratio,

    # Theme-side default palettes
    palette.colour.discrete = pal_colors,
    palette.colour.continuous = if (!is.null(pal_colors) && length(pal_colors) >= 2)
      pal_colors[c(2, 1)] else pal_colors,
    palette.fill.discrete = pal_colors,
    palette.fill.continuous = if (!is.null(pal_colors) && length(pal_colors) >= 2)
      pal_colors[c(2, 1)] else pal_colors,

    complete = complete
  )

  # Convenience labels (same as using labs(), but handy in-theme)
  if (!is.null(plot.title))
    th <- th + ggplot2::labs(title = plot.title)
  if (!is.null(plot.subtitle))
    th <- th + ggplot2::labs(subtitle = plot.subtitle)
  if (!is.null(plot.caption))
    th <- th + ggplot2::labs(caption = plot.caption)
  if (!is.null(plot.tag))
    th <- th + ggplot2::labs(tag = plot.tag)

  # Store clip parameter as attribute for later use in ggplot_add
  attr(th, "theme_sci_clip") <- clip

  # Add special class so ggplot_add dispatches our method
  class(th) <- c("theme_sci", class(th))

  # Allow user overrides last
  th + ggplot2::theme(...)
}


#' @importFrom ggplot2 ggplot_add
#' @keywords internal
#' @export
ggplot_add.theme_sci <- function(object, plot, ...) {
  # First, add the theme normally via the parent method
  plot <- NextMethod()

  # Retrieve the clip setting stored in the theme object
  clip <- attr(object, "theme_sci_clip")

  # Apply coord_cartesian with the stored clip setting
  # Skip only if clip is explicitly "on" (default ggplot2 behavior)
  if (!is.null(clip) && clip != "on") {
    plot <- plot + ggplot2::coord_cartesian(clip = clip)
  }

  plot
}


#' Turn off coordinate clipping
#'
#' Allows plot elements (like text labels, annotations) to extend
#' beyond the panel boundaries.
#'
#' @param clip Character, either "on" (default) or "off".
#' @return A coord_cartesian object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   geom_text(aes(label = rownames(mtcars)), hjust = 1.1) +
#'   theme_sci() +
#'   clip_off()
clip_off <- function(clip = "off") {
  ggplot2::coord_cartesian(clip = clip)
}


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
#' @param open.file Logical; if \code{TRUE}, opens the saved file using the
#'   system-default application. Cross-platform support for Windows (\code{shell.exec}),
#'   macOS (\code{open}), and Linux (\code{xdg-open}). Default \code{TRUE}.
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
                    open.file = TRUE,
                    ...) {
  # Apply font family if specified
  if (!is.null(family)) {
    if (inherits(plot, "patchwork")) {
      # Use patchwork's & operator to apply theme to all subplots
      plot <- plot &
        ggplot2::theme(text = ggplot2::element_text(family = family))
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

  if (open.file) {
    sys_name <- Sys.info()["sysname"]
    if (sys_name == "Windows") {
      shell.exec(normalizePath(path))
    } else if (sys_name == "Darwin") {
      system(paste0("open ", shQuote(normalizePath(path))))
    } else if (sys_name == "Linux") {
      system(paste0("xdg-open ", shQuote(normalizePath(path))))
    } else {
      warning("Unsupported operating system: cannot open file automatically.")
    }
  }

  invisible(normalizePath(path))
}


#' Rotate x-axis labels
#'
#' Rotate x-axis tick labels.
#'
#' @param angle Rotation angle in degrees.
#' @param hjust Horizontal justification.
#' @param vjust Vertical justification.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_point() +
#'   rotate_x_axis()
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_point() +
#'   rotate_x_axis(angle = 90)
rotate_x_axis <- function(angle = 45,
                          hjust = NULL,
                          vjust = NULL) {
  if (!is.numeric(angle) ||
      length(angle) != 1L ||
      is.na(angle)) {
    stop("`angle` must be a single numeric value.")
  }

  if (is.null(hjust)) {
    hjust <- if (angle > 0)
      1
    else
      0
  }

  if (is.null(vjust)) {
    vjust <- 1
  }

  ggplot2::theme(axis.text.x = ggplot2::element_text(
    angle = angle,
    hjust = hjust,
    vjust = vjust
  ))
}


#' Rotate y-axis labels
#'
#' Rotate y-axis tick labels.
#'
#' @param angle Rotation angle in degrees.
#' @param hjust Horizontal justification.
#' @param vjust Vertical justification.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(mpg, factor(cyl))) +
#'   ggplot2::geom_point() +
#'   rotate_y_axis()
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(mpg, factor(cyl))) +
#'   ggplot2::geom_point() +
#'   rotate_y_axis(angle = 45)
rotate_y_axis <- function(angle = 45,
                          hjust = NULL,
                          vjust = NULL) {
  if (!is.numeric(angle) ||
      length(angle) != 1L ||
      is.na(angle)) {
    stop("`angle` must be a single numeric value.")
  }

  if (!is.null(hjust)) {
    if (!is.numeric(hjust) ||
        length(hjust) != 1L ||
        is.na(hjust)) {
      stop("`hjust` must be NULL or a single numeric value.")
    }
  }

  if (!is.null(vjust)) {
    if (!is.numeric(vjust) ||
        length(vjust) != 1L ||
        is.na(vjust)) {
      stop("`vjust` must be NULL or a single numeric value.")
    }
  }

  if (is.null(hjust)) {
    hjust <- if (angle > 0)
      1
    else
      0
  }

  if (is.null(vjust)) {
    vjust <- 1
  }

  ggplot2::theme(axis.text.y = ggplot2::element_text(
    angle = angle,
    hjust = hjust,
    vjust = vjust
  ))
}


# Tag generator
generate_tags <- function(n, format = "A") {
  # If format is a custom vector, use it directly
  if (length(format) > 1) {
    # Only return what we have, no recycling
    if (n <= length(format)) {
      return(format[1:n])
    } else {
      warning("Custom tag vector is shorter than number of plots (",
              length(format), " < ", n,
              "). Remaining plots will not be tagged.")
      return(format)
    }
  }

  switch(format,
         "A"    = LETTERS[1:n],                     # A, B, C, ...
         "a"    = letters[1:n],                     # a, b, c, ...
         "(A)"  = paste0("(", LETTERS[1:n], ")"),   # (A), (B), (C), ...
         "(a)"  = paste0("(", letters[1:n], ")"),   # (a), (b), (c), ...
         "(1)"  = paste0("(", 1:n, ")"),            # (1), (2), (3), ...

         # Lowercase Greek letters
         "alpha" = {
           greek <- c("\u03B1", "\u03B2", "\u03B3", "\u03B4", "\u03B5",
                      "\u03B6", "\u03B7", "\u03B8", "\u03B9", "\u03BA",
                      "\u03BB", "\u03BC", "\u03BD", "\u03BE", "\u03BF",
                      "\u03C0", "\u03C1", "\u03C3", "\u03C4", "\u03C5",
                      "\u03C6", "\u03C7", "\u03C8", "\u03C9")
           if (n <= length(greek)) {
             greek[1:n]
           } else {
             warning("Only 24 Greek letters available, using numbers for extras")
             c(greek, as.character(25:n))
           }
         },

         # Uppercase Greek letters
         "Alpha" = {
           greek <- c("\u0391", "\u0392", "\u0393", "\u0394", "\u0395",
                      "\u0396", "\u0397", "\u0398", "\u0399", "\u039A",
                      "\u039B", "\u039C", "\u039D", "\u039E", "\u039F",
                      "\u03A0", "\u03A1", "\u03A3", "\u03A4", "\u03A5",
                      "\u03A6", "\u03A7", "\u03A8", "\u03A9")
           if (n <= length(greek)) {
             greek[1:n]
           } else {
             warning("Only 24 uppercase Greek letters available, using numbers for extras")
             c(greek, as.character(25:n))
           }
         },

         # Roman numerals
         "I" = {
           roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
                      "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX")
           if (n <= length(roman)) {
             roman[1:n]
           } else {
             warning("Roman numerals only up to XX, using numbers for extras")
             c(roman, as.character(21:n))
           }
         },

         # Default: plain numbers
         as.character(1:n)
  )
}


#' Combine multiple ggplot objects into a patchwork layout
#'
#' @param ... Multiple ggplot objects or a list containing ggplot objects.
#'   These will be flattened if nested lists are provided.
#' @param tags Character string or vector specifying plot tags.
#'   Options include:
#'   \itemize{
#'     \item \code{"A"} - Uppercase letters (A, B, C, ...)
#'     \item \code{"a"} - Lowercase letters (a, b, c, ...)
#'     \item \code{"(A)"} - Parenthesized uppercase ((A), (B), ...)
#'     \item \code{"(a)"} - Parenthesized lowercase ((a), (b), ...)
#'     \item \code{"(1)"} - Parenthesized numbers ((1), (2), ...)
#'     \item \code{"alpha"} - Lowercase Greek letters (\eqn{\alpha}, \eqn{\beta}, ...)
#'     \item \code{"Alpha"} - Uppercase Greek letters (\eqn{A}, \eqn{B}, ...)
#'     \item \code{"I"} - Roman numerals (I, II, III, ...)
#'     \item A custom character vector used directly as tags.
#'   }
#'   If a custom vector is shorter than the number of plots, only the first
#'   N plots receive tags. Remaining plots are left untagged.
#' @param titles Character string or vector specifying plot titles added via
#'   \code{ggtitle()}. A single string applies only to the first plot.
#'   A vector applies to the first N plots where N equals the vector length.
#'   If the vector is shorter than the number of plots, remaining plots
#'   receive no title.
#' @param ncol,nrow The dimensions of the grid to create - if both are NULL
#'   it will use the same logic as \code{facet_wrap()} to set the dimensions.
#' @param byrow Analogous to \code{byrow} in \code{matrix()}. If FALSE the
#'   plots will be filled in column-major order.
#' @param widths,heights The relative widths and heights of each column and
#'   row in the grid. Will get repeated to match the dimensions of the grid.
#'   The special value of \code{NA}/\code{null} will behave as \code{1null}
#'   unless a fixed aspect plot is inserted in which case it will allow the
#'   dimension to expand or contract to match the aspect ratio of the content.
#' @param guides A string specifying how guides should be treated in the
#'   layout. \code{'collect'} will collect guides below to the given nesting
#'   level, removing duplicates. \code{'keep'} will stop collection at this
#'   level and let guides be placed alongside their plot. \code{'auto'} will
#'   allow guides to be collected if an upper level tries, but place them
#'   alongside the plot if not. If you modify default guide "position" with
#'   \code{theme(legend.position=...)} while also collecting guides you must
#'   apply that change to the overall patchwork (see example).
#' @param design Specification of the location of areas in the layout. Can
#'   either be specified as a text string or by concatenating calls to
#'   \code{area()} together. See the examples for further information on use.
#' @param spacing_x Numeric value specifying the horizontal spacing between
#'   adjacent plots, measured in centimeters (cm). Only the gaps between
#'   plots are adjusted; the outer margins of the entire layout remain
#'   unchanged. For interior plots, half of the spacing is added to both
#'   the left and right sides. Edge plots receive spacing only on the inner
#'   side. Set to \code{NULL} (default) to keep the original plot margins.
#' @param spacing_y Numeric value specifying the vertical spacing between
#'   adjacent plots, measured in centimeters (cm). Only the gaps between
#'   plots are adjusted; the outer margins of the entire layout remain
#'   unchanged. For interior plots, half of the spacing is added to both
#'   the top and bottom sides. Edge plots receive spacing only on the inner
#'   side. Set to \code{NULL} (default) to keep the original plot margins.
#'
#' @return A patchwork object combining all input plots.
#'
#' @examples
#' library(ggplot2)
#'
#' # Sample plots
#' p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p2 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' p3 <- ggplot(mtcars, aes(hp)) + geom_histogram(bins = 20)
#' p4 <- ggplot(mtcars) + geom_bar(aes(gear)) + facet_wrap(~cyl)
#'
#' # Basic usage with automatic tags
#' arrange_plots(p1, p2, p3, tags = "A")
#'
#' # Custom tags shorter than plots
#' arrange_plots(p1, p2, p3, tags = c("A", "B"))
#'
#' # Single title applied to first plot only
#' arrange_plots(p1, p2, p3, titles = "Main Title")
#'
#' # Multiple titles
#' arrange_plots(p1, p2, p3,
#'   titles = c("Plot 1", "Plot 2", "Plot 3"))
#'
#' # Tags and titles together
#' arrange_plots(p1, p2, p3,
#'   tags = "(a)",
#'   titles = c("Weight vs MPG", "Cylinders vs MPG", "HP Distribution"))
#'
#' # Layout control
#' arrange_plots(p1, p2, p3, p4,
#'   tags = "A",
#'   ncol = 2,
#'   byrow = TRUE)
#'
#' # Custom design
#' arrange_plots(p1, p2, p3,
#'   design = "AB\nCC")
#'
#' # Adjust horizontal spacing between plots
#' arrange_plots(p1, p2, p3, p4,
#'   ncol = 2,
#'   spacing_x = 0.5)
#'
#' # Adjust both horizontal and vertical spacing
#' arrange_plots(p1, p2, p3, p4,
#'   ncol = 2,
#'   spacing_x = 0.3,
#'   spacing_y = 0.4)
#'
#' @export
arrange_plots <- function(...,
                          tags = NULL,
                          titles = NULL,
                          ncol = NULL,
                          nrow = NULL,
                          byrow = NULL,
                          widths = NULL,
                          heights = NULL,
                          guides = NULL,
                          design = NULL,
                          spacing_x = NULL,
                          spacing_y = NULL) {
  plots <- list(...)
  plots <- flatten_list(plots)
  n <- length(plots)

  if (n == 0) {
    stop("At least one plot object is required")
  }

  # Process tags
  if (!is.null(tags)) {
    tag_labels <- generate_tags(n, tags)
    m_tags <- min(length(tag_labels), n)

    for (i in seq_len(m_tags)) {
      plots[[i]] <- plots[[i]] + add_tag(tag_labels[i])
    }
  }

  # Process titles
  if (!is.null(titles)) {
    if (length(titles) == 1) {
      plots[[1]] <- plots[[1]] +  ggplot2::ggtitle(titles[1])
    } else {
      m_titles <- min(length(titles), n)
      if (m_titles < length(titles)) {
        warning("titles vector is longer than number of plots (",
                length(titles), " > ", n,
                "). Extra titles ignored.")
      }
      for (i in seq_len(m_titles)) {
        plots[[i]] <- plots[[i]] + ggplot2::ggtitle(titles[i])
      }
    }
  }

  # Determine grid dimensions if not explicitly provided
  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n))
    nrow <- ceiling(n / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }

  # Apply inter-plot spacing by adjusting inner margins only
  if (!is.null(spacing_x) || !is.null(spacing_y)) {
    for (i in seq_len(n)) {
      # Calculate row and column position (1-indexed)
      row_idx <- ceiling(i / ncol)
      col_idx <- i %% ncol
      if (col_idx == 0) col_idx <- ncol

      # Get existing plot margin or use defaults
      current_margin <- tryCatch(
        plots[[i]]$theme$plot.margin,
        error = function(e) NULL
      )

      if (is.null(current_margin)) {
        # Default margins in cm: top=0.2, right=0.3, bottom=0.2, left=0.2
        top <- 0.2
        right <- 0.3
        bottom <- 0.2
        left <- 0.2
      } else {
        top <- as.numeric(current_margin[1])
        right <- as.numeric(current_margin[2])
        bottom <- as.numeric(current_margin[3])
        left <- as.numeric(current_margin[4])
      }

      # Add spacing based on plot position in the grid
      if (!is.null(spacing_x)) {
        # Not in the rightmost column → add spacing to the right
        if (col_idx < ncol) {
          right <- right + spacing_x / 2
        }
        # Not in the leftmost column → add spacing to the left
        if (col_idx > 1) {
          left <- left + spacing_x / 2
        }
      }

      if (!is.null(spacing_y)) {
        # Not in the bottom row → add spacing to the bottom
        if (row_idx < nrow) {
          bottom <- bottom + spacing_y / 2
        }
        # Not in the top row → add spacing to the top
        if (row_idx > 1) {
          top <- top + spacing_y / 2
        }
      }

      # Apply the updated margin to the plot
      plots[[i]] <- plots[[i]] +
        ggplot2::theme(plot.margin = grid::unit(c(top, right, bottom, left), "cm"))
    }
  }

  # Combine all plots into a single patchwork layout
  patchwork::wrap_plots(plots,
                        ncol = ncol,
                        nrow = nrow,
                        byrow = byrow,
                        widths = widths,
                        heights = heights,
                        guides = guides,
                        design = design)
}

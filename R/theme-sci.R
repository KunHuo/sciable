#' Scientific / publication ggplot2 theme
#'
#' A flexible wrapper around [ggplot2::theme_bw()] tuned for compact,
#' journal-style figures. Most non-data appearance parameters are exposed
#' as arguments; anything not covered can still be overridden via `...`.
#'
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
#' @param complete         Logical; passed to [ggplot2::theme()] (usually FALSE).
#' @param ...              Additional elements passed to [ggplot2::theme()].
#' @return A ggplot2 theme object.
#' @export
theme_sci <- function(base.size = 7,
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
                      plot.title.position = "plot",
                      plot.subtitle = NULL,
                      subtitle.size = base.size,
                      plot.caption = NULL,
                      caption.size = base.size - 1,
                      caption.hjust = 1,
                      plot.caption.position = "plot",
                      plot.tag = NULL,
                      tag.size = base.size + 2,
                      plot.margin = c(0.4, 0.6, 0.4, 0.4),
                      plot.background = NA,
                      aspect.ratio = NULL,
                      complete = FALSE,
                      ...) {

  # Resolve axis tick lengths (allow per-axis override)
  xtl <- if (is.null(x.ticks.length)) axis.ticks.length else x.ticks.length
  ytl <- if (is.null(y.ticks.length)) axis.ticks.length else y.ticks.length

  # Grid elements
  grid.major <- if (show.grid.major) {
    ggplot2::element_line(color = grid.major.color,
                          size = grid.major.size,
                          linetype = grid.major.linetype)
  } else {
    ggplot2::element_blank()
  }

  grid.minor <- if (show.grid.minor) {
    ggplot2::element_line(color = grid.minor.color,
                          size = grid.minor.size,
                          linetype = grid.minor.linetype)
  } else {
    ggplot2::element_blank()
  }

  # Panel border element
  panel.border.el <- if (panel.border) {
    ggplot2::element_rect(color = panel.border.color,
                          size = panel.border.size,
                          fill = NA)
  } else {
    ggplot2::element_blank()
  }

  # Axis lines / ticks
  axis.line.el <- if (show.axis.lines) {
    ggplot2::element_line(color = axis.line.color, size = axis.line.size, lineend = "square")
  } else {
    ggplot2::element_blank()
  }

  axis.ticks.el <- if (show.axis.ticks) {
    ggplot2::element_line(color = axis.ticks.color, size = axis.ticks.size)
  } else {
    ggplot2::element_blank()
  }

  # Base theme
  th <- ggplot2::theme_bw(
    base_size    = base.size,
    base_family  = base.family,
    base_line_size = base.line.size,
    base_rect_size = base.rect.size
  )

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
    panel.spacing.x  = if (!is.null(panel.spacing.x)) ggplot2::unit(panel.spacing.x, "cm") else NULL,
    panel.spacing.y  = if (!is.null(panel.spacing.y)) ggplot2::unit(panel.spacing.y, "cm") else NULL,

    # Axes
    axis.line        = axis.line.el,
    axis.ticks       = axis.ticks.el,
    axis.ticks.length.x = ggplot2::unit(xtl, "cm"),
    axis.ticks.length.y = ggplot2::unit(ytl, "cm"),
    axis.text        = ggplot2::element_text(color = axis.text.color,
                                             size = axis.text.size,
                                             face = axis.text.face),
    axis.text.x      = ggplot2::element_text(color = axis.text.color,
                                             size = axis.text.size,
                                             face = axis.text.face),
    axis.text.y      = ggplot2::element_text(color = axis.text.color,
                                             size = axis.text.size,
                                             face = axis.text.face),
    axis.title       = ggplot2::element_text(color = axis.title.color,
                                             size = axis.title.size,
                                             face = axis.title.face),
    axis.title.x     = ggplot2::element_text(color = axis.title.color,
                                             size = axis.title.size,
                                             face = axis.title.face),
    axis.title.y     = ggplot2::element_text(color = axis.title.color,
                                             size = axis.title.size,
                                             face = axis.title.face),

    # Strip / facets
    strip.background = ggplot2::element_rect(fill = strip.background,
                                             size = base.rect.size),
    strip.placement  = strip.placement,
    strip.text       = ggplot2::element_text(color = strip.color,
                                             size = strip.size,
                                             face = strip.face),
    strip.text.x     = ggplot2::element_text(color = strip.color,
                                             size = strip.size,
                                             face = strip.face),
    strip.text.y     = ggplot2::element_text(color = strip.color,
                                             size = strip.size,
                                             face = strip.face),

    # Legend
    legend.position        = legend.position,
    legend.direction       = legend.direction,
    legend.box             = legend.box,
    legend.justification   = legend.justification,
    legend.key             = ggplot2::element_rect(fill = legend.key.fill,
                                                   color = legend.key.color),
    legend.key.size        = ggplot2::unit(legend.key.size, "lines"),
    legend.text            = ggplot2::element_text(color = "black",
                                                   size = legend.text.size),
    legend.title           = ggplot2::element_text(size = legend.title.size,
                                                   face = legend.title.face),
    legend.margin          = ggplot2::margin(legend.margin[1],
                                             legend.margin[2],
                                             legend.margin[3],
                                             legend.margin[4], "cm"),
    legend.spacing        = ggplot2::unit(legend.spacing, "cm"),
    legend.background      = ggplot2::element_rect(fill = NA),

    # Plot-level text
    plot.title         = ggplot2::element_text(size = plot.title.size,
                                               face = plot.title.face,
                                               hjust = plot.title.hjust,
                                               vjust = plot.title.vjust),
    plot.title.position = plot.title.position,
    plot.subtitle      = ggplot2::element_text(size = subtitle.size, face = "plain"),
    plot.caption       = ggplot2::element_text(size = caption.size, hjust = caption.hjust),
    plot.caption.position = plot.caption.position,
    plot.tag           = ggplot2::element_text(size = tag.size, face = "bold"),
    plot.background    = ggplot2::element_rect(fill = plot.background),
    plot.margin        = ggplot2::unit(plot.margin, "cm"),

    aspect.ratio = aspect.ratio,
    complete = complete
  )

  # Convenience labels (same as using labs(), but handy in-theme)
  if (!is.null(plot.title))   th <- th + ggplot2::labs(title = plot.title)
  if (!is.null(plot.subtitle)) th <- th + ggplot2::labs(subtitle = plot.subtitle)
  if (!is.null(plot.caption)) th <- th + ggplot2::labs(caption = plot.caption)
  if (!is.null(plot.tag))     th <- th + ggplot2::labs(tag = plot.tag)

  # Allow user overrides last
  th + ggplot2::theme(...)
}


#' Can be added directly to a ggplot with `+` operator.
#'
#' @param family Font family name. e.g., "serif", "sans", "Arial", "Times New Roman"
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   set_family("serif")
set_family <- function(family = "sans") {
  ggplot2::theme(text = ggplot2::element_text(family = family))
}

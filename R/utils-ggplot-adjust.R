# ============================================================================
# ggplot2 Theme Adjustment Helper Functions
#
# A comprehensive set of functions to quickly adjust individual theme elements.
# Each function only modifies the specified element; all other settings remain
# unchanged. NULL parameters are ignored.
# ============================================================================


# ==============================================================================
# 1. PLOT TITLE ELEMENTS
# ==============================================================================

#' Adjust main title appearance
#'
#' @param size Font size. Numeric. NULL to keep default.
#' @param face Font face. "plain", "bold", "italic", "bold.italic". NULL to keep default.
#' @param color Font color. NULL to keep default.
#' @param hjust Horizontal justification (0-1). NULL to keep default.
#' @param vjust Vertical justification. NULL to keep default.
#' @param family Font family. NULL to keep default.
#' @param margin Margin. A \code{margin()} object. NULL to keep default.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Change size and face
#' p + ggtitle("My Plot") + adjust_title(size = 16, face = "bold")
#'
#' # Change color and alignment
#' p + ggtitle("Centered Title") + adjust_title(color = "#2166AC", hjust = 0.5)
#'
#' # Add bottom margin
#' p + ggtitle("Title with Margin") + adjust_title(margin = margin(b = 15))
adjust_title <- function(size = NULL, face = NULL, color = NULL,
                         hjust = NULL, vjust = NULL, family = NULL,
                         margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(plot.title = element)
}

#' Adjust subtitle appearance
#'
#' @inheritParams adjust_title
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Change subtitle style
#' p + labs(subtitle = "A subtitle") + adjust_subtitle(size = 12, color = "#666666")
#'
#' # Italic subtitle
#' p + labs(subtitle = "Italic subtitle") + adjust_subtitle(face = "italic")
adjust_subtitle <- function(size = NULL, face = NULL, color = NULL,
                            hjust = NULL, vjust = NULL, family = NULL,
                            margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(plot.subtitle = element)
}

#' Adjust caption appearance
#'
#' @inheritParams adjust_title
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Right-aligned caption
#' p + labs(caption = "Data source") + adjust_caption(hjust = 1, color = "#999999")
#'
#' # Small italic caption
#' p + labs(caption = "Note: example data") + adjust_caption(size = 8, face = "italic")
adjust_caption <- function(size = NULL, face = NULL, color = NULL,
                           hjust = NULL, vjust = NULL, family = NULL,
                           margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(plot.caption = element)
}

#' Adjust tag appearance
#'
#' @inheritParams adjust_title
#' @param position Tag position. "topleft", "topright", "bottomleft",
#'   "bottomright", or numeric vector of length 2. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Bold tag at top-left
#' p + labs(tag = "A") + adjust_tag(size = 14, face = "bold")
#'
#' # Tag at top-right with custom color
#' p + labs(tag = "A") + adjust_tag(size = 16, color = "#D6604D", position = "topright")
adjust_tag <- function(size = NULL, face = NULL, color = NULL,
                       hjust = NULL, vjust = NULL, family = NULL,
                       margin = NULL, position = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )

  theme_args <- list(plot.tag = element)
  if (!is.null(position)) {
    theme_args$plot.tag.position <- position
  }
  do.call(ggplot2::theme, theme_args)
}

# ==============================================================================
# 2. AXIS TITLE ELEMENTS
# ==============================================================================

#' Adjust x-axis title appearance
#'
#' @inheritParams adjust_title
#' @param angle Text rotation angle. Numeric. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Bold x-axis title
#' p + labs(x = "Weight") + adjust_axis_title_x(face = "bold", size = 13)
#'
#' # Colored x-axis title
#' p + labs(x = "Weight (1000 lbs)") + adjust_axis_title_x(color = "#2166AC")
adjust_axis_title_x <- function(size = NULL, face = NULL, color = NULL,
                                hjust = NULL, vjust = NULL, family = NULL,
                                margin = NULL, angle = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family,
      margin = margin, angle = angle
    ))
  )
  ggplot2::theme(axis.title.x = element)
}

#' Adjust y-axis title appearance
#'
#' @inheritParams adjust_axis_title_x
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Bold y-axis title
#' p + labs(y = "MPG") + adjust_axis_title_y(face = "bold", size = 13)
#'
#' # Rotated y-axis title
#' p + labs(y = "Miles per Gallon") + adjust_axis_title_y(angle = 0, color = "#D6604D")
adjust_axis_title_y <- function(size = NULL, face = NULL, color = NULL,
                                hjust = NULL, vjust = NULL, family = NULL,
                                margin = NULL, angle = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family,
      margin = margin, angle = angle
    ))
  )
  ggplot2::theme(axis.title.y = element)
}

#' Adjust both axis titles appearance
#'
#' @inheritParams adjust_axis_title_x
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Bold both axis titles
#' p + labs(x = "Weight", y = "MPG") + adjust_axis_title(face = "bold", size = 13)
#'
#' # Consistent styling for both axes
#' p + labs(x = "Weight", y = "MPG") + adjust_axis_title(color = "#333333", size = 14)
adjust_axis_title <- function(size = NULL, face = NULL, color = NULL,
                              hjust = NULL, vjust = NULL, family = NULL,
                              margin = NULL, angle = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family,
      margin = margin, angle = angle
    ))
  )
  ggplot2::theme(
    axis.title.x = element,
    axis.title.y = element
  )
}

# ==============================================================================
# 3. AXIS TEXT (TICK LABELS) ELEMENTS
# ==============================================================================

#' Adjust x-axis text (tick labels) appearance
#'
#' @inheritParams adjust_axis_title_x
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Rotate x-axis labels
#' p + adjust_axis_text_x(angle = 45, hjust = 1)
#'
#' # Smaller x-axis labels
#' p + adjust_axis_text_x(size = 9, color = "#555555")
adjust_axis_text_x <- function(size = NULL, face = NULL, color = NULL,
                               hjust = NULL, vjust = NULL, family = NULL,
                               angle = NULL, margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family,
      angle = angle, margin = margin
    ))
  )
  ggplot2::theme(axis.text.x = element)
}

#' Adjust y-axis text (tick labels) appearance
#'
#' @inheritParams adjust_axis_title_x
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Larger y-axis labels
#' p + adjust_axis_text_y(size = 12, color = "#333333")
#'
#' # Bold y-axis labels
#' p + adjust_axis_text_y(face = "bold")
adjust_axis_text_y <- function(size = NULL, face = NULL, color = NULL,
                               hjust = NULL, vjust = NULL, family = NULL,
                               angle = NULL, margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family,
      angle = angle, margin = margin
    ))
  )
  ggplot2::theme(axis.text.y = element)
}

#' Adjust both axis text appearance
#'
#' @inheritParams adjust_axis_title_x
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Uniform axis text style
#' p + adjust_axis_text(size = 11, color = "#444444")
#'
#' # Bold axis text
#' p + adjust_axis_text(face = "bold")
adjust_axis_text <- function(size = NULL, face = NULL, color = NULL,
                             hjust = NULL, vjust = NULL, family = NULL,
                             angle = NULL, margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family,
      angle = angle, margin = margin
    ))
  )
  ggplot2::theme(
    axis.text.x = element,
    axis.text.y = element
  )
}

# ==============================================================================
# 4. AXIS LINE AND TICK ELEMENTS
# ==============================================================================

#' Adjust axis lines appearance
#'
#' @param color Line color. NULL to keep default.
#' @param linewidth Line width. Numeric. NULL to keep default.
#' @param linetype Line type. "solid", "dashed", etc. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Gray axis lines
#' p + adjust_axis_line(color = "#888888", linewidth = 0.5)
#'
#' # Dashed axis lines
#' p + adjust_axis_line(linetype = "dashed")
adjust_axis_line <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(
    axis.line.x = element,
    axis.line.y = element
  )
}

#' Adjust x-axis line appearance
#'
#' @inheritParams adjust_axis_line
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Only show x-axis line
#' p + adjust_axis_line_x(color = "#333333", linewidth = 0.8)
adjust_axis_line_x <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(axis.line.x = element)
}

#' Adjust y-axis line appearance
#'
#' @inheritParams adjust_axis_line
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Only show y-axis line
#' p + adjust_axis_line_y(color = "#333333", linewidth = 0.8)
adjust_axis_line_y <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(axis.line.y = element)
}

#' Adjust axis ticks appearance
#'
#' @inheritParams adjust_axis_line
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Thinner ticks
#' p + adjust_axis_ticks(linewidth = 0.3, color = "#888888")
adjust_axis_ticks <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(
    axis.ticks.x = element,
    axis.ticks.y = element
  )
}

#' Adjust x-axis ticks appearance
#'
#' @inheritParams adjust_axis_line
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Custom x-axis ticks
#' p + adjust_axis_ticks_x(color = "#2166AC", linewidth = 0.5)
adjust_axis_ticks_x <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(axis.ticks.x = element)
}

#' Adjust y-axis ticks appearance
#'
#' @inheritParams adjust_axis_line
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Custom y-axis ticks
#' p + adjust_axis_ticks_y(color = "#D6604D", linewidth = 0.5)
adjust_axis_ticks_y <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(axis.ticks.y = element)
}

# ==============================================================================
# 5. LEGEND ELEMENTS
# ==============================================================================

#' Adjust legend title appearance
#'
#' @inheritParams adjust_title
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Bold legend title
#' p + adjust_legend_title(face = "bold", size = 12)
#'
#' # Colored legend title
#' p + adjust_legend_title(color = "#2166AC")
adjust_legend_title <- function(size = NULL, face = NULL, color = NULL,
                                hjust = NULL, vjust = NULL, family = NULL,
                                margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(legend.title = element)
}

#' Adjust legend text appearance
#'
#' @inheritParams adjust_title
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Smaller legend text
#' p + adjust_legend_text(size = 10)
#'
#' # Italic legend text
#' p + adjust_legend_text(face = "italic")
adjust_legend_text <- function(size = NULL, face = NULL, color = NULL,
                               hjust = NULL, vjust = NULL, family = NULL,
                               margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(legend.text = element)
}

#' Adjust legend key appearance
#'
#' @param fill Fill color. NULL to keep default.
#' @param color Border color. NULL to keep default.
#' @param linewidth Border line width. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Light gray legend key background
#' p + adjust_legend_key(fill = "#F5F5F5")
#'
#' # White key with border
#' p + adjust_legend_key(fill = "white", color = "#CCCCCC")
adjust_legend_key <- function(fill = NULL, color = NULL, linewidth = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color, linewidth = linewidth
    ))
  )
  ggplot2::theme(legend.key = element)
}

#' Adjust legend box background
#'
#' @param fill Fill color. NULL to keep default.
#' @param color Border color. NULL to keep default.
#' @param linewidth Border line width. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # White legend box with border
#' p + adjust_legend_box(fill = "white", color = "#CCCCCC", linewidth = 0.5)
#'
#' # Transparent legend box
#' p + adjust_legend_box(fill = NA)
adjust_legend_box <- function(fill = NULL, color = NULL, linewidth = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color, linewidth = linewidth
    ))
  )
  ggplot2::theme(legend.box.background = element)
}

#' Adjust legend position
#'
#' @param position Legend position. "none", "left", "right", "bottom",
#'   "top", or a numeric vector of length 2. NULL to keep default.
#' @param justification Anchor point for positioning. NULL to keep default.
#' @param direction Layout direction. "horizontal" or "vertical". NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Move legend to bottom
#' p + adjust_legend_position(position = "bottom")
#'
#' # Inside plot at top-right
#' p + adjust_legend_position(position = c(0.85, 0.8))
#'
#' # Remove legend
#' p + adjust_legend_position(position = "none")
adjust_legend_position <- function(position = NULL, justification = NULL,
                                   direction = NULL) {
  theme_args <- list()
  if (!is.null(position)) theme_args$legend.position <- position
  if (!is.null(justification)) theme_args$legend.justification <- justification
  if (!is.null(direction)) theme_args$legend.direction <- direction
  do.call(ggplot2::theme, theme_args)
}

# ==============================================================================
# 6. STRIP (FACET LABEL) ELEMENTS
# ==============================================================================

#' Adjust strip text (facet labels) appearance
#'
#' @inheritParams adjust_title
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ cyl)
#'
#' # Bold facet labels
#' p + adjust_strip_text(face = "bold", size = 12)
#'
#' # Colored facet labels
#' p + adjust_strip_text(color = "#2166AC")
adjust_strip_text <- function(size = NULL, face = NULL, color = NULL,
                              hjust = NULL, vjust = NULL, family = NULL,
                              margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(strip.text = element)
}

#' Adjust x-direction strip text appearance
#'
#' @inheritParams adjust_title
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#'
#' # Style x-direction facet labels
#' p + adjust_strip_text_x(face = "bold", color = "#2166AC")
adjust_strip_text_x <- function(size = NULL, face = NULL, color = NULL,
                                hjust = NULL, vjust = NULL, family = NULL,
                                margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(strip.text.x = element)
}

#' Adjust y-direction strip text appearance
#'
#' @inheritParams adjust_title
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#'
#' # Style y-direction facet labels
#' p + adjust_strip_text_y(face = "bold", color = "#D6604D")
adjust_strip_text_y <- function(size = NULL, face = NULL, color = NULL,
                                hjust = NULL, vjust = NULL, family = NULL,
                                margin = NULL) {
  element <- do.call(
    ggplot2::element_text,
    Filter(Negate(is.null), list(
      size = size, face = face, colour = color,
      hjust = hjust, vjust = vjust, family = family, margin = margin
    ))
  )
  ggplot2::theme(strip.text.y = element)
}

#' Adjust strip background appearance
#'
#' @param fill Fill color. NULL to keep default.
#' @param color Border color. NULL to keep default.
#' @param linewidth Border line width. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ cyl)
#'
#' # Light gray strip background
#' p + adjust_strip_background(fill = "#F0F0F0", color = "#CCCCCC")
#'
#' # White strip background
#' p + adjust_strip_background(fill = "white")
adjust_strip_background <- function(fill = NULL, color = NULL, linewidth = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color, linewidth = linewidth
    ))
  )
  ggplot2::theme(strip.background = element)
}

#' Adjust x-direction strip background appearance
#'
#' @inheritParams adjust_strip_background
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#'
#' # Style x-direction strip background
#' p + adjust_strip_background_x(fill = "#E8F0FE")
adjust_strip_background_x <- function(fill = NULL, color = NULL, linewidth = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color, linewidth = linewidth
    ))
  )
  ggplot2::theme(strip.background.x = element)
}

#' Adjust y-direction strip background appearance
#'
#' @inheritParams adjust_strip_background
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#'
#' # Style y-direction strip background
#' p + adjust_strip_background_y(fill = "#FDE8E8")
adjust_strip_background_y <- function(fill = NULL, color = NULL, linewidth = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color, linewidth = linewidth
    ))
  )
  ggplot2::theme(strip.background.y = element)
}

# ==============================================================================
# 7. PANEL ELEMENTS
# ==============================================================================

#' Adjust panel background appearance
#'
#' @param fill Fill color. NULL to keep default.
#' @param color Border color. NULL to keep default.
#' @param linewidth Border line width. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # White panel background
#' p + adjust_panel_background(fill = "white")
#'
#' # Light gray panel
#' p + adjust_panel_background(fill = "#F9F9F9")
adjust_panel_background <- function(fill = NULL, color = NULL, linewidth = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color, linewidth = linewidth
    ))
  )
  ggplot2::theme(panel.background = element)
}

#' Adjust panel border appearance
#'
#' @param fill Fill color. Use NA for transparent. NULL to keep default.
#' @param color Border color. NULL to keep default.
#' @param linewidth Border line width. NULL to keep default.
#' @param linetype Line type. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Add panel border
#' p + adjust_panel_border(color = "#CCCCCC", linewidth = 0.5)
#'
#' # Dark border
#' p + adjust_panel_border(color = "#333333", linewidth = 0.8)
adjust_panel_border <- function(fill = NULL, color = NULL,
                                linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color,
      linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(panel.border = element)
}

#' Adjust major grid lines appearance
#'
#' @param color Line color. NULL to keep default.
#' @param linewidth Line width. NULL to keep default.
#' @param linetype Line type. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Lighter grid lines
#' p + adjust_panel_grid_major(color = "#EEEEEE", linewidth = 0.3)
#'
#' # Dashed grid lines
#' p + adjust_panel_grid_major(linetype = "dashed", color = "#CCCCCC")
adjust_panel_grid_major <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(
    panel.grid.major.x = element,
    panel.grid.major.y = element
  )
}

#' Adjust minor grid lines appearance
#'
#' @inheritParams adjust_panel_grid_major
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Very light minor grid lines
#' p + adjust_panel_grid_minor(color = "#F5F5F5", linewidth = 0.2)
#'
#' # Remove minor grid lines
#' p + adjust_panel_grid_minor(color = NA)
adjust_panel_grid_minor <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(
    panel.grid.minor.x = element,
    panel.grid.minor.y = element
  )
}

#' Adjust x-direction major grid lines appearance
#'
#' @inheritParams adjust_panel_grid_major
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Custom x-direction grid
#' p + adjust_panel_grid_major_x(color = "#EEEEEE", linewidth = 0.3)
adjust_panel_grid_major_x <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(panel.grid.major.x = element)
}

#' Adjust y-direction major grid lines appearance
#'
#' @inheritParams adjust_panel_grid_major
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Custom y-direction grid
#' p + adjust_panel_grid_major_y(color = "#EEEEEE", linewidth = 0.3)
adjust_panel_grid_major_y <- function(color = NULL, linewidth = NULL, linetype = NULL) {
  element <- do.call(
    ggplot2::element_line,
    Filter(Negate(is.null), list(
      colour = color, linewidth = linewidth, linetype = linetype
    ))
  )
  ggplot2::theme(panel.grid.major.y = element)
}

# ==============================================================================
# 8. PLOT BACKGROUND AND MARGIN
# ==============================================================================

#' Adjust plot background appearance
#'
#' @param fill Fill color. NULL to keep default.
#' @param color Border color. NULL to keep default.
#' @param linewidth Border line width. NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # White plot background
#' p + adjust_plot_background(fill = "white")
#'
#' # Light colored background with border
#' p + adjust_plot_background(fill = "#FFF9F0", color = "#CCCCCC")
adjust_plot_background <- function(fill = NULL, color = NULL, linewidth = NULL) {
  element <- do.call(
    ggplot2::element_rect,
    Filter(Negate(is.null), list(
      fill = fill, colour = color, linewidth = linewidth
    ))
  )
  ggplot2::theme(plot.background = element)
}

#' Adjust plot margin
#'
#' @param top Top margin. Numeric. NULL to keep default.
#' @param right Right margin. Numeric. NULL to keep default.
#' @param bottom Bottom margin. Numeric. NULL to keep default.
#' @param left Left margin. Numeric. NULL to keep default.
#' @param unit Unit for margins. Default "pt". NULL to keep default.
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Add space around plot
#' p + adjust_plot_margin(top = 20, right = 20, bottom = 20, left = 20)
#'
#' # Asymmetric margins
#' p + adjust_plot_margin(top = 30, bottom = 10)
adjust_plot_margin <- function(top = NULL, right = NULL,
                               bottom = NULL, left = NULL,
                               unit = "pt") {
  margins <- c(top, right, bottom, left)
  if (any(!is.null(margins))) {
    margins[is.null(margins)] <- 0
    margin_obj <- do.call(ggplot2::margin, as.list(c(margins, unit = unit)))
    ggplot2::theme(plot.margin = margin_obj)
  } else {
    ggplot2::theme()
  }
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
#'   adjust_family("serif")
adjust_family <- function(family = "sans") {
  ggplot2::theme(text = ggplot2::element_text(family = family))
}

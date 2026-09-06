# ============================================================================
# ggplot2 Element Removal Functions
#
# A comprehensive set of functions to remove specific ggplot2 theme elements.
# Each function removes a particular element by setting it to element_blank().
# ============================================================================


# ==============================================================================
# 1. PLOT TITLE ELEMENTS
# ==============================================================================

#' Remove main title
#'
#' @return A ggplot2 theme object with title removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + ggtitle("My Title")
#' p + remove_title()
remove_title <- function() {
  ggplot2::theme(plot.title = ggplot2::element_blank())
}

#' Remove subtitle
#'
#' @return A ggplot2 theme object with subtitle removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(subtitle = "Subtitle")
#' p + remove_subtitle()
remove_subtitle <- function() {
  ggplot2::theme(plot.subtitle = ggplot2::element_blank())
}

#' Remove caption
#'
#' @return A ggplot2 theme object with caption removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(caption = "Caption")
#' p + remove_caption()
remove_caption <- function() {
  ggplot2::theme(plot.caption = ggplot2::element_blank())
}

#' Remove tag
#'
#' @return A ggplot2 theme object with tag removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(tag = "A")
#' p + remove_tag()
remove_tag <- function() {
  ggplot2::theme(plot.tag = ggplot2::element_blank())
}

# ==============================================================================
# 2. AXIS ELEMENTS
# ==============================================================================

#' Remove x-axis title
#'
#' @return A ggplot2 theme object with x-axis title removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_title_x()
remove_axis_title_x <- function() {
  ggplot2::theme(axis.title.x = ggplot2::element_blank())
}

#' Remove y-axis title
#'
#' @return A ggplot2 theme object with y-axis title removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_title_y()
remove_axis_title_y <- function() {
  ggplot2::theme(axis.title.y = ggplot2::element_blank())
}

#' Remove both axis titles
#'
#' @return A ggplot2 theme object with both axis titles removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_title()
remove_axis_title <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  )
}

#' Remove x-axis text (tick labels)
#'
#' @return A ggplot2 theme object with x-axis text removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_text_x()
remove_axis_text_x <- function() {
  ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

#' Remove y-axis text (tick labels)
#'
#' @return A ggplot2 theme object with y-axis text removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_text_y()
remove_axis_text_y <- function() {
  ggplot2::theme(axis.text.y = ggplot2::element_blank())
}

#' Remove both axis text (tick labels)
#'
#' @return A ggplot2 theme object with both axis text removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_text()
remove_axis_text <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  )
}

#' Remove x-axis ticks
#'
#' @return A ggplot2 theme object with x-axis ticks removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_ticks_x()
remove_axis_ticks_x <- function() {
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
}

#' Remove y-axis ticks
#'
#' @return A ggplot2 theme object with y-axis ticks removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_ticks_y()
remove_axis_ticks_y <- function() {
  ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
}

#' Remove both axis ticks
#'
#' @return A ggplot2 theme object with both axis ticks removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_ticks()
remove_axis_ticks <- function() {
  ggplot2::theme(
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
}

#' Remove x-axis line
#'
#' @return A ggplot2 theme object with x-axis line removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_line_x()
remove_axis_line_x <- function() {
  ggplot2::theme(axis.line.x = ggplot2::element_blank())
}

#' Remove y-axis line
#'
#' @return A ggplot2 theme object with y-axis line removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_line_y()
remove_axis_line_y <- function() {
  ggplot2::theme(axis.line.y = ggplot2::element_blank())
}

#' Remove both axis lines
#'
#' @return A ggplot2 theme object with both axis lines removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_line()
remove_axis_line <- function() {
  ggplot2::theme(
    axis.line.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank()
  )
}

#' Remove entire x-axis (title, text, ticks, line)
#'
#' @return A ggplot2 theme object with entire x-axis removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_x()
remove_axis_x <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank()
  )
}

#' Remove entire y-axis (title, text, ticks, line)
#'
#' @return A ggplot2 theme object with entire y-axis removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis_y()
remove_axis_y <- function() {
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank()
  )
}

#' Remove both axes completely
#'
#' @return A ggplot2 theme object with both axes removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_axis()
remove_axis <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank()
  )
}

# ==============================================================================
# 3. LEGEND ELEMENTS
# ==============================================================================

#' Remove legend title
#'
#' @return A ggplot2 theme object with legend title removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#' p + remove_legend_title()
remove_legend_title <- function() {
  ggplot2::theme(legend.title = ggplot2::element_blank())
}

#' Remove legend text
#'
#' @return A ggplot2 theme object with legend text removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#' p + remove_legend_text()
remove_legend_text <- function() {
  ggplot2::theme(legend.text = ggplot2::element_blank())
}

#' Remove legend key background
#'
#' @return A ggplot2 theme object with legend key removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#' p + remove_legend_key()
remove_legend_key <- function() {
  ggplot2::theme(legend.key = ggplot2::element_blank())
}

#' Remove legend box background
#'
#' @return A ggplot2 theme object with legend box background removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#' p + remove_legend_box()
remove_legend_box <- function() {
  ggplot2::theme(legend.box.background = ggplot2::element_blank())
}

#' Remove entire legend
#'
#' @return A ggplot2 theme object with legend removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#' p + remove_legend()
remove_legend <- function() {
  ggplot2::theme(legend.position = "none")
}

# ==============================================================================
# 4. STRIP (FACET LABEL) ELEMENTS
# ==============================================================================

#' Remove strip text (facet labels)
#'
#' @return A ggplot2 theme object with strip text removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ cyl)
#' p + remove_strip_text()
remove_strip_text <- function() {
  ggplot2::theme(strip.text = ggplot2::element_blank())
}

#' Remove x-direction strip text
#'
#' @return A ggplot2 theme object with x-direction strip text removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#' p + remove_strip_text_x()
remove_strip_text_x <- function() {
  ggplot2::theme(strip.text.x = ggplot2::element_blank())
}

#' Remove y-direction strip text
#'
#' @return A ggplot2 theme object with y-direction strip text removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#' p + remove_strip_text_y()
remove_strip_text_y <- function() {
  ggplot2::theme(strip.text.y = ggplot2::element_blank())
}

#' Remove strip background
#'
#' @return A ggplot2 theme object with strip background removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ cyl)
#' p + remove_strip_background()
remove_strip_background <- function() {
  ggplot2::theme(strip.background = ggplot2::element_blank())
}

#' Remove x-direction strip background
#'
#' @return A ggplot2 theme object with x-direction strip background removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#' p + remove_strip_background_x()
remove_strip_background_x <- function() {
  ggplot2::theme(strip.background.x = ggplot2::element_blank())
}

#' Remove y-direction strip background
#'
#' @return A ggplot2 theme object with y-direction strip background removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ cyl)
#' p + remove_strip_background_y()
remove_strip_background_y <- function() {
  ggplot2::theme(strip.background.y = ggplot2::element_blank())
}

#' Remove entire strip (text and background)
#'
#' @return A ggplot2 theme object with entire strip removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ cyl)
#' p + remove_strip()
remove_strip <- function() {
  ggplot2::theme(
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )
}

# ==============================================================================
# 5. PANEL ELEMENTS
# ==============================================================================

#' Remove panel background
#'
#' @return A ggplot2 theme object with panel background removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_panel_background()
remove_panel_background <- function() {
  ggplot2::theme(panel.background = ggplot2::element_blank())
}

#' Remove panel border
#'
#' @return A ggplot2 theme object with panel border removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_panel_border()
remove_panel_border <- function() {
  ggplot2::theme(panel.border = ggplot2::element_blank())
}

#' Remove major grid lines
#'
#' @return A ggplot2 theme object with major grid lines removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_grid_major()
remove_grid_major <- function() {
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank()
  )
}

#' Remove minor grid lines
#'
#' @return A ggplot2 theme object with minor grid lines removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_grid_minor()
remove_grid_minor <- function() {
  ggplot2::theme(
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )
}

#' Remove all grid lines (major and minor)
#'
#' @return A ggplot2 theme object with all grid lines removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_grid()
remove_grid <- function() {
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )
}

#' Remove x-direction major grid lines
#'
#' @return A ggplot2 theme object with x-direction major grid lines removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_grid_major_x()
remove_grid_major_x <- function() {
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

#' Remove y-direction major grid lines
#'
#' @return A ggplot2 theme object with y-direction major grid lines removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_grid_major_y()
remove_grid_major_y <- function() {
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Remove entire panel (background, border, grid)
#'
#' @return A ggplot2 theme object with entire panel decorations removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_panel()
remove_panel <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )
}

# ==============================================================================
# 6. PLOT BACKGROUND
# ==============================================================================

#' Remove plot background
#'
#' @return A ggplot2 theme object with plot background removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_plot_background()
remove_plot_background <- function() {
  ggplot2::theme(plot.background = ggplot2::element_blank())
}

# ==============================================================================
# 7. COMPOSITE REMOVAL FUNCTIONS
# ==============================================================================

#' Remove all non-data ink (minimal theme)
#'
#' Removes background, grid lines, panel border, axis lines, and axis ticks.
#' Keeps axis titles and text.
#'
#' @return A ggplot2 theme object with minimal non-data ink.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_non_data_ink()
remove_non_data_ink <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank()
  )
}

#' Create a completely blank theme
#'
#' Removes all theme elements including axes, legends, strips, panels,
#' backgrounds, titles, etc. Only the data remains visible.
#'
#' @return A ggplot2 theme object with everything removed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + remove_all()
remove_all <- function() {
  ggplot2::theme(
    # Titles
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank(),
    plot.tag = ggplot2::element_blank(),

    # Axes
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),

    # Legend
    legend.position = "none",

    # Strips
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),

    # Panel
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),

    # Background
    plot.background = ggplot2::element_blank()
  )
}

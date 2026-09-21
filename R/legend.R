#' Customize legend title appearance and labeling
#'
#' @param title Character string for legend title, or NULL to skip.
#' @param size Numeric font size for title, or NULL.
#' @param face Font face ("plain", "bold", "italic", etc.), or NULL.
#' @param color Font color for title, or NULL.
#' @param angle Numeric rotation angle in degrees, or NULL.
#' @param hjust Horizontal justification (0-1), or NULL.
#' @param vjust Vertical justification (0-1), or NULL.
#' @param family Font family name, or NULL.
#' @param margin Margin around title (numeric vector), or NULL.
#' @param lineheight Line height multiplier, or NULL.
#' @param direction Legend direction ("horizontal" or "vertical"), or NULL.
#' @param position Legend position ("top", "bottom", "left", "right"), or NULL.
#' @param aesthetic Aesthetic type: "all", "color", "fill", "colour", "shape",
#'   "size", "alpha", "linetype", or "group". Default "all".
#'
#' @return List of ggplot2 theme/labs components for modifying legend title.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Change legend title and make it bold
#' p + legend_title(title = "Cylinders", size = 12, face = "bold")
#'
#' # Change title for color aesthetic only
#' p + legend_title(title = "Cyl Count", aesthetic = "color")
#'
#' # Move legend to bottom with horizontal direction
#' p + legend_title(title = "Cyl", position = "bottom", direction = "horizontal")
#'
#' @export
legend_title <- function(title = NULL,
                         size = NULL,
                         face = NULL,
                         color = NULL,
                         angle = NULL,
                         hjust = NULL,
                         vjust = NULL,
                         family = NULL,
                         margin = NULL,
                         lineheight = NULL,
                         direction = NULL,
                         position = NULL,
                         aesthetic = c("all",
                                       "color",
                                       "fill",
                                       "colour",
                                       "shape",
                                       "size",
                                       "alpha",
                                       "linetype",
                                       "group")) {
  aesthetic <- match.arg(aesthetic)
  components <- list()

  if (!is.null(title)) {
    if (aesthetic == "all") {
      components <- c(components, list(ggplot2::labs(color = title, fill = title)))
    } else {
      labs_args <- list(title)
      names(labs_args) <- aesthetic
      components <- c(components, list(do.call(ggplot2::labs, labs_args)))
    }
  }

  text_args <- list()
  if (!is.null(size))
    text_args$size <- size
  if (!is.null(face))
    text_args$face <- face
  if (!is.null(color))
    text_args$color <- color
  if (!is.null(angle))
    text_args$angle <- angle
  if (!is.null(hjust))
    text_args$hjust <- hjust
  if (!is.null(vjust))
    text_args$vjust <- vjust
  if (!is.null(family))
    text_args$family <- family
  if (!is.null(margin))
    text_args$margin <- margin
  if (!is.null(lineheight))
    text_args$lineheight <- lineheight

  if (length(text_args) > 0) {
    components <- c(components, list(ggplot2::theme(
      legend.title = do.call(ggplot2::element_text, text_args)
    )))
  }

  if (!is.null(direction)) {
    components <- c(components, list(ggplot2::theme(legend.direction = direction)))
  }
  if (!is.null(position)) {
    components <- c(components, list(ggplot2::theme(legend.position = position)))
  }

  return(components)
}

#' Customize legend key dimensions and spacing
#'
#' @param width Numeric key width in cm, or NULL.
#' @param height Numeric key height in cm, or NULL.
#' @param size Numeric key size (overrides width/height if set), or NULL.
#' @param spacing_x Numeric horizontal spacing between keys in cm, or NULL.
#' @param spacing_y Numeric vertical spacing between keys in cm, or NULL.
#'
#' @return List of ggplot2 theme components for legend key styling.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Make legend keys wider and taller
#' p + legend_key(width = 1.5, height = 1.5)
#'
#' # Increase spacing between legend items
#' p + legend_key(spacing_x = 1, spacing_y = 0.5)
#'
#' # Uniform key size
#' p + legend_key(size = 2)
#'
#' @export
legend_key <- function(width = NULL,
                       height = NULL,
                       size = NULL,
                       spacing_x = NULL,
                       spacing_y = NULL) {
  components <- list()

  if (!is.null(width)) {
    components <- c(components, list(ggplot2::theme(
      legend.key.width = ggplot2::unit(width, "cm")
    )))
  }
  if (!is.null(height)) {
    components <- c(components, list(ggplot2::theme(
      legend.key.height = ggplot2::unit(height, "cm")
    )))
  }
  if (!is.null(size)) {
    components <- c(components, list(ggplot2::theme(legend.key.size = ggplot2::unit(size, "cm"))))
  }
  if (!is.null(spacing_x)) {
    components <- c(components, list(ggplot2::theme(
      legend.spacing.x = ggplot2::unit(spacing_x, "cm")
    )))
  }
  if (!is.null(spacing_y)) {
    components <- c(components, list(ggplot2::theme(
      legend.spacing.y = ggplot2::unit(spacing_y, "cm")
    )))
  }

  return(components)
}

#' Relabel legend entries for discrete scales
#'
#' @param labels Character vector of new label names, or NULL.
#' @param aesthetic Aesthetic type: "all", "color", "fill", "colour", "shape",
#'   "size", "alpha", "linetype", or "group". Default "all".
#'
#' @return List of ggplot2 scale functions with updated labels.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Rename all legend labels
#' p + legend_text(labels = c("4-Cyl", "6-Cyl", "8-Cyl"))
#'
#' # Rename only color aesthetic labels
#' p + legend_text(labels = c("Four", "Six", "Eight"), aesthetic = "color")
#'
#' @export
legend_text <- function(labels = NULL,
                        aesthetic = c("all",
                                      "color",
                                      "fill",
                                      "colour",
                                      "shape",
                                      "size",
                                      "alpha",
                                      "linetype",
                                      "group")) {
  aesthetic <- match.arg(aesthetic)
  components <- list()

  if (!is.null(labels)) {
    if (aesthetic == "all") {
      components <- c(components, list(ggplot2::scale_color_discrete(labels = labels)))
      components <- c(components, list(ggplot2::scale_fill_discrete(labels = labels)))
    } else {
      scale_func <- switch(
        aesthetic,
        "color" = ggplot2::scale_color_discrete,
        "colour" = ggplot2::scale_colour_discrete,
        "fill" = ggplot2::scale_fill_discrete,
        "shape" = ggplot2::scale_shape_discrete,
        "size" = ggplot2::scale_size_discrete,
        "alpha" = ggplot2::scale_alpha_discrete,
        "linetype" = ggplot2::scale_linetype_discrete,
        "group" = ggplot2::scale_color_discrete
      )
      components <- c(components, list(scale_func(labels = labels)))
    }
  }

  return(components)
}

#' Remove entire legend or specific aesthetic guides
#'
#' @param aesthetic Aesthetic type: "all", "color", "fill", "colour", "shape",
#'   "size", "alpha", "linetype", or "group". Default "all".
#'
#' @return List of ggplot2 theme/guides components to hide legends.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl), shape = factor(cyl))) +
#'   geom_point()
#'
#' # Remove all legends
#' p + legend_delete()
#'
#' # Remove only the color legend
#' p + legend_delete(aesthetic = "color")
#'
#' @export
legend_delete <- function(aesthetic = c("all",
                                        "color",
                                        "fill",
                                        "colour",
                                        "shape",
                                        "size",
                                        "alpha",
                                        "linetype",
                                        "group")) {
  aesthetic <- match.arg(aesthetic)
  components <- list()

  if (aesthetic == "all") {
    components <- c(components, list(ggplot2::theme(legend.position = "none")))
  } else {
    guide_args <- list("none")
    names(guide_args) <- aesthetic
    components <- c(components, list(do.call(ggplot2::guides, guide_args)))
  }

  return(components)
}


#' Remove legend title for specific or all aesthetics
#'
#' @param aesthetic Aesthetic type: "all", "color", "fill", "colour", "shape",
#'   "size", "alpha", "linetype", or "group". Default "all".
#'
#' @return List of ggplot2 theme components that blank the legend title.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Remove all legend titles
#' p + legend_delete_title()
#'
#' # Remove only the color legend title
#' p + legend_delete_title(aesthetic = "color")
#'
#' @export
legend_delete_title <- function(aesthetic = c("all",
                                              "color",
                                              "fill",
                                              "colour",
                                              "shape",
                                              "size",
                                              "alpha",
                                              "linetype",
                                              "group")) {
  aesthetic <- match.arg(aesthetic)
  components <- list()

  if (aesthetic == "all") {
    components <- c(components, list(ggplot2::theme(legend.title = ggplot2::element_blank())))
  } else {
    # Use setNames to create a named list without rlang
    theme_args <- list(ggplot2::element_blank())
    names(theme_args) <- paste0("legend.title.", aesthetic)
    components <- c(components, list(do.call(ggplot2::theme, theme_args)))
  }

  return(components)
}


#' Set legend position with optional justification and direction
#'
#' @param position Legend position: "top", "bottom", "left", "right", or numeric
#'   coordinates. Default "right".
#' @param justify Justification point as a numeric vector of length 2, or NULL.
#' @param direction Legend direction ("horizontal" or "vertical"), or NULL.
#' @param aesthetic Aesthetic type: "all", "color", "fill", "colour", "shape",
#'   "size", "alpha", "linetype", or "group". Default "all".
#'
#' @return List of ggplot2 theme components for legend positioning.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#'
#' # Move legend to top
#' p + legend_position(position = "top")
#'
#' # Move legend to bottom-left corner
#' p + legend_position(position = c(0, 0), justify = c(0, 0))
#'
#' # Move legend to bottom with horizontal layout
#' p + legend_position(position = "bottom", direction = "horizontal")
#'
#' @export
legend_position <- function(position = "right",
                            justify = NULL,
                            direction = c("vertical", "horizontal"),
                            aesthetic = c("all",
                                          "color",
                                          "fill",
                                          "colour",
                                          "shape",
                                          "size",
                                          "alpha",
                                          "linetype",
                                          "group")) {

  direction <- match.arg(direction)

  aesthetic <- match.arg(aesthetic)

  components <- list()

  if (aesthetic == "all") {
    components <- c(components, list(ggplot2::theme(legend.position = position)))
    if (!is.null(justify)) {
      components <- c(components, list(ggplot2::theme(legend.justification = justify)))
    }
    if (!is.null(direction)) {
      components <- c(components, list(ggplot2::theme(legend.direction = direction)))
    }
  } else {
    components <- c(components, list(ggplot2::theme(legend.position = position)))
  }

  return(components)
}

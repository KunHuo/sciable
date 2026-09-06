#' Add title with styling
#'
#' @param label Title text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param hjust Horizontal justification. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + add_title("My Title", size = 16, color = "steelblue")
#' p + add_title("Centered", hjust = 0.5, face = "bold")
#' p + add_title("Only label")
add_title <- function(label = NULL,
                      size = NULL,
                      face = NULL,
                      color = NULL,
                      hjust = NULL,
                      family = NULL) {
  result <- list()

  if (!is.null(label)) {
    result[[length(result) + 1]] <- ggplot2::labs(title = label)
  }

  args <- list(size = size, face = face, color = color,
               hjust = hjust, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    result[[length(result) + 1]] <- ggplot2::theme(
      plot.title = do.call(ggplot2::element_text, args)
    )
  }

  result
}

#' Add subtitle with styling
#'
#' @param label Subtitle text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param hjust Horizontal justification. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + add_subtitle("A note", size = 12, color = "grey40")
#' p + add_subtitle("Italic subtitle", face = "italic")
add_subtitle <- function(label = NULL,
                         size = NULL,
                         face = NULL,
                         color = NULL,
                         hjust = NULL,
                         family = NULL) {
  result <- list()

  if (!is.null(label)) {
    result[[length(result) + 1]] <- ggplot2::labs(subtitle = label)
  }

  args <- list(size = size, face = face, color = color,
               hjust = hjust, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    result[[length(result) + 1]] <- ggplot2::theme(
      plot.subtitle = do.call(ggplot2::element_text, args)
    )
  }

  result
}

#' Add caption with styling
#'
#' @param label Caption text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param hjust Horizontal justification. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + add_caption("Data: mtcars", size = 8, color = "grey60")
#' p + add_caption("Right aligned", hjust = 1)
add_caption <- function(label = NULL,
                        size = NULL,
                        face = NULL,
                        color = NULL,
                        hjust = NULL,
                        family = NULL) {
  result <- list()

  if (!is.null(label)) {
    result[[length(result) + 1]] <- ggplot2::labs(caption = label)
  }

  args <- list(size = size, face = face, color = color,
               hjust = hjust, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    result[[length(result) + 1]] <- ggplot2::theme(
      plot.caption = do.call(ggplot2::element_text, args)
    )
  }

  result
}

#' Add tag with styling
#'
#' @param label Tag text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param position Tag position. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + add_tag("A", size = 12, color = "red")
#' p + add_tag("(a)", position = "bottomright")
add_tag <- function(label = NULL,
                    size = NULL,
                    face = NULL,
                    color = NULL,
                    position = NULL,
                    family = NULL) {
  result <- list()

  if (!is.null(label)) {
    result[[length(result) + 1]] <- ggplot2::labs(tag = label)
  }

  args <- list(size = size, face = face, color = color, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    result[[length(result) + 1]] <- ggplot2::theme(
      plot.tag = do.call(ggplot2::element_text, args)
    )
  }

  if (!is.null(position)) {
    result[[length(result) + 1]] <- ggplot2::theme(
      plot.tag.position = position
    )
  }

  result
}

#' Add axis title x with styling
#'
#' @param label X-axis title text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + add_axis_title_x("Weight", size = 14, color = "steelblue")
#' p + add_axis_title_x("X Label")
add_axis_title_x <- function(label = NULL,
                             size = NULL,
                             face = NULL,
                             color = NULL,
                             family = NULL) {
  result <- list()

  if (!is.null(label)) {
    result[[length(result) + 1]] <- ggplot2::labs(x = label)
  }

  args <- list(size = size, face = face, color = color, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    result[[length(result) + 1]] <- ggplot2::theme(
      axis.title.x = do.call(ggplot2::element_text, args)
    )
  }

  result
}

#' Add axis title y with styling
#'
#' @param label Y-axis title text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + add_axis_title_y("MPG", size = 14, color = "darkgreen")
#' p + add_axis_title_y("Y Label")
add_axis_title_y <- function(label = NULL,
                             size = NULL,
                             face = NULL,
                             color = NULL,
                             family = NULL) {
  result <- list()

  if (!is.null(label)) {
    result[[length(result) + 1]] <- ggplot2::labs(y = label)
  }

  args <- list(size = size, face = face, color = color, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    result[[length(result) + 1]] <- ggplot2::theme(
      axis.title.y = do.call(ggplot2::element_text, args)
    )
  }

  result
}

#' Add both axis titles with styling
#'
#' @param x_label X-axis title text. Default NULL.
#' @param y_label Y-axis title text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + add_axis_title(x_label = "Weight", y_label = "MPG")
#' p + add_axis_title(x_label = "X", y_label = "Y", size = 14, color = "steelblue")
add_axis_title <- function(x_label = NULL,
                           y_label = NULL,
                           size = NULL,
                           face = NULL,
                           color = NULL,
                           family = NULL) {
  result <- list()

  lab_args <- list()
  if (!is.null(x_label)) lab_args$x <- x_label
  if (!is.null(y_label)) lab_args$y <- y_label
  if (length(lab_args) > 0) {
    result[[length(result) + 1]] <- do.call(ggplot2::labs, lab_args)
  }

  args <- list(size = size, face = face, color = color, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    elem <- do.call(ggplot2::element_text, args)
    result[[length(result) + 1]] <- ggplot2::theme(
      axis.title.x = elem,
      axis.title.y = elem
    )
  }

  result
}

#' Add legend title with styling
#'
#' @param label Legend title text. Default NULL.
#' @param size Font size. Default NULL.
#' @param face Font face. Default NULL.
#' @param color Font color. Default NULL.
#' @param family Font family. Default NULL.
#'
#' @return A list of ggplot2 components (labs + theme).
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point()
#' p + add_legend_title("Cylinders", size = 12, color = "steelblue")
#' p + add_legend_title("No. of Cyl")
add_legend_title <- function(label = NULL,
                             size = NULL,
                             face = NULL,
                             color = NULL,
                             family = NULL) {
  result <- list()

  if (!is.null(label)) {
    result[[length(result) + 1]] <- ggplot2::labs(
      color = label, fill = label,
      linetype = label, shape = label
    )
  }

  args <- list(size = size, face = face, color = color, family = family)
  args <- Filter(Negate(is.null), args)
  if (length(args) > 0) {
    result[[length(result) + 1]] <- ggplot2::theme(
      legend.title = do.call(ggplot2::element_text, args)
    )
  }

  result
}

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

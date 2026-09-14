#' Set both axis titles text and/or style
#'
#' @param x Title text for x-axis. If NULL, removes the title. Default: "".
#' @param y Title text for y-axis. If NULL, removes the title. Default: "".
#' @param size Font size in points. Default: NULL.
#' @param color Text color. Default: NULL.
#' @param face Font face. Default: NULL.
#' @param family Font family. Default: NULL.
#' @param hjust Horizontal justification. Default: NULL.
#' @param vjust Vertical justification. Default: NULL.
#' @param margin Numeric vector of length 4 (t,r,b,l) in pt. Default: NULL.
#'
#' @return A list of ggplot components.
#'
#' @export
#'
#' @seealso \code{\link{axis_x_title}}, \code{\link{axis_y_title}}
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage: set both axis titles with same style
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + axis_title(x = "Weight", y = "MPG", size = 14, color = "darkred")
#'
#' # Only change x-axis title text
#' p + axis_title(x = "Weight (1000 lbs)")
#'
#' # Only change y-axis title text
#' p + axis_title(y = "Miles per Gallon")
#'
#' # Change both titles with different styles using separate calls
#' p + axis_x_title("Weight", size = 16, color = "blue") +
#'     axis_y_title("MPG", size = 14, color = "red", face = "italic")
#'
#' # Remove x-axis title
#' p + axis_title(x = NULL)
#'
#' # Remove both axis titles
#' p + axis_title(x = NULL, y = NULL)
#'
#' # Add margin above x-axis title
#' p + axis_x_title("Weight", margin = c(10, 0, 0, 0))
axis_title <- function(x = "",
                       y = "",
                       size = NULL,
                       color = NULL,
                       face = NULL,
                       family = NULL,
                       hjust = NULL,
                       vjust = NULL,
                       margin = NULL) {
  out <- list()
  a <- Filter(
    Negate(is.null),
    list(
      size = size,
      color = color,
      face = face,
      family = family,
      hjust = hjust,
      vjust = vjust
    )
  )
  if (!is.null(margin))
    a$margin <- ggplot2::margin(t = margin[1],
                                r = margin[2],
                                b = margin[3],
                                l = margin[4])

  if (is.null(x))
    out <- c(out, list(ggplot2::theme(axis.title.x = ggplot2::element_blank())))
  else if (nzchar(x))
    out <- c(out, list(ggplot2::xlab(x)))

  if (is.null(y))
    out <- c(out, list(ggplot2::theme(axis.title.y = ggplot2::element_blank())))
  else if (nzchar(y))
    out <- c(out, list(ggplot2::ylab(y)))

  if (length(a)) {
    out <- c(out, list(ggplot2::theme(
      axis.title.x = do.call(ggplot2::element_text, a)
    )))
    out <- c(out, list(ggplot2::theme(
      axis.title.y = do.call(ggplot2::element_text, a)
    )))
  }

  if (!length(out))
    out <- list(ggplot2::theme())
  out
}

#' Set x-axis title text and/or style
#'
#' @param text Title text for x-axis. If NULL, removes the title. Default: "".
#' @inheritParams axis_title
#' @return A list of ggplot components.
#'
#' @export
#'
#' @seealso \code{\link{axis_title}}, \code{\link{axis_y_title}}
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Set text only
#' p + axis_x_title("Weight")
#'
#' # Set text with style
#' p + axis_x_title("Weight", size = 16, color = "blue", face = "bold")
#'
#' # Style only (keep existing title)
#' p + axis_x_title(size = 14, color = "red")
#'
#' # Remove title
#' p + axis_x_title(NULL)
#'
#' # With margin
#' p + axis_x_title("Weight", margin = c(15, 0, 0, 0))
axis_x_title <- function(text = "",
                         size = NULL,
                         color = NULL,
                         face = NULL,
                         family = NULL,
                         hjust = NULL,
                         vjust = NULL,
                         margin = NULL) {
  axis_title(
    x = text,
    size = size,
    color = color,
    face = face,
    family = family,
    hjust = hjust,
    vjust = vjust,
    margin = margin
  )
}

#' Set y-axis title text and/or style
#'
#' @param text Title text for x-axis. If NULL, removes the title. Default: "".
#'
#' @inheritParams axis_title
#' @return A list of ggplot components.
#'
#' @export
#'
#' @seealso \code{\link{axis_title}}, \code{\link{axis_x_title}}
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Set text only
#' p + axis_y_title("MPG")
#'
#' # Set text with style
#' p + axis_y_title("MPG", size = 14, color = "darkgreen", face = "italic")
#'
#' # Style only (keep existing title)
#' p + axis_y_title(size = 12, color = "gray40")
#'
#' # Remove title
#' p + axis_y_title(NULL)
#'
#' # With margin
#' p + axis_y_title("MPG", margin = c(0, 10, 0, 0))
axis_y_title <- function(text = "",
                         size = NULL,
                         color = NULL,
                         face = NULL,
                         family = NULL,
                         hjust = NULL,
                         vjust = NULL,
                         margin = NULL) {
  axis_title(
    y = text,
    size = size,
    color = color,
    face = face,
    family = family,
    hjust = hjust,
    vjust = vjust,
    margin = margin
  )
}

#' Set both axis tick label text and/or style
#' @param x Named character vector or list for x-axis labels. Names are original
#' labels, values are new labels. If NULL, removes labels. Default: NULL.
#' @param y Named character vector or list for y-axis labels. Names are original
#' labels, values are new labels. If NULL, removes labels. Default: NULL.
#' @param size Font size in points. Default: NULL.
#' @param color Text color. Default: NULL.
#' @param angle Rotation angle in degrees. Default: NULL.
#' @param face Font face. Default: NULL.
#' @param family Font family. Default: NULL.
#' @param hjust Horizontal justification (0-1). Default: NULL.
#' @param vjust Vertical justification (0-1). Default: NULL.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_x_text}}, \code{\link{axis_y_text}}, \code{\link{axis_title}}
#' @examples
#' library(ggplot2)
#'
#' # Discrete variable
#' p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' p + axis_text(x = c("4" = "4 Cyl", "6" = "6 Cyl", "8" = "8 Cyl"))
#' p + axis_text(x = c("4" = "Four", "8" = "Eight"))
#' p + axis_text(size = 12, color = "blue", angle = 45)
#' p + axis_text(x = NULL)
#'
#' # Continuous variable - use a function to format labels
#' p2 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p2 + axis_text(x = function(x) paste0(x, " tons"))
axis_text <- function(x = NULL,
                      y = NULL,
                      size = NULL,
                      color = NULL,
                      angle = NULL,
                      face = NULL,
                      family = NULL,
                      hjust = NULL,
                      vjust = NULL) {
  out <- list()
  a <- Filter(
    Negate(is.null),
    list(
      size = size,
      color = color,
      angle = angle,
      face = face,
      family = family,
      hjust = hjust,
      vjust = vjust
    )
  )

  build_labels <- function(var, labs) {
    if (is.null(labs))
      return(list(
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
      ))
    if (is.function(labs))
      return(list(switch(
        var,
        x = ggplot2::scale_x_continuous(labels = labs),
        y = ggplot2::scale_y_continuous(labels = labs)
      )))
    if (is.character(labs) &&
        length(labs) > 0 && !all(names(labs) == "")) {
      labeller <- function(orig) {
        idx <- match(as.character(orig), names(labs))
        ifelse(is.na(idx), as.character(orig), unname(labs[idx]))
      }
      return(list(switch(
        var,
        x = ggplot2::scale_x_discrete(labels = labeller),
        y = ggplot2::scale_y_discrete(labels = labeller)
      )))
    }
    if (is.character(labs) && length(labs) > 0) {
      return(list(switch(
        var,
        x = ggplot2::scale_x_discrete(labels = labs),
        y = ggplot2::scale_y_discrete(labels = labs)
      )))
    }
    NULL
  }

  if (!is.null(x))
    out <- c(out, build_labels("x", x))
  if (!is.null(y))
    out <- c(out, build_labels("y", y))

  if (length(a)) {
    out <- c(out, list(ggplot2::theme(
      axis.text.x = do.call(ggplot2::element_text, a)
    )))
    out <- c(out, list(ggplot2::theme(
      axis.text.y = do.call(ggplot2::element_text, a)
    )))
  }

  if (!length(out))
    out <- list(ggplot2::theme())
  out
}

#' Set x-axis tick label text and/or style
#' @param text Named character vector or function for x-axis labels. Names are
#' original labels, values are new labels. If a function, applied to original
#' labels. If NULL, removes labels. Default: NULL.
#' @param size Font size in points. Default: NULL.
#' @param color Text color. Default: NULL.
#' @param angle Rotation angle in degrees. Default: NULL.
#' @param face Font face. Default: NULL.
#' @param family Font family. Default: NULL.
#' @param hjust Horizontal justification (0-1). Default: NULL.
#' @param vjust Vertical justification (0-1). Default: NULL.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_text}}, \code{\link{axis_y_text}}
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' p + axis_x_text(c("4" = "4 Cyl", "6" = "6 Cyl", "8" = "8 Cyl"))
#' p + axis_x_text(c("4" = "Four"))
#' p + axis_x_text(size = 12, color = "red", angle = 45)
#' p + axis_x_text(NULL)
#'
#' # Continuous variable
#' p2 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p2 + axis_x_text(function(x) paste0(x, " tons"))
axis_x_text <- function(text = NULL,
                        size = NULL,
                        color = NULL,
                        angle = NULL,
                        face = NULL,
                        family = NULL,
                        hjust = NULL,
                        vjust = NULL) {
  axis_text(
    x = text,
    size = size,
    color = color,
    angle = angle,
    face = face,
    family = family,
    hjust = hjust,
    vjust = vjust
  )
}

#' Set y-axis tick label text and/or style
#' @param text Named character vector or function for y-axis labels. Names are
#' original labels, values are new labels. If a function, applied to original
#' labels. If NULL, removes labels. Default: NULL.
#' @param size Font size in points. Default: NULL.
#' @param color Text color. Default: NULL.
#' @param angle Rotation angle in degrees. Default: NULL.
#' @param face Font face. Default: NULL.
#' @param family Font family. Default: NULL.
#' @param hjust Horizontal justification (0-1). Default: NULL.
#' @param vjust Vertical justification (0-1). Default: NULL.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_text}}, \code{\link{axis_x_text}}
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' p + axis_y_text(c("4" = "Low", "6" = "Mid", "8" = "High"))
#' p + axis_y_text(c("8" = "Top"))
#' p + axis_y_text(size = 10, color = "blue")
#' p + axis_y_text(NULL)
#'
#' # Continuous variable
#' p2 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p2 + axis_y_text(function(x) paste0(x, " mpg"))
axis_y_text <- function(text = NULL,
                        size = NULL,
                        color = NULL,
                        angle = NULL,
                        face = NULL,
                        family = NULL,
                        hjust = NULL,
                        vjust = NULL) {
  axis_text(
    y = text,
    size = size,
    color = color,
    angle = angle,
    face = face,
    family = family,
    hjust = hjust,
    vjust = vjust
  )
}

#' Reorder factor levels on an axis
#' @param x New order of x-axis levels as a character vector. Default: NULL.
#' @param y New order of y-axis levels as a character vector. Default: NULL.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_x_order}}, \code{\link{axis_y_order}}, \code{\link{axis_text}}
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' # Reverse order
#' p + axis_order(x = c("8", "6", "4"))
#' # Custom order
#' p + axis_order(x = c("6", "4", "8"))
axis_order <- function(x = NULL, y = NULL) {
  out <- list()

  if (!is.null(x)) {
    out <- c(out, list(ggplot2::scale_x_discrete(limits = x)))
  }

  if (!is.null(y)) {
    out <- c(out, list(ggplot2::scale_y_discrete(limits = y)))
  }

  if (!length(out))
    out <- list(ggplot2::theme())
  out
}

#' Reorder x-axis factor levels
#' @param levels New order of x-axis levels as a character vector. Default: NULL.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_order}}, \code{\link{axis_y_order}}
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' p + axis_x_order(c("8", "6", "4"))
#' p + axis_x_order(c("6", "4", "8"))
axis_x_order <- function(levels = NULL) {
  axis_order(x = levels)
}

#' Reorder y-axis factor levels
#' @param levels New order of y-axis levels as a character vector. Default: NULL.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_order}}, \code{\link{axis_x_order}}
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, factor(cyl))) + geom_boxplot()
#' p + axis_y_order(c("8", "6", "4"))
#' p + axis_y_order(c("6", "4", "8"))
axis_y_order <- function(levels = NULL) {
  axis_order(y = levels)
}


#' Set axis line visibility and/or style
#' @param show Logical. TRUE shows line, FALSE hides. Applies to both axes.
#' Use axis_x_line/axis_y_line for individual control. Default: NULL.
#' @param color Line color. Default: NULL.
#' @param linewidth Line width in mm. Default: NULL.
#' @param linetype Line type. Default: NULL.
#' @param length Tick length in cm. Only used when ticks = TRUE. Default: NULL.
#' @param ticks Logical. If TRUE, also applies same style to axis ticks. Default: TRUE.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_x_line}}, \code{\link{axis_y_line}},
#' \code{\link{axis_text}}, \code{\link{axis_title}}
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Style both axes lines and ticks
#' p + axis_line(color = "red", linewidth = 1.2, linetype = "dashed")
#'
#' # Style lines only, leave ticks unchanged
#' p + axis_line(color = "blue", linewidth = 1, ticks = FALSE)
#'
#' # Custom tick length
#' p + axis_line(color = "darkgreen", length = 0.4)
#'
#' # Hide both axes lines and ticks
#' p + axis_line(show = FALSE)
#'
#' # Hide lines but keep ticks visible
#' p + axis_line(show = FALSE, ticks = FALSE)
#'
#' # Style only x-axis line and ticks
#' p + axis_x_line(color = "red", linewidth = 1)
#'
#' # Style only y-axis line, no tick changes
#' p + axis_y_line(color = "blue", linetype = "dotted", ticks = FALSE)
#'
#' # Hide x-axis line and ticks
#' p + axis_x_line(show = FALSE)
#'
#' # Hide y-axis line only, keep ticks
#' p + axis_y_line(show = FALSE, ticks = FALSE)
#'
#' # Restore hidden x-axis line with style
#' p + axis_x_line(show = TRUE, color = "darkgreen")
#'
#' # Chain: hide x, style y
#' p + axis_x_line(FALSE) + axis_y_line(color = "purple", linewidth = 0.8)
#'
#' # Full customization
#' p + axis_line(color = "gray50", linewidth = 0.5) +
#'     axis_x_line(show = FALSE) +
#'     axis_y_line(color = "red", linewidth = 1.5, linetype = "solid")
axis_line <- function(show = NULL,
                      color = NULL,
                      linewidth = NULL,
                      linetype = NULL,
                      length = NULL,
                      ticks = TRUE) {
  out <- list()
  a <- Filter(Negate(is.null),
              list(
                color = color,
                linewidth = linewidth,
                linetype = linetype
              ))

  if (identical(show, FALSE)) {
    out <- c(out, list(ggplot2::theme(axis.line.x = ggplot2::element_blank())))
    out <- c(out, list(ggplot2::theme(axis.line.y = ggplot2::element_blank())))
    if (ticks) {
      out <- c(out, list(ggplot2::theme(axis.ticks.x = ggplot2::element_blank())))
      out <- c(out, list(ggplot2::theme(axis.ticks.y = ggplot2::element_blank())))
    }
  } else if (length(a) || !is.null(length)) {
    if (length(a)) {
      out <- c(out, list(ggplot2::theme(
        axis.line.x = do.call(ggplot2::element_line, a)
      )))
      out <- c(out, list(ggplot2::theme(
        axis.line.y = do.call(ggplot2::element_line, a)
      )))
      if (ticks) {
        out <- c(out, list(ggplot2::theme(
          axis.ticks.x = do.call(ggplot2::element_line, a)
        )))
        out <- c(out, list(ggplot2::theme(
          axis.ticks.y = do.call(ggplot2::element_line, a)
        )))
      }
    }
    if (!is.null(length) && ticks) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.length = ggplot2::unit(length, "cm")
      )))
    }
  }

  if (!length(out))
    list(ggplot2::theme())
  else
    out
}

#' @rdname axis_line
#' @export
axis_x_line <- function(show = NULL,
                        color = NULL,
                        linewidth = NULL,
                        linetype = NULL,
                        length = NULL,
                        ticks = TRUE) {
  out <- list()
  a <- Filter(Negate(is.null),
              list(
                color = color,
                linewidth = linewidth,
                linetype = linetype
              ))

  if (identical(show, FALSE)) {
    out <- c(out, list(ggplot2::theme(axis.line.x = ggplot2::element_blank())))
    if (ticks) {
      out <- c(out, list(ggplot2::theme(axis.ticks.x = ggplot2::element_blank())))
    }
  } else if (length(a) || !is.null(length)) {
    if (length(a)) {
      out <- c(out, list(ggplot2::theme(
        axis.line.x = do.call(ggplot2::element_line, a)
      )))
      if (ticks) {
        out <- c(out, list(ggplot2::theme(
          axis.ticks.x = do.call(ggplot2::element_line, a)
        )))
      }
    }
    if (!is.null(length) && ticks) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.length = ggplot2::unit(length, "cm")
      )))
    }
  }

  if (!length(out))
    list(ggplot2::theme())
  else
    out
}

#' @rdname axis_line
#' @export
axis_y_line <- function(show = NULL,
                        color = NULL,
                        linewidth = NULL,
                        linetype = NULL,
                        length = NULL,
                        ticks = TRUE) {
  out <- list()
  a <- Filter(Negate(is.null),
              list(
                color = color,
                linewidth = linewidth,
                linetype = linetype
              ))

  if (identical(show, FALSE)) {
    out <- c(out, list(ggplot2::theme(axis.line.y = ggplot2::element_blank())))
    if (ticks) {
      out <- c(out, list(ggplot2::theme(axis.ticks.y = ggplot2::element_blank())))
    }
  } else if (length(a) || !is.null(length)) {
    if (length(a)) {
      out <- c(out, list(ggplot2::theme(
        axis.line.y = do.call(ggplot2::element_line, a)
      )))
      if (ticks) {
        out <- c(out, list(ggplot2::theme(
          axis.ticks.y = do.call(ggplot2::element_line, a)
        )))
      }
    }
    if (!is.null(length) && ticks) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.length = ggplot2::unit(length, "cm")
      )))
    }
  }

  if (!length(out))
    list(ggplot2::theme())
  else
    out
}


#' Set axis ticks visibility and/or style
#' @param show Logical. TRUE shows ticks, FALSE hides. Applies to both axes.
#' Use axis_x_ticks/axis_y_ticks for individual control. Default: NULL.
#' @param color Tick color. Default: NULL.
#' @param linewidth Tick line width in mm. Default: NULL.
#' @param linetype Tick line type. Default: NULL.
#' @param length Tick length in cm. Default: NULL.
#' @return A list of ggplot components.
#' @export
#' @seealso \code{\link{axis_x_ticks}}, \code{\link{axis_y_ticks}},
#' \code{\link{axis_line}}, \code{\link{axis_text}}, \code{\link{axis_title}}
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Style both axes ticks
#' p + axis_ticks(color = "red", linewidth = 1.2, length = 0.3)
#'
#' # Hide both axes ticks
#' p + axis_ticks(show = FALSE)
#'
#' # Style only x-axis ticks
#' p + axis_x_ticks(color = "blue", linewidth = 1)
#'
#' # Style only y-axis ticks
#' p + axis_y_ticks(color = "darkgreen", linetype = "dotted")
#'
#' # Hide x-axis ticks only
#' p + axis_x_ticks(show = FALSE)
#'
#' # Hide y-axis ticks only
#' p + axis_y_ticks(show = FALSE)
#'
#' # Chain: hide x, style y
#' p + axis_x_ticks(FALSE) + axis_y_ticks(color = "purple", linewidth = 0.8)
#'
#' # Full customization
#' p + axis_ticks(color = "gray50", linewidth = 0.5) +
#'     axis_x_ticks(show = FALSE) +
#'     axis_y_ticks(color = "red", linewidth = 1.5, length = 0.4)
axis_ticks <- function(show = NULL,
                       color = NULL,
                       linewidth = NULL,
                       linetype = NULL,
                       length = NULL) {
  out <- list()
  a <- Filter(Negate(is.null),
              list(
                color = color,
                linewidth = linewidth,
                linetype = linetype
              ))

  if (identical(show, FALSE)) {
    out <- c(out, list(ggplot2::theme(axis.ticks.x = ggplot2::element_blank())))
    out <- c(out, list(ggplot2::theme(axis.ticks.y = ggplot2::element_blank())))
  } else {
    if (length(a)) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.x = do.call(ggplot2::element_line, a)
      )))
      out <- c(out, list(ggplot2::theme(
        axis.ticks.y = do.call(ggplot2::element_line, a)
      )))
    }
    if (!is.null(length)) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.length = ggplot2::unit(length, "cm")
      )))
    }
  }

  if (!length(out))
    list(ggplot2::theme())
  else
    out
}

#' @rdname axis_ticks
#' @export
axis_x_ticks <- function(show = NULL,
                         color = NULL,
                         linewidth = NULL,
                         linetype = NULL,
                         length = NULL) {
  out <- list()
  a <- Filter(Negate(is.null),
              list(
                color = color,
                linewidth = linewidth,
                linetype = linetype
              ))

  if (identical(show, FALSE)) {
    out <- c(out, list(ggplot2::theme(axis.ticks.x = ggplot2::element_blank())))
  } else {
    if (length(a)) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.x = do.call(ggplot2::element_line, a)
      )))
    }
    if (!is.null(length)) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.length = ggplot2::unit(length, "cm")
      )))
    }
  }

  if (!length(out))
    list(ggplot2::theme())
  else
    out
}

#' @rdname axis_ticks
#' @export
axis_y_ticks <- function(show = NULL,
                         color = NULL,
                         linewidth = NULL,
                         linetype = NULL,
                         length = NULL) {
  out <- list()
  a <- Filter(Negate(is.null),
              list(
                color = color,
                linewidth = linewidth,
                linetype = linetype
              ))

  if (identical(show, FALSE)) {
    out <- c(out, list(ggplot2::theme(axis.ticks.y = ggplot2::element_blank())))
  } else {
    if (length(a)) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.y = do.call(ggplot2::element_line, a)
      )))
    }
    if (!is.null(length)) {
      out <- c(out, list(ggplot2::theme(
        axis.ticks.length = ggplot2::unit(length, "cm")
      )))
    }
  }

  if (!length(out))
    list(ggplot2::theme())
  else
    out
}

#' Generate pretty axis breaks for ggplot2
#'
#' Automatically generates and applies axis breaks for continuous or
#' discrete x and y axes. For continuous axes, values specified by
#' `xinclude` and `yinclude` are incorporated into the range before
#' `pretty()` calculates the final breaks.
#'
#' If `xbreaks` or `ybreaks` is supplied, the corresponding axis uses
#' those breaks directly. The corresponding `xinclude` or `yinclude`
#' values are then ignored.
#'
#' Continuous axes always use `expand = c(0, 0)`. For discrete axes,
#' `discrete.expand = TRUE` retains the default ggplot2 expansion,
#' whereas `discrete.expand = FALSE` uses `expand = c(0, 0)`.
#'
#' @param n Numeric. Approximate number of breaks to generate when
#'   `xbreaks` or `ybreaks` is `NULL`. Defaults to 5.
#' @param xbreaks Optional vector of x-axis breaks. If supplied, these
#'   values are used directly.
#' @param ybreaks Optional vector of y-axis breaks. If supplied, these
#'   values are used directly.
#' @param xinclude Optional numeric vector of values to include when
#'   determining the range passed to `pretty()` for the x-axis.
#' @param yinclude Optional numeric vector of values to include when
#'   determining the range passed to `pretty()` for the y-axis.
#' @param discrete.expand Logical. Whether to retain the default
#'   expansion for discrete axes. If `TRUE`, discrete axes retain the
#'   default ggplot2 expansion. If `FALSE`, discrete axes use
#'   `expand = c(0, 0)`. Defaults to `TRUE`.
#'
#' @seealso
#' [pretty_xbreaks()] for the x-axis only.
#' [pretty_ybreaks()] for the y-axis only.
#'
#' @return An object of class `pretty_breaks`.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   pretty_breaks()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   pretty_breaks(
#'     xbreaks = seq(1, 6, 1),
#'     ybreaks = seq(10, 35, 5)
#'   )
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   pretty_breaks(
#'     xinclude = c(2, 4),
#'     yinclude = c(15, 30)
#'   )
#'
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   pretty_breaks()
#'
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   pretty_breaks(
#'     discrete.expand = FALSE
#'   )
#'
#' @export
pretty_breaks <- function(n = 5,
                          xbreaks = NULL,
                          ybreaks = NULL,
                          xinclude = NULL,
                          yinclude = NULL,
                          discrete.expand = TRUE) {
  stopifnot(
    length(n) == 1L,
    is.numeric(n),
    is.finite(n),
    n > 0,
    length(discrete.expand) == 1L,
    is.logical(discrete.expand),!is.na(discrete.expand)
  )

  structure(
    list(
      n = n,
      xbreaks = xbreaks,
      ybreaks = ybreaks,
      xinclude = xinclude,
      yinclude = yinclude,
      discrete.expand = discrete.expand
    ),
    class = "pretty_breaks"
  )
}


#' Generate pretty x-axis breaks
#'
#' A convenience wrapper around [pretty_breaks()] for controlling
#' the x-axis.
#'
#' @param n Numeric. Approximate number of breaks to generate when
#'   `breaks` is `NULL`. Defaults to 5.
#' @param breaks Optional vector of x-axis breaks. If supplied, these
#'   values are used directly.
#' @param include Optional numeric vector of values to include when
#'   determining the range passed to `pretty()`.
#' @param discrete.expand Logical. Whether to retain the default
#'   expansion when the x-axis is discrete. If `TRUE`, the x-axis
#'   retains the default ggplot2 expansion. If `FALSE`, the x-axis
#'   uses `expand = c(0, 0)`. Defaults to `TRUE`.
#'
#' @seealso
#' [pretty_breaks()] for both axes.
#' [pretty_ybreaks()] for the y-axis.
#'
#' @return An object of class `pretty_breaks`.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   pretty_xbreaks()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   pretty_xbreaks(
#'     include = c(2, 4)
#'   )
#'
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   pretty_xbreaks(
#'     discrete.expand = FALSE
#'   )
#'
#' @export
pretty_xbreaks <- function(n = 5,
                           breaks = NULL,
                           include = NULL,
                           discrete.expand = TRUE) {
  pretty_breaks(
    n = n,
    xbreaks = breaks,
    xinclude = include,
    discrete.expand = discrete.expand
  )
}


#' Generate pretty y-axis breaks
#'
#' A convenience wrapper around [pretty_breaks()] for controlling
#' the y-axis.
#'
#' @param n Numeric. Approximate number of breaks to generate when
#'   `breaks` is `NULL`. Defaults to 5.
#' @param breaks Optional vector of y-axis breaks. If supplied, these
#'   values are used directly.
#' @param include Optional numeric vector of values to include when
#'   determining the range passed to `pretty()`.
#' @param discrete.expand Logical. Whether to retain the default
#'   expansion when the y-axis is discrete. If `TRUE`, the y-axis
#'   retains the default ggplot2 expansion. If `FALSE`, the y-axis
#'   uses `expand = c(0, 0)`. Defaults to `TRUE`.
#'
#' @seealso
#' [pretty_breaks()] for both axes.
#' [pretty_xbreaks()] for the x-axis.
#'
#' @return An object of class `pretty_breaks`.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   pretty_ybreaks()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   pretty_ybreaks(
#'     include = c(15, 30)
#'   )
#'
#' @export
pretty_ybreaks <- function(n = 5,
                           breaks = NULL,
                           include = NULL,
                           discrete.expand = TRUE) {
  pretty_breaks(
    n = n,
    ybreaks = breaks,
    yinclude = include,
    discrete.expand = discrete.expand
  )
}


#' Check whether a ggplot2 scale is discrete
#'
#' @param scale A ggplot2 scale object.
#'
#' @return A single logical value.
#'
#' @keywords internal
.is_discrete_axis <- function(scale) {
  isTRUE(scale$is_discrete())
}


#' Extract axis data from a built ggplot object
#'
#' @param built A built ggplot object.
#' @param which Character scalar specifying `"x"` or `"y"`.
#'
#' @return A numeric vector containing the relevant axis values.
#'
#' @keywords internal
.get_axis_data <- function(built, which = c("x", "y")) {
  which <- match.arg(which)

  out <- numeric(0)

  for (layer_data in built$data) {
    if (which == "x") {
      if ("x" %in% names(layer_data)) {
        out <- c(out, layer_data$x)
      }

      if ("xmin" %in% names(layer_data)) {
        out <- c(out, layer_data$xmin, layer_data$xmax)
      }

    } else {
      if ("y" %in% names(layer_data)) {
        out <- c(out, layer_data$y)
      }

      if ("ymin" %in% names(layer_data)) {
        out <- c(out, layer_data$ymin, layer_data$ymax)
      }
    }
  }

  out
}


#' Generate pretty breaks from data and included values
#'
#' Included values are combined with the observed axis values before
#' the range is passed to `pretty()`. The final break sequence is
#' therefore determined entirely by `pretty()`.
#'
#' @param built A built ggplot object.
#' @param which Character scalar specifying `"x"` or `"y"`.
#' @param n Numeric. Approximate number of breaks.
#' @param include Numeric vector of values to include when determining
#'   the range passed to `pretty()`.
#'
#' @return A numeric vector of breaks, or `NULL` if no usable values
#'   are available.
#'
#' @keywords internal
.get_pretty_breaks <- function(built,
                               which,
                               n = 5,
                               include = NULL) {
  values <- .get_axis_data(built = built, which = which)

  values <- values[is.finite(values)]

  if (!is.null(include)) {
    if (!is.numeric(include)) {
      stop("`include` must be numeric.", call. = FALSE)
    }

    include <- include[is.finite(include)]

    if (length(include) > 0L) {
      values <- c(values, include)
    }
  }

  if (length(values) == 0L) {
    return(NULL)
  }

  pretty(range(values), n = n)
}


#' Select approximately n breaks from a discrete scale
#'
#' @param breaks Existing discrete scale breaks.
#' @param n Maximum approximate number of breaks.
#'
#' @return A vector containing the selected breaks.
#'
#' @keywords internal
.select_discrete_breaks <- function(breaks, n = 5) {
  if (length(breaks) <= n || n <= 1) {
    return(breaks)
  }

  index <- unique(round(seq(
    from = 1,
    to = length(breaks),
    length.out = n
  )))

  breaks[index]
}


#' Add pretty breaks to a ggplot object
#'
#' @param object A `pretty_breaks` object.
#' @param plot A ggplot object.
#' @param ... Additional arguments passed by `ggplot2::ggplot_add()`.
#'
#' @return A ggplot object with updated x and y scales.
#'
#' @keywords internal
#' @export
ggplot_add.pretty_breaks <- function(object, plot, ...) {
  built <- tryCatch(
    ggplot2::ggplot_build(plot),
    error = function(e)
      NULL
  )

  if (is.null(built)) {
    return(plot)
  }

  x_scale <- built$layout$panel_scales_x[[1]]
  y_scale <- built$layout$panel_scales_y[[1]]

  if (is.null(x_scale) || is.null(y_scale)) {
    return(plot)
  }

  x_breaks <- object$xbreaks

  if (is.null(x_breaks)) {
    if (.is_discrete_axis(x_scale)) {
      x_breaks <- x_scale$get_breaks()

      x_breaks <- .select_discrete_breaks(breaks = x_breaks, n = object$n)

    } else {
      x_breaks <- .get_pretty_breaks(
        built = built,
        which = "x",
        n = object$n,
        include = object$xinclude
      )
    }
  }

  if (!is.null(x_breaks)) {
    if (.is_discrete_axis(x_scale)) {
      if (isTRUE(object$discrete.expand)) {
        plot <- plot +
          ggplot2::scale_x_discrete(breaks = x_breaks)

      } else {
        plot <- plot +
          ggplot2::scale_x_discrete(breaks = x_breaks, expand = c(0, 0))
      }

    } else {
      plot <- plot +
        ggplot2::scale_x_continuous(
          breaks = x_breaks,
          limits = range(x_breaks, na.rm = TRUE),
          expand = c(0, 0)
        )
    }
  }

  y_breaks <- object$ybreaks

  if (is.null(y_breaks)) {
    if (.is_discrete_axis(y_scale)) {
      y_breaks <- y_scale$get_breaks()

      y_breaks <- .select_discrete_breaks(breaks = y_breaks, n = object$n)

    } else {
      y_breaks <- .get_pretty_breaks(
        built = built,
        which = "y",
        n = object$n,
        include = object$yinclude
      )
    }
  }

  if (!is.null(y_breaks)) {
    if (.is_discrete_axis(y_scale)) {
      if (isTRUE(object$discrete.expand)) {
        plot <- plot +
          ggplot2::scale_y_discrete(breaks = y_breaks)

      } else {
        plot <- plot +
          ggplot2::scale_y_discrete(breaks = y_breaks, expand = c(0, 0))
      }

    } else {
      plot <- plot +
        ggplot2::scale_y_continuous(
          breaks = y_breaks,
          limits = range(y_breaks, na.rm = TRUE),
          expand = c(0, 0)
        )
    }
  }

  plot
}

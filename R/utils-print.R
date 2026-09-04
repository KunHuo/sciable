#' Print a Booktabs-Style Table
#'
#' Print an object as a publication-quality table using the \code{booktabs}
#' LaTeX style. This is a generic-like wrapper that forwards arguments to
#' \code{print_booktabs()}.
#'
#' @param x An object to be printed as a booktabs table.
#' @param ... Additional arguments passed to \code{print_booktabs()}.
#'
#' @return The output of \code{print_booktabs(x, ...)}, typically the table
#'   markup or the input object invisibly.
#'
#' @keywords internal
#'
#' @export
print.booktabs <- function(x, ...){
  print_booktabs(x, ...)
}

print_booktabs <- function(data, sep = "__", adj = NULL, ...){

  title <- attr(data, "title")
  note  <- attr(data, "note")

  if(!is.null(title)){
    cat("\n")
    cat(title, "\n")
  }

  pad_df <- function(data, adj = "left"){
    if(length(adj) != ncol(data)){
      adj <- c(adj,  rep(adj[length(adj)], ncol(data) - length(adj) ))
    }

    data[,] <-  Map(function(x, a){
      str_pad(x, width = max_nchar(x), adj = a)
    }, data, adj)
    data
  }

  print_lines <- function(data, n.title = 1, n.space = 3){
    for(i in 1:nrow(data)){
      row <- data[i, ,drop = TRUE]
      row <- paste(row, collapse = strrep(" ", n.space))
      lines <- strrep("-", nchar(row))
      if(n.title == 1){
        if(i == 1 | i == 2){
          cat(lines, "\n")
        }
      }else{
        if(i == 1 | i == 4){
          cat(lines, "\n")
        }
      }
      cat(row, "\n")
      if(i== nrow(data)){
        cat(lines, "\n")
      }
    }
  }

  if(is.null(adj)){
    adj <- sapply(data, function(x){
      ifelse(is.numeric(x), "center", "left")
    })
  }

  data[, ] <- lapply(data[, ], function(x){
    if(is.numeric(x)){
      sapply(x, function(i){
        if(is.na(i)){
          ""
        }else{
          fmt <- sprintf("%%.%df", max_digits(i))
          sprintf(fmt, i)
        }
      })
    }else{
      sapply(x, function(i){
        if(is.na(i)){
          ""
        }else{
          as.character(i)
        }
      })
    }
  })

  if(any(regex_detect(names(data), pattern = sep, fixed = TRUE))){
    titles <- names(data)
    titles <- strsplit(names(data), split = sep, fixed = TRUE)
    titles <- do.call(cbind, titles)
    title1 <- titles[1, ]
    title2 <- titles[2, ]

    data <- rbind(title2, data)
    data <- pad_df(data, adj = adj)
    data <- lapply(data, function(x){ c(strrep("-", max_nchar(x)), x) })
    data <- as.data.frame(data)

    cdata <- lapply(unique(title1), function(x){
      colindex <- which(x == title1)
      tmpdata <- data[, colindex, drop = FALSE]
      tmpdata <- apply(tmpdata, 1, paste, collapse = strrep(" ", 3))
      tmpdata[1] <- strrep("-", nchar(tmpdata[1]))
      tmpdata
    })

    names(cdata) <- unique(title1)
    cdata <- as.data.frame(cdata)
    cdata <- rbind(unique(title1), cdata)
    cdata <- pad_df(cdata, adj = c("left", "center"))

    for(i in seq_along(cdata)){
      if(str_trim(cdata[1, i]) == str_trim(cdata[3, i])){
        cdata[2, i] <- cdata[1, i]
        cdata[1, i] <- strrep(" ", nchar(cdata[1, i]))
        cdata[3, i] <- strrep(" ", nchar(cdata[3, i]))
      }
    }

    print_lines(cdata, n.title = 2, n.space = 3)
  }else{
    data <- rbind(names(data), data)
    data <- pad_df(data, adj = adj)
    print_lines(data, n.title = 1, n.space = 3)
  }

  if(!is.null(note)){
    cat(note)
    cat("\n\n")
  }
}

n_digits <- function(x){

  x <- as.character(x)

  sapply(x, function(i) {
    # Split the character at the decimal point
    i <- regex_split(i, pattern = ".", fixed = TRUE)
    i <- unlist(i)

    if (length(i) == 1L) {
      0 # If there is no decimal point, return 0
    } else{
      nchar(i[2]) # Otherwise, return the number of characters after the decimal point
    }
  })
}

max_digits <- function(x){
  max(n_digits(x), na.rm = TRUE)
}

max_nchar <- function(x){
  max(sapply(x, nchar), na.rm = TRUE)
}

regex_detect <- function(string,
                         pattern,
                         ignore.case = FALSE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE){
  grepl(
    pattern,
    string,
    ignore.case = ignore.case,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)

}

regex_split <- function(string,
                        pattern,
                        perl = FALSE,
                        fixed = FALSE,
                        useBytes = FALSE){
  strsplit(
    string,
    pattern,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)
}

str_pad <- function (x, width = NULL, pad = " ", adj = "left") {
  .pad <- function(x, width, pad = " ", adj = "left") {
    if (is.na(x))
      return(NA)
    mto <- match.arg(adj, c("left", "right", "center"))
    free <- max(0, width - nchar(x))
    fill <- substring(paste(rep(pad, ceiling(free/nchar(pad))),
                            collapse = ""), 1, free)
    if (free <= 0)
      x
    else if (mto == "left")
      paste(x, fill, sep = "")
    else if (mto == "right")
      paste(fill, x, sep = "")
    else paste(substring(fill, 1, free%/%2), x, substring(fill,
                                                          1 + free%/%2, free), sep = "")
  }
  if (is.null(width))
    width <- max(nchar(x), na.rm = TRUE)
  lgp <- recycle(x = x, width = width, pad = pad,
                 adj = adj)
  sapply(1:attr(lgp, "maxdim"), function(i) .pad(lgp$x[i],
                                                 lgp$width[i], lgp$pad[i], lgp$adj[i]))
}

str_trim <- function (x, pattern = " \t\n", method = "both") {
  switch(
    match.arg(arg = method, choices = c("both", "left", "right")),
    both = {
      gsub(
        pattern = gettextf("^[%s]+|[%s]+$", pattern, pattern),
        replacement = "",
        x = x
      )
    },
    left = {
      gsub(
        pattern = gettextf("^[%s]+", pattern),
        replacement = "",
        x = x
      )
    },
    right = {
      gsub(
        pattern = gettextf("[%s]+$", pattern),
        replacement = "",
        x = x
      )
    }
  )
}

recycle <- function (...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}

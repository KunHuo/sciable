#' Tidy and Format Quantile Regression Model Coefficients
#'
#' @description
#' Extracts, formats, and tidies coefficients from a fitted quantile regression
#' model, producing a publication-ready table with estimates, confidence
#' intervals, and P values.
#'
#' @details
#' This function wraps \code{\link[broom]{tidy}} to extract coefficient
#' estimates from a fitted quantile regression model and formats them for
#' publication. The output is a data frame with estimates, confidence intervals,
#' and P values, optionally split and combined across multiple quantile levels
#' (\code{tau}).
#'
#' Standard errors and confidence intervals are computed according to the
#' \code{se.method} argument. See the \strong{Arguments} section for details on
#' the available inference methods.
#'
#' When \code{stats = "estp"} (the default), the output includes the estimate,
#' confidence interval, and P value for each term. Use \code{stats = "full"}
#' to retain all available columns from \code{broom::tidy}, or
#' \code{stats = "est"} for estimate-only output.
#'
#' The returned data frame carries a \code{"title"} attribute containing a
#' language-specific table caption that references the dependent variable.
#'
#' @param model A fitted model object (e.g., from \code{\link[quantreg]{rq}} or
#'   \code{\link[quantreg]{rq.fit}}).
#' @param se.method Character string specifying the method used to compute
#'   standard errors. There are currently eight available methods:
#'   \describe{
#'     \item{\code{"rank"}}{Produces confidence intervals by inverting a rank
#'       test as described in Koenker (1994). Involves solving a parametric
#'       linear programming problem; for large sample sizes can be extremely
#'       slow. The default option assumes iid errors, while
#'       \code{iid = FALSE} implements a proposal of Koenker and Machado
#'       (1999).}
#'     \item{\code{"iid"}}{Presumes that the errors are iid and computes an
#'       estimate of the asymptotic covariance matrix as in Koenker and Bassett
#'       (1978).}
#'     \item{\code{"nid"}}{Presumes local (in tau) linearity (in x) of the
#'       conditional quantile functions and computes a Huber sandwich estimate
#'       using a local estimate of the sparsity. If the initial fitting was done
#'       with method \code{"sfn"}, then \code{se = "nid"} is recommended.}
#'     \item{\code{"ker"}}{Uses a kernel estimate of the sandwich as proposed
#'       by Powell (1991).}
#'     \item{\code{"boot"}}{Implements one of several possible bootstrapping
#'       alternatives for estimating standard errors, including a variant of the
#'       wild bootstrap for clustered response.}
#'     \item{\code{"BLB"}}{Implements the bag of little bootstraps method
#'       proposed in Kleiner et al. (2014). Intended for applications with very
#'       large n where other flavors of the bootstrap can be slow.}
#'     \item{\code{"conquer"}}{Invoked automatically if the fitted object was
#'       created with \code{method = "conquer"}, and returns the multiplier
#'       bootstrap percentile confidence intervals described in He et al.
#'       (2020).}
#'     \item{\code{"extreme"}}{Uses the subsampling method of Chernozhukov,
#'       Fernandez-Val, and Kaji (2018) designed for inference on extreme
#'       quantiles.}
#'   }
#'   If \code{se.method = NULL} (the default) and \code{covariance = FALSE},
#'   and the sample size is less than 1001, then the \code{"rank"} method is
#'   used; otherwise the \code{"nid"} method is used. Default is
#'   \code{"boot"}.
#' @param R Integer. Number of bootstrap replications when
#'   \code{se.method = "boot"}. Default is \code{1000}.
#' @param digits Integer. Number of decimal places for coefficient estimates
#'   and confidence intervals. Default is \code{2}.
#' @param digits.pvalue Integer. Number of decimal places for P values.
#'   Default is \code{3}.
#' @param intercept Logical. Whether to include the intercept term in the
#'   output. Default is \code{FALSE}.
#' @param stats Character string specifying which statistics to display.
#'   Must be one of \code{"estp"} (estimate, CI, and P value),
#'   \code{"full"} (all available statistics), or \code{"est"}
#'   (estimate only). Default is \code{"estp"}.
#' @param by.row Logical. If \code{TRUE}, multiple quantile groups are
#'   combined by row; otherwise combined by column. Default is \code{TRUE}.
#' @param lang Character string specifying the language for labels.
#'   Must be one of \code{"en"} (English) or \code{"cn"} (Chinese).
#'   Default is \code{"en"}.
#'
#' @return A data frame of formatted model coefficients. The data frame has an
#'   additional attribute \code{"title"} containing a language-specific table
#'   caption. Row names are \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   data(engel, package = "quantreg")
#'   fit <- quantreg::rq(foodexp ~ income, data = engel, tau = c(0.25, 0.5))
#'   tidy_quantreg(fit, se.method = "boot", R = 500)
#' }
#'
#' @seealso \code{\link[broom]{tidy}}, \code{\link[quantreg]{rq}}
#'
#' @export
tidy_quantreg <- function(model,
                          se.method = c("boot", "rank", "iid", "nid", "ker", "BLB", "conquer", "extreme"),
                          R = 1000,
                          digits = 2,
                          digits.pvalue = 3,
                          intercept = FALSE,
                          stats = c("estp", "full", "est"),
                          by.row = TRUE,
                          lang = c("en", "cn")) {

  # Match argument values
  stats <- match.arg(stats)
  lang <- match.arg(lang)
  se.method <- match.arg(se.method)

  # Extract model variables and data
  vars <- attr(stats::terms(model$formula),"term.labels")
  dependent <- all.vars(model$formula)[1]
  data <- model$model

  # Get variable labels
  tvars <- tidy_vars(data, vars = vars, intercept = intercept)

  # Tidy model coefficients
  coefs <- broom::tidy(
    model,
    se = se.method,
    R = R,
    conf.int = TRUE,
    conf.level = 0.95
  )

  # Format estimates and confidence intervals
  coefs <- format_estimate_ci(coefs, digits = digits)

  # Format P values
  coefs$p.value <- format_pvalue(coefs$p.value, digits.pvalue)

  # Select statistics to display
  selected <- select_stats(stats)

  coefs <- coefs[c("term", "tau", selected)]

  # Rename display columns
  names(coefs) <- dplyr::recode(names(coefs), !!!get_stats_labels(lang))

  # Split coefficients by tau
  coefs <- split(coefs, coefs$tau, drop = TRUE)

  # Format each tau group
  coefs <- lapply(coefs, .format_tau_group, tvars = tvars)

  # Combine multiple tau groups
  if (length(coefs) > 1) {
    tau_label <- .get_tau_label(lang)

    coefs <- if (by.row) {
      .combine_tau_by_row(coefs, tau_label)
    } else {
      .combine_tau_by_column(coefs, tau_label, stats = stats)
    }

  } else {
    coefs <- coefs[[1]]
  }

  # Rename variable column
  names(coefs)[1] <- get_variable_label(lang)

  coefs <- add_estimate_reference(coefs, lang = lang)

  title.label <- get_label(data, dependent)

  title <- switch(lang,
                  en = sprintf("Table: Quantile regression estimates of factors associated with %s", title.label),
                  cn = sprintf("\u8868\uff1a\u5206\u4f4d\u6570\u56de\u5f52\u4f30\u8ba1\u4e0e%s\u76f8\u5173\u7684\u56e0\u7d20", title.label))

  note <- .add_quantreg_stat_methods(model, se.method = se.method, R= R, lang = lang)

  attr(coefs, "title") <- title
  attr(coefs, "note")  <- note

  class(coefs) <- c("booktabs", "data.frame")

  coefs
}

# Get tau label
.get_tau_label <- function(lang = c("en", "cn")) {
  switch(match.arg(lang), en = "tau = ", cn = "\u5206\u4f4d\u70b9 = ")
}


# Format coefficients within each tau group
.format_tau_group <- function(coef, tvars) {
  coef$tau <- NULL

  # remain_terms <- setdiff(coef$term, tvars$term)
  #
  #
  # if(length(remain_terms) != 0L){
  #
  # }
  # print(remain_terms)

  coef <- dplyr::left_join(tvars, coef, by = "term")

  coef$term <- NULL

  coef
}



# Combine tau groups by rows
.combine_tau_by_row <- function(coefs, tau_label) {
  coefs <- Map(function(coef, tau) {
    coef$Variable <- paste0("   ", coef$Variable)

    dplyr::add_row(coef,
                   Variable = paste0(tau_label, tau),
                   .before = 1)
  }, coefs, names(coefs))

  result <- do.call(rbind, coefs)
  row.names(result) <- NULL

  result
}


# Combine tau groups by columns
.combine_tau_by_column <- function(coefs, tau_label, stats = "est") {

  coefs <- lapply(names(coefs), function(tau) {
    df <- coefs[[tau]]

    if(stats == "est"){
      names(df)[-1] <- paste0(names(df)[-1], "__", tau_label, tau)
    }else{
      names(df)[-1] <- paste0(tau_label, tau, "__", names(df)[-1])
    }

    df
  })

  Reduce(function(x, y)
    cbind(x, y[-1]), coefs)
}


.add_quantreg_stat_methods <- function(model,
                                       alpha = 0.05,
                                       se.method = "boot",
                                       R = 1000,
                                       alternative = c("two.sided",
                                                       "greater",
                                                       "less"),
                                       lang = c("en", "cn")) {

  # Match arguments
  alternative <- match.arg(alternative)
  lang        <- match.arg(lang)

  # Extract outcome variable name from model formula
  outcome_raw <- all.vars(model$formula)[1L]

  if (exists("get_label", mode = "function")) {
    outcome <- get_label(model$model, outcome_raw)
  } else {
    outcome <- outcome_raw
  }

  # Extract tau levels from model
  tau <- if (is.null(model$tau)) NA_real_ else model$tau

  # Auto-detect quantreg version
  quantreg.version <- if (requireNamespace("quantreg", quietly = TRUE)) {
    as.character(utils::packageVersion("quantreg"))
  } else {
    "unknown"
  }

  analysis.date <- as.character(Sys.Date())
  r_version     <- paste0(R.version$major, ".", R.version$minor)

  # Tau text
  if (lang == "en") {
    tau_text <- if (length(tau) == 1L && !is.na(tau)) {
      sprintf("the %s quantile", tau)
    } else if (length(tau) > 1L) {
      sprintf("the %s quantiles", paste(tau, collapse = ", "))
    } else {
      "the fitted quantile"
    }
  } else {
    tau_text <- if (length(tau) == 1L && !is.na(tau)) {
      sprintf("%s\u5206\u4f4d\u6570", tau)
    } else if (length(tau) > 1L) {
      sprintf("%s\u5206\u4f4d\u6570", paste(tau, collapse = "\u3001"))
    } else {
      "\u62df\u5408\u7684\u5206\u4f4d\u6570"
    }
  }

  # SE method text
  if (lang == "en") {
    se_text <- switch(
      se.method,
      boot     = sprintf("bootstrap with %d replications", R),
      rank     = "inversion of the quantile rank test",
      iid      = paste("asymptotic covariance assuming independent",
                       "and identically distributed errors"),
      nid      = "Huber sandwich estimation under local linearity",
      ker      = "kernel-based sandwich estimation",
      BLB      = "bag of little bootstraps",
      conquer  = "multiplier bootstrap for conquer-fitted models",
      extreme  = "subsampling for extreme quantiles",
      "the selected method"
    )
  } else {
    se_text <- switch(
      se.method,
      boot     = sprintf("\u5357\u7f6e\u91cd\u62bd\u6837\uff08%d\u6b21\u91cd\u590d\u62bd\u6837\uff09", R),
      rank     = "\u5206\u4f4d\u6570\u79e9\u68c0\u9a8c\u53cd\u8f6c\u6cd5",
      iid      = "\u5047\u5b9a\u72ec\u7acb\u540c\u5206\u5e03\u8bef\u5dee\u7684\u6e10\u8fd1\u534f\u65b9\u5dee\u6cd5",
      nid      = "\u5c40\u90e8\u7ebf\u6027\u4e0b\u7684Huber\u4e09\u660e\u6cbb\u4f30\u8ba1",
      ker      = "\u57fa\u4e8e\u6838\u7684\u4e09\u660e\u6cbb\u4f30\u8ba1",
      BLB      = "Bag of Little Bootstraps",
      conquer  = "Conquer\u62df\u5408\u6a21\u578b\u7684\u4e58\u79efBootstrap\u6cd5",
      extreme  = "\u6781\u7aef\u5206\u4f4d\u6570\u4e0b\u7684\u5b50\u62bd\u6837\u6cd5",
      "\u6240\u9009\u65b9\u6cd5"
    )
  }

  # Significance text
  if (lang == "en") {
    alt_text <- switch(
      alternative,
      two.sided = "two-sided",
      greater   = "one-sided (greater)",
      less      = "one-sided (less)"
    )
    sig_text <- sprintf(
      "A %s P < %g was considered statistically significant.",
      alt_text, alpha
    )
  } else {
    alt_text <- switch(
      alternative,
      two.sided = "\u53cc\u4fa7",
      greater   = "\u5355\u4fa7\uff08\u5927\u4e8e\u65b9\u5411\uff09",
      less      = "\u5355\u4fa7\uff08\u5c0f\u4e8e\u65b9\u5411\uff09"
    )
    sig_text <- sprintf(
      "\u4ee5%s\u68c0\u9a8cP<%g\u4e3a\u5dee\u5f02\u6709\u7edf\u8ba1\u5b66\u610f\u4e49\u3002",
      alt_text, alpha
    )
  }

  # Build paragraph
  if (lang == "en") {

    paragraph <- sprintf(
      paste(
        "Quantile regression models were used to estimate the",
        "association between covariates and %s.",
        "Models were fitted at %s using the pinball loss function.",
        "Standard errors and 95%% confidence intervals were",
        "computed by %s.",
        "%s",
        "All statistical analyses were performed using R software",
        "(version %s) with the quantreg package (version %s) on %s.",
        sep = " "
      ),
      outcome, tau_text, se_text, sig_text,
      r_version, quantreg.version, analysis.date
    )

  } else {

    paragraph <- sprintf(
      paste(
        "\u5206\u4f4d\u6570\u56de\u5f52\u6a21\u578b\u7528\u4e8e\u4f30\u8ba1\u6f5c\u5728\u5f71\u54cd\u56e0\u7d20\u4e0e%s\u4e4b\u95f4\u7684\u5173\u8054\u3002",
        "\u6a21\u578b\u5728%s\u5904\u8fdb\u884c\u62df\u5408\u3002",
        "\u6807\u51c6\u8bef\u548c95%%\u7f6e\u4fe1\u533a\u95f4\u91c7\u7528%s\u8ba1\u7b97\u3002",
        "%s",
        "\u6240\u6709\u7edf\u8ba1\u5206\u6790\u5747\u4f7f\u7528R\u8f6f\u4ef6(\u7248\u672c%s)\u8fdb\u884c\u5206\u6790\uff0c\u5176\u4e2dquantreg\u5305(\u7248\u672c%s)\u7528\u4e8e\u62df\u5408\u5206\u4f4d\u6570\u56de\u5f52\u6a21\u578b\uff0c\u5e76\u4e8e%s\u5b8c\u6210\u3002",
        sep = ""
      ),
      outcome, tau_text, se_text, sig_text,
      r_version, quantreg.version, analysis.date
    )
  }

  paragraph
}

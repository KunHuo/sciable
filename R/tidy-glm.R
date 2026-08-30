tidy_glm <- function(model,
                    digits = 2,
                    digits.pvalue = 3,
                    intercept = FALSE,
                    stats = c("estp", "full", "est"),
                    lang = c("en", "cn")) {

  # Match argument values
  stats <- match.arg(stats)
  lang <- match.arg(lang)

  # Extract model variables and data
  vars <- attr(stats::terms(model$formula),"term.labels")
  dependent <- all.vars(model$formula)[1]
  data <- model$model

  # Get variable labels
  tvars <- tidy_vars(data, vars = vars, intercept = intercept)

  # Tidy model coefficients
  coefs <- broom::tidy(
    model,
    conf.int = TRUE,
    conf.level = 0.95
  )

  # Format estimates and confidence intervals
  coefs <- format_estimate_ci(coefs, digits = digits)

  # Format P values
  coefs$p.value <- format_pvalue(coefs$p.value, digits.pvalue)

  # Select statistics to display
  selected <- select_stats(stats)

  coefs <- coefs[c("term", selected)]

  # Rename display columns
  names(coefs) <- dplyr::recode(names(coefs), !!!get_stats_labels(lang))

  coefs <- dplyr::left_join(tvars, coefs, by = "term")

  coefs <- coefs[-1]

  # Rename variable column
  names(coefs)[1] <- get_variable_label(lang)

  coefs <- add_estimate_reference(coefs, lang = lang)

  #
  # title.label <- get_label(data, dependent)
  #
  # title <- switch(lang,
  #                 en = sprintf("Table: Quantile regression estimates of factors associated with %s", title.label),
  #                 cn = sprintf("\u8868\uff1a\u5206\u4f4d\u6570\u56de\u5f52\u4f30\u8ba1\u4e0e%s\u76f8\u5173\u7684\u56e0\u7d20", title.label))
  #
  # note <- .add_quantreg_stat_methods(model, se.method = se.method, R= R, lang = lang)
  #
  # attr(coefs, "title") <- title
  # attr(coefs, "note")  <- note

  class(coefs) <- c("booktabs", "data.frame")

  coefs
}

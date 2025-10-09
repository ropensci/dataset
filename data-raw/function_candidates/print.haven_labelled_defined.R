print.haven_labelled_defined <- function(x, ...) {
  has_def   <- !is.null(var_concept(x)) && !is.na(var_concept(x)) && nzchar(var_concept(x))
  has_unit  <- !is.null(var_unit(x)) && !is.na(var_unit(x)) && nzchar(var_unit(x))
  has_label <- !is.null(var_label(x))

  cat(deparse(substitute(x)))
  if (has_label) cat(paste0(": ", var_label(x)))
  cat("\n")

  if (has_def && has_unit) {
    msg <- paste0("Defined as ", var_concept(x), ", measured in ", var_unit(x))
  } else if (has_def) {
    msg <- paste0("Defined as ", var_concept(x))
  } else if (has_unit) {
    msg <- paste0("Measured in ", var_unit(x))
  } else {
    msg <- "Defined vector"
  }
  cat(msg, "\n")

  # show code + label if available
  codes  <- vctrs::vec_data(x)
  labels <- attr(x, "labels")
  levels <- attr(x, "levels")

  if (!is.null(labels) && !is.null(levels)) {
    mapped <- labels[match(codes, levels)]
    out <- paste0(codes, " [", mapped, "]")
    print(noquote(out), ...)
  } else {
    print(codes, ...)
  }

  invisible(x)
}

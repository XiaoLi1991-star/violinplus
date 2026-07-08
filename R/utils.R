`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

check_scalar_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-empty string.", call. = FALSE)
  }
  x
}

check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a positive number.", call. = FALSE)
  }
  invisible(x)
}

as_column_name <- function(x, arg) {
  check_scalar_string(x, arg)
}

require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  invisible(data)
}

compact_list <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

resolve_auto_dimension <- function(value, default, arg) {
  if (is.character(value) && length(value) == 1L && identical(tolower(trimws(value)), "auto")) {
    return(default)
  }
  value <- suppressWarnings(as.numeric(value))
  check_positive_number(value, arg)
  value
}

optional_namespace <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

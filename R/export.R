#' Save a violinplus plot
#'
#' @param plot A plot returned by [violin_plot()].
#' @param filename Output file path.
#' @param width,height Plot size in inches. Use `"auto"` for attached recommendations.
#' @param dpi Raster resolution.
#' @param bg Background color.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return The normalized output filename, invisibly.
#' @export
save_violin <- function(plot,
                        filename,
                        width = "auto",
                        height = "auto",
                        dpi = 320,
                        bg = "white",
                        ...) {
  filename <- check_scalar_string(filename, "filename")
  ext <- tolower(tools::file_ext(filename))
  if (!nzchar(ext)) {
    stop("`filename` must include a file extension.", call. = FALSE)
  }
  params <- attr(plot, "violinplus_params", exact = TRUE) %||% list()
  width <- resolve_auto_dimension(width, params$width %||% 7.2, "width")
  height <- resolve_auto_dimension(height, params$height %||% 5.4, "height")
  check_positive_number(dpi, "dpi")

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg,
    ...
  )
  invisible(normalizePath(filename, mustWork = FALSE))
}

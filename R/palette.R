palette_definitions <- function() {
  list(
    npg_red_blue = c(fill = "#E64B35", accent = "#4DBBD5", point = "#3C5488", line = "#242A32", source = "NPG"),
    lancet_blue_red = c(fill = "#00468B", accent = "#ED0000", point = "#1B1919", line = "#1B1919", source = "Lancet"),
    nejm_red_blue = c(fill = "#BC3C29", accent = "#0072B5", point = "#E18727", line = "#2B2B2B", source = "NEJM"),
    jama_teal_orange = c(fill = "#374E55", accent = "#DF8F44", point = "#00A1D5", line = "#263238", source = "JAMA"),
    jco_blue_gold = c(fill = "#0073C2", accent = "#EFC000", point = "#CD534C", line = "#003C67", source = "JCO"),
    aaas_blue_red = c(fill = "#3B4992", accent = "#EE0000", point = "#008B45", line = "#1B1919", source = "AAAS"),
    npg_green_apricot = c(fill = "#00A087", accent = "#F39B7F", point = "#3C5488", line = "#24313D", source = "NPG"),
    lancet_green_purple = c(fill = "#42B540", accent = "#925E9F", point = "#00468B", line = "#1B1919", source = "Lancet"),
    nejm_orange_green = c(fill = "#E18727", accent = "#20854E", point = "#0072B5", line = "#2B2B2B", source = "NEJM"),
    jama_cyan_red = c(fill = "#00A1D5", accent = "#B24745", point = "#374E55", line = "#263238", source = "JAMA"),
    nature_violet_lime = c(fill = "#6A3D9A", accent = "#B2DF8A", point = "#1F78B4", line = "#232B35", source = "Nature-inspired"),
    cell_magenta_green = c(fill = "#C51B7D", accent = "#4DAC26", point = "#2166AC", line = "#252525", source = "Cell-inspired"),
    jco_red_sky = c(fill = "#CD534C", accent = "#7AA6DC", point = "#003C67", line = "#263238", source = "JCO"),
    aaas_purple_teal = c(fill = "#631879", accent = "#008280", point = "#EE0000", line = "#1B1919", source = "AAAS")
  )
}

#' List violinplus palettes
#'
#' @return A data frame of palette names and colors.
#' @export
violin_palettes <- function() {
  palettes <- palette_definitions()
  data.frame(
    name = names(palettes),
    fill = vapply(palettes, `[[`, character(1), "fill"),
    accent = vapply(palettes, `[[`, character(1), "accent"),
    point = vapply(palettes, `[[`, character(1), "point"),
    line = vapply(palettes, `[[`, character(1), "line"),
    source = vapply(palettes, `[[`, character(1), "source"),
    stringsAsFactors = FALSE
  )
}

resolve_violin_palette <- function(name) {
  palettes <- palette_definitions()
  name <- check_scalar_string(name, "palette")
  if (!name %in% names(palettes)) {
    stop("Unknown palette: ", name, ".", call. = FALSE)
  }
  palettes[[name]]
}

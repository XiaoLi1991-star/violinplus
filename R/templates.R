template_definitions <- function() {
  list(
    violin_box = list(
      id = "violin_box",
      title = "Violin Box",
      description = "Violin distributions with compact boxplot summaries for publication-ready group comparison.",
      palette = "npg_red_blue",
      thumbnail = "violinplus-violin_box.png",
      default_params = list(layers = c("violin", "box"), show_points = FALSE, show_box = TRUE, compare = FALSE)
    ),
    violin_jitter = list(
      id = "violin_jitter",
      title = "Violin Points",
      description = "Violin distributions with raw jittered observations to keep sample spread and outliers visible.",
      palette = "lancet_blue_red",
      thumbnail = "violinplus-violin_jitter.png",
      default_params = list(layers = c("violin", "points"), show_points = TRUE, show_box = FALSE, compare = FALSE)
    ),
    box_jitter = list(
      id = "box_jitter",
      title = "Box Points",
      description = "Conservative boxplot summaries overlaid with raw observations, useful for small samples.",
      palette = "nejm_red_blue",
      thumbnail = "violinplus-box_jitter.png",
      default_params = list(layers = c("box", "points"), show_points = TRUE, show_box = TRUE, compare = FALSE)
    ),
    violin_only = list(
      id = "violin_only",
      title = "Clean Violin",
      description = "Minimal violin distributions with a median cue for many-group comparisons.",
      palette = "jama_teal_orange",
      thumbnail = "violinplus-violin_only.png",
      default_params = list(layers = c("violin", "median"), show_points = FALSE, show_box = FALSE, compare = FALSE)
    ),
    raincloud = list(
      id = "raincloud",
      title = "Raincloud",
      description = "Half-density style view combining distribution shape, summary, and raw observations.",
      palette = "jco_blue_gold",
      thumbnail = "violinplus-raincloud.png",
      default_params = list(layers = c("half_violin", "box", "points"), show_points = TRUE, show_box = TRUE, compare = FALSE)
    ),
    half_violin_box = list(
      id = "half_violin_box",
      title = "Half Violin Box",
      description = "Space-efficient half-violin and boxplot composition for dense group comparison.",
      palette = "aaas_blue_red",
      thumbnail = "violinplus-half_violin_box.png",
      default_params = list(layers = c("half_violin", "box"), show_points = FALSE, show_box = TRUE, compare = FALSE)
    ),
    beeswarm_summary = list(
      id = "beeswarm_summary",
      title = "Beeswarm Summary",
      description = "Beeswarm-style points with robust median and IQR cues for non-overlapping raw data display.",
      palette = "npg_green_apricot",
      thumbnail = "violinplus-beeswarm_summary.png",
      default_params = list(layers = c("beeswarm", "summary"), show_points = TRUE, show_box = FALSE, compare = FALSE)
    ),
    sina_density = list(
      id = "sina_density",
      title = "Sina Density",
      description = "Density-controlled point spread that reads like a compact raw-data violin.",
      palette = "lancet_green_purple",
      thumbnail = "violinplus-sina_density.png",
      default_params = list(layers = c("sina", "median"), show_points = TRUE, show_box = FALSE, compare = FALSE)
    ),
    two_group_sig = list(
      id = "two_group_sig",
      title = "Two Group Test",
      description = "Two-group distribution comparison with a single significance bracket.",
      palette = "nejm_orange_green",
      thumbnail = "violinplus-two_group_sig.png",
      default_params = list(layers = c("violin", "box", "significance"), show_points = FALSE, show_box = TRUE, compare = TRUE)
    ),
    multi_group_sig = list(
      id = "multi_group_sig",
      title = "Multi Group Test",
      description = "Multi-group distributions with global and adjusted pairwise comparison labels.",
      palette = "jama_cyan_red",
      thumbnail = "violinplus-multi_group_sig.png",
      default_params = list(layers = c("box", "points", "significance"), show_points = TRUE, show_box = TRUE, compare = TRUE)
    ),
    violin_box_letter = list(
      id = "violin_box_letter",
      title = "Violin Box Letters",
      description = "Violin, boxplot, and raw points with compact letter display for multi-group post-hoc comparison.",
      palette = "nature_violet_lime",
      thumbnail = "violinplus-violin_box_letter.png",
      default_params = list(layers = c("violin", "box", "points", "letters"), show_points = TRUE, show_box = TRUE, compare = TRUE, annotation = "letters")
    ),
    split_violin_letter = list(
      id = "split_violin_letter",
      title = "Bordered Half Violin Letters",
      description = "A compact half-violin-inspired bordered template with box summaries and letter-based group differences.",
      palette = "cell_magenta_green",
      thumbnail = "violinplus-split_violin_letter.png",
      default_params = list(layers = c("half_violin", "box", "letters"), show_points = FALSE, show_box = TRUE, compare = TRUE, annotation = "letters")
    ),
    paired_change = list(
      id = "paired_change",
      title = "Paired Change",
      description = "Before-after paired samples with subject-level connecting lines and compact summaries.",
      palette = "jco_red_sky",
      thumbnail = "violinplus-paired_change.png",
      default_params = list(layers = c("points", "lines", "box"), show_points = TRUE, show_box = TRUE, compare = TRUE)
    ),
    facet_grid = list(
      id = "facet_grid",
      title = "Facet Grid",
      description = "Repeated distribution template across biomarkers, tissues, timepoints, or subgroups.",
      palette = "aaas_purple_teal",
      thumbnail = "violinplus-facet_grid.png",
      default_params = list(layers = c("violin", "box", "facet"), show_points = FALSE, show_box = TRUE, compare = FALSE)
    )
  )
}

#' List available violinplus plot templates
#'
#' @return A data frame with template IDs, titles, descriptions, palettes, thumbnails, and default parameter lists.
#' @export
violin_templates <- function() {
  defs <- template_definitions()
  data.frame(
    id = vapply(defs, `[[`, character(1), "id"),
    title = vapply(defs, `[[`, character(1), "title"),
    description = vapply(defs, `[[`, character(1), "description"),
    palette = vapply(defs, `[[`, character(1), "palette"),
    thumbnail = vapply(defs, `[[`, character(1), "thumbnail"),
    stringsAsFactors = FALSE
  ) |>
    add_default_params(unname(lapply(defs, `[[`, "default_params")))
}

add_default_params <- function(df, default_params) {
  df$default_params <- I(default_params)
  df
}

#' Resolve a violinplus template
#'
#' @param template Template ID, title, or numeric position from [violin_templates()].
#'
#' @return A named list describing the template.
#' @export
resolve_violin_template <- function(template = "violin_box") {
  defs <- template_definitions()
  if (is.numeric(template) && length(template) == 1L && !is.na(template)) {
    idx <- as.integer(template)
    if (idx >= 1L && idx <= length(defs)) {
      return(defs[[idx]])
    }
  }
  template <- check_scalar_string(as.character(template), "template")
  keys <- names(defs)
  titles <- tolower(vapply(defs, `[[`, character(1), "title"))
  match_idx <- match(tolower(template), c(keys, titles))
  if (!is.na(match_idx)) {
    if (match_idx > length(keys)) {
      match_idx <- match_idx - length(keys)
    }
    return(defs[[match_idx]])
  }
  stop("Unknown template: ", template, ".", call. = FALSE)
}

#' List example mappings for the single violinplus demo table
#'
#' @return A data frame with one example story and mapping per template.
#' @export
violin_template_examples <- function() {
  rows <- list(
    list(
      template = "violin_box",
      claim = "Week 8 IL6 separates clinical response classes.",
      filter_label = "IL6 at Week 8 across all cohorts",
      filter = function(data) data$facet == "IL6" & data$pair == "Week 8" & !is.na(data$value),
      x = "response",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "IL6 separates response classes",
      ylab = "IL6 (pg/mL)"
    ),
    list(
      template = "violin_jitter",
      claim = "Validation cohort retains CRP dose separation across baseline and Week 8.",
      filter_label = "CRP across time in validation cohort",
      filter = function(data) data$facet == "CRP" & data$cohort == "Validation" & !is.na(data$value),
      x = "pair",
      fill_col = "group",
      y = "value",
      facet = "",
      subject = "",
      title = "Validation CRP by time and dose",
      ylab = "CRP (mg/L)"
    ),
    list(
      template = "box_jitter",
      claim = "Albumin shifts are modest and best shown with conservative summaries.",
      filter_label = "Albumin at Week 8 in discovery cohort",
      filter = function(data) data$facet == "Albumin" & data$pair == "Week 8" & data$cohort == "Discovery" & !is.na(data$value),
      x = "group",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "Discovery albumin shift",
      ylab = "Albumin (g/L)"
    ),
    list(
      template = "violin_only",
      claim = "TNF-alpha has a broad treatment-associated distribution without needing raw-point clutter.",
      filter_label = "TNF-alpha at Week 8",
      filter = function(data) data$facet == "TNF-alpha" & data$pair == "Week 8" & !is.na(data$value),
      x = "group",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "TNF-alpha distribution",
      ylab = "TNF-alpha (pg/mL)"
    ),
    list(
      template = "raincloud",
      claim = "Responder, stable, and progressor strata show distinct IL6 distributions with raw observations visible.",
      filter_label = "IL6 at Week 8 by response",
      filter = function(data) data$facet == "IL6" & data$pair == "Week 8" & !is.na(data$value),
      x = "response",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "IL6 by response stratum",
      ylab = "IL6 (pg/mL)"
    ),
    list(
      template = "half_violin_box",
      claim = "CRP treatment distributions remain separated when shown as a compact density-summary panel.",
      filter_label = "CRP at Week 8",
      filter = function(data) data$facet == "CRP" & data$pair == "Week 8" & !is.na(data$value),
      x = "group",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "Compact CRP treatment profile",
      ylab = "CRP (mg/L)"
    ),
    list(
      template = "beeswarm_summary",
      claim = "TNF-alpha response strata show individual-level spread rather than only summary separation.",
      filter_label = "TNF-alpha at Week 8 by response",
      filter = function(data) data$facet == "TNF-alpha" & data$pair == "Week 8" & !is.na(data$value),
      x = "response",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "TNF-alpha response spread",
      ylab = "TNF-alpha (pg/mL)"
    ),
    list(
      template = "sina_density",
      claim = "Discovery and validation cohorts have comparable Week 8 IL6 distributions.",
      filter_label = "IL6 at Week 8 by cohort",
      filter = function(data) data$facet == "IL6" & data$pair == "Week 8" & !is.na(data$value),
      x = "cohort",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "Cohort-level IL6 replication",
      ylab = "IL6 (pg/mL)"
    ),
    list(
      template = "two_group_sig",
      claim = "Progressors have higher Week 8 IL6 than responders.",
      filter_label = "IL6 at Week 8, responder versus progressor",
      filter = function(data) data$facet == "IL6" & data$pair == "Week 8" & data$response %in% c("Responder", "Progressor") & !is.na(data$value),
      x = "response",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "Progressor IL6 elevation",
      ylab = "IL6 (pg/mL)"
    ),
    list(
      template = "multi_group_sig",
      claim = "CRP follows a graded Week 8 treatment response.",
      filter_label = "CRP at Week 8",
      filter = function(data) data$facet == "CRP" & data$pair == "Week 8" & !is.na(data$value),
      x = "group",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "CRP graded treatment response",
      ylab = "CRP (mg/L)"
    ),
    list(
      template = "violin_box_letter",
      claim = "Albumin shows ordered treatment separation that is clearer with compact letters than bracket clutter.",
      filter_label = "Albumin at Week 8",
      filter = function(data) data$facet == "Albumin" & data$pair == "Week 8" & !is.na(data$value),
      x = "group",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "Albumin treatment letters",
      ylab = "Albumin (g/L)"
    ),
    list(
      template = "split_violin_letter",
      claim = "TNF-alpha response classes can be summarized with a compact bordered half-violin and letter display.",
      filter_label = "TNF-alpha at Week 8 by response",
      filter = function(data) data$facet == "TNF-alpha" & data$pair == "Week 8" & !is.na(data$value),
      x = "response",
      fill_col = "",
      y = "value",
      facet = "",
      subject = "",
      title = "TNF-alpha letter groups",
      ylab = "TNF-alpha (pg/mL)"
    ),
    list(
      template = "paired_change",
      claim = "Responders show coordinated within-subject biomarker improvement by Week 8.",
      filter_label = "Responder paired baseline and Week 8 values across markers",
      filter = function(data) data$response == "Responder" & data$facet %in% c("IL6", "CRP", "TNF-alpha", "Albumin") & !is.na(data$value),
      x = "pair",
      fill_col = "",
      y = "value",
      facet = "facet",
      subject = "subject",
      title = "Responder biomarker trajectories",
      ylab = "Normalized biomarker value"
    ),
    list(
      template = "facet_grid",
      claim = "Treatment trajectories are marker-specific across baseline and Week 8.",
      filter_label = "All marker values across time and treatment groups",
      filter = function(data) !is.na(data$value),
      x = "pair",
      fill_col = "group",
      y = "value",
      facet = "facet",
      subject = "",
      title = "Marker treatment trajectories",
      ylab = "Normalized biomarker value"
    )
  )
  out <- data.frame(
    template = vapply(rows, `[[`, character(1), "template"),
    claim = vapply(rows, `[[`, character(1), "claim"),
    filter_label = vapply(rows, `[[`, character(1), "filter_label"),
    x = vapply(rows, `[[`, character(1), "x"),
    fill_col = vapply(rows, `[[`, character(1), "fill_col"),
    y = vapply(rows, `[[`, character(1), "y"),
    facet = vapply(rows, `[[`, character(1), "facet"),
    subject = vapply(rows, `[[`, character(1), "subject"),
    title = vapply(rows, `[[`, character(1), "title"),
    ylab = vapply(rows, `[[`, character(1), "ylab"),
    stringsAsFactors = FALSE
  )
  out$filter <- I(lapply(rows, `[[`, "filter"))
  out
}

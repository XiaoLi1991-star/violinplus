#' Inspect data and recommend violinplus plot defaults
#'
#' @param data A data frame.
#' @param x,y Column names for group and numeric value.
#' @param facet Optional facet column.
#' @param subject Optional subject ID column for paired templates.
#' @param template Template ID passed to [resolve_violin_template()].
#'
#' @return A `violinplus_inspection` list with metrics, resolved parameters, and risk flags.
#' @export
inspect_violin_plot <- function(data,
                                x,
                                y,
                                facet = NULL,
                                subject = NULL,
                                template = "violin_box") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  x <- as_column_name(x, "x")
  y <- as_column_name(y, "y")
  facet <- if (is.null(facet)) NULL else as_column_name(facet, "facet")
  subject <- if (is.null(subject)) NULL else as_column_name(subject, "subject")
  require_columns(data, compact_list(list(x, y, facet, subject)))

  template_def <- resolve_violin_template(template)
  values <- suppressWarnings(as.numeric(data[[y]]))
  if (all(is.na(values))) {
    stop("`y` must contain numeric values.", call. = FALSE)
  }

  groups <- unique(as.character(data[[x]][!is.na(data[[x]])]))
  facets <- if (is.null(facet)) "all" else unique(as.character(data[[facet]][!is.na(data[[facet]])]))
  n <- sum(!is.na(values))
  n_groups <- length(groups)
  n_facets <- length(facets)
  n_per_group <- if (n_groups > 0L) stats::setNames(as.integer(table(data[[x]])), names(table(data[[x]]))) else integer()
  min_n_per_group <- if (length(n_per_group) > 0L) min(n_per_group) else 0L

  small_sample <- min_n_per_group > 0L && min_n_per_group < 12L
  many_groups <- n_groups > 6L
  many_facets <- n_facets > 4L

  template_params <- template_def$default_params
  show_points <- isTRUE(template_params$show_points)
  if (small_sample && !template_def$id %in% c("facet_grid")) {
    show_points <- TRUE
  }
  if (many_groups && !template_def$id %in% c("beeswarm_summary", "sina_density")) {
    show_points <- FALSE
  }

  facet_cols <- if (n_facets <= 1L) 1L else min(3L, ceiling(sqrt(n_facets)))
  facet_extra <- max(0L, n_facets - 1L)
  width <- round(3.9 + max(0L, n_groups - 2L) * 0.25 + min(facet_extra, 1L) * 0.85 + max(0L, facet_extra - 1L) * 0.25, 1)
  height <- round(3.2 + min(facet_extra, 1L) * 1.2 + max(0L, facet_extra - 1L) * 0.45 + if (many_groups) 0.45 else 0, 1)
  point_size <- if (small_sample) 1.7 else if (n > 120L) 0.8 else 1.15
  point_alpha <- if (n > 120L) 0.42 else if (n > 60L) 0.58 else 0.76
  base_size <- if (many_facets || many_groups) 8.2 else 8.8

  comparison_method <- NULL
  p_adjust_method <- "none"
  if (isTRUE(template_params$compare)) {
    if (n_groups <= 2L) {
      comparison_method <- "wilcox.test"
    } else {
      comparison_method <- "kruskal.test"
      p_adjust_method <- "BH"
    }
  }

  structure(
    list(
      metrics = list(
        n = as.integer(n),
        n_groups = as.integer(n_groups),
        n_facets = as.integer(n_facets),
        min_n_per_group = as.integer(min_n_per_group)
      ),
      resolved_params = list(
        template = template_def$id,
        palette = template_def$palette,
        width = width,
        height = height,
        dpi = 320,
        show_points = show_points,
        show_box = isTRUE(template_params$show_box),
        point_size = point_size,
        point_alpha = point_alpha,
        base_size = base_size,
        facet_cols = as.integer(facet_cols),
        comparison_method = comparison_method,
        p_adjust_method = p_adjust_method
      ),
      risks = list(
        small_sample = isTRUE(small_sample),
        many_groups = isTRUE(many_groups),
        many_facets = isTRUE(many_facets),
        raw_points_may_overplot = isTRUE(show_points && n > 120L)
      )
    ),
    class = c("violinplus_inspection", "list")
  )
}

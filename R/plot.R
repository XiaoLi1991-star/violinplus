#' Draw a violinplus distribution plot
#'
#' @param data A data frame.
#' @param x,y Column names for group and numeric value.
#' @param fill_col Optional column used for fill and color grouping. Defaults to `x`.
#' @param template Template ID from [violin_templates()].
#' @param facet Optional facet column.
#' @param subject Optional subject ID column for paired templates.
#' @param palette Palette name from [violin_palettes()]. Defaults to the template palette.
#' @param comparisons Optional list of length-two character vectors naming groups to compare.
#' @param p_label Use `"p.signif"` for stars, `"p.format"` for formatted p values, or `"letters"` for compact group letters.
#' @param title,subtitle,caption Plot text.
#' @param xlab,ylab Axis labels.
#' @param show_points,show_box Optional layer overrides. Defaults come from template and inspection.
#' @param legend_position Optional legend position: `"none"`, `"bottom"`, `"right"`, `"left"`, or `"top"`.
#'   When `NULL`, grouped-fill plots use `"bottom"` and single-fill plots use `"none"`.
#' @param facet_cols Optional number of facet columns. When `NULL`, a compact automatic value is used.
#' @param orientation Plot orientation, either `"vertical"` or `"horizontal"`.
#' @param width,height Output dimensions or `"auto"` for attached metadata.
#' @param print_params Print resolved parameters for reproducible platform tuning.
#' @param ... Reserved for future template controls.
#'
#' @return A ggplot object with `violinplus_params` metadata.
#' @export
violin_plot <- function(data,
                        x,
                        y,
                        fill_col = NULL,
                        template = "violin_box",
                        facet = NULL,
                        subject = NULL,
                        palette = NULL,
                        comparisons = NULL,
                        p_label = c("p.signif", "p.format", "letters"),
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        show_points = NULL,
                        show_box = NULL,
                        legend_position = NULL,
                        facet_cols = NULL,
                        orientation = c("vertical", "horizontal"),
                        width = "auto",
                        height = "auto",
                        print_params = TRUE,
                        ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  x <- as_column_name(x, "x")
  y <- as_column_name(y, "y")
  fill_col <- if (is.null(fill_col)) x else as_column_name(fill_col, "fill_col")
  facet <- if (is.null(facet)) NULL else as_column_name(facet, "facet")
  subject <- if (is.null(subject)) NULL else as_column_name(subject, "subject")
  require_columns(data, compact_list(list(x, y, fill_col, facet, subject)))

  template_def <- resolve_violin_template(template)
  palette <- palette %||% template_def$palette
  pal <- resolve_violin_palette(palette)
  orientation <- resolve_orientation(orientation)
  p_label_missing <- missing(p_label)
  p_label <- match.arg(p_label)
  if (isTRUE(p_label_missing) && identical(template_def$default_params$annotation, "letters")) {
    p_label <- "letters"
  }

  plot_data <- data
  plot_data[[y]] <- suppressWarnings(as.numeric(plot_data[[y]]))
  plot_data <- plot_data[!is.na(plot_data[[x]]) & !is.na(plot_data[[fill_col]]) & !is.na(plot_data[[y]]), , drop = FALSE]
  if (nrow(plot_data) == 0L) {
    stop("No complete rows remain for `x`, `fill_col`, and `y`.", call. = FALSE)
  }
  plot_data[[x]] <- factor(plot_data[[x]], levels = unique(as.character(plot_data[[x]])))
  plot_data[[fill_col]] <- factor(plot_data[[fill_col]], levels = unique(as.character(plot_data[[fill_col]])))
  if (template_def$id == "paired_change" && !is.null(subject)) {
    line_parts <- list(plot_data[[subject]])
    if (!is.null(facet)) {
      line_parts[[length(line_parts) + 1L]] <- plot_data[[facet]]
    }
    if ("group" %in% names(plot_data)) {
      line_parts[[length(line_parts) + 1L]] <- plot_data[["group"]]
    }
    plot_data$.violinplus_line_group <- interaction(line_parts, drop = TRUE, sep = "__")
  }

  inspection <- inspect_violin_plot(plot_data, x = x, y = y, facet = facet, subject = subject, template = template_def$id)
  resolved <- inspection$resolved_params
  resolved$x <- x
  resolved$fill_col <- fill_col
  resolved$fill_grouped <- !identical(fill_col, x)
  resolved$legend_position <- resolve_legend_position(legend_position, resolved$fill_grouped)
  if (!is.null(facet_cols)) {
    resolved$facet_cols <- check_positive_integer(facet_cols, "facet_cols")
  }
  resolved$orientation <- orientation
  resolved$palette <- palette
  resolved$width <- resolve_auto_dimension(width, resolved$width, "width")
  resolved$height <- resolve_auto_dimension(height, resolved$height, "height")
  if (!is.null(show_points)) {
    resolved$show_points <- isTRUE(show_points)
  }
  if (!is.null(show_box)) {
    resolved$show_box <- isTRUE(show_box)
  }
  resolved$p_label <- p_label
  resolved$annotation <- template_def$default_params$annotation %||% "bracket"
  resolved$compare <- isTRUE(template_def$default_params$compare) || length(comparisons) > 0L

  if (isTRUE(print_params)) {
    print_violin_params(resolved)
  }

  fill_values <- template_fill_values(plot_data[[fill_col]], pal)
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      fill = .data[[fill_col]],
      color = .data[[fill_col]],
      group = interaction(.data[[x]], .data[[fill_col]], drop = TRUE)
    )
  )
  p <- add_template_layers(p, plot_data, x, y, fill_col, subject, template_def$id, resolved, pal)

  if (isTRUE(resolved$compare)) {
    p <- add_comparison_annotations(p, plot_data, x, y, comparisons, p_label, pal)
  }

  if (!is.null(facet)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)), ncol = resolved$facet_cols, scales = "fixed")
  }

  p <- p +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::scale_color_manual(values = fill_values, guide = "none") +
    ggplot2::guides(fill = legend_guide(resolved, pal)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = xlab %||% x,
      y = ylab %||% y,
      fill = fill_col,
      color = fill_col
    ) +
    theme_violinplus(base_size = resolved$base_size) +
    theme_violinplus_facets(!is.null(facet)) +
    theme_violinplus_legend(resolved) +
    plot_coordinate(resolved)

  attr(p, "violinplus_params") <- resolved
  p
}

template_fill_values <- function(groups, pal) {
  levels <- levels(groups)
  if (is.null(levels)) {
    levels <- unique(as.character(groups))
  }
  anchors <- unname(pal[c("fill", "accent", "point")])
  if (length(levels) <= length(anchors)) {
    return(stats::setNames(anchors[seq_along(levels)], levels))
  }
  ramp <- grDevices::colorRampPalette(anchors)
  stats::setNames(ramp(length(levels)), levels)
}

add_template_layers <- function(p, data, x, y, fill_col, subject, template_id, resolved, pal) {
  dodge <- layer_dodge_position(resolved)
  if (template_id %in% c("violin_box", "violin_jitter", "violin_only", "two_group_sig", "facet_grid", "violin_box_letter")) {
    p <- p + ggplot2::geom_violin(width = 0.86, alpha = 0.72, linewidth = 0.35, trim = FALSE, position = dodge)
  } else if (template_id %in% c("raincloud", "half_violin_box", "split_violin_letter")) {
    p <- add_half_violin_layer(p, alpha = 0.72, position = dodge)
    p <- add_half_violin_mask(p, data, x)
  }

  if (template_id %in% c("box_jitter", "multi_group_sig", "paired_change", "violin_box_letter", "split_violin_letter") || isTRUE(resolved$show_box)) {
    p <- p + ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.88, color = unname(pal[["line"]]), linewidth = 0.36, position = dodge)
  }

  if (template_id == "paired_change" && !is.null(subject)) {
    line_group <- if (".violinplus_line_group" %in% names(data)) ".violinplus_line_group" else subject
    p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[line_group]]), color = "#8A93A3", alpha = 0.42, linewidth = 0.32)
  }

  if (isTRUE(resolved$show_points) || template_id %in% c("beeswarm_summary", "sina_density", "paired_change")) {
    p <- add_point_layer(p, template_id, resolved)
  }

  if (template_id %in% c("violin_only", "sina_density", "beeswarm_summary")) {
    p <- add_median_segments(p, data, x, y, fill_col, resolved, pal)
  }

  p
}

layer_dodge_position <- function(resolved) {
  if (isTRUE(resolved$fill_grouped)) {
    ggplot2::position_dodge(width = 0.78)
  } else {
    "identity"
  }
}

add_half_violin_layer <- function(p, alpha = 0.72, position = "identity") {
  p + ggplot2::geom_violin(width = 0.78, alpha = alpha, linewidth = 0.32, trim = FALSE, position = position)
}

add_half_violin_mask <- function(p, data, x) {
  groups <- levels(data[[x]])
  if (is.null(groups)) {
    groups <- unique(as.character(data[[x]]))
  }
  mask <- data.frame(
    xmin = seq_along(groups),
    xmax = seq_along(groups) + 0.44,
    ymin = -Inf,
    ymax = Inf
  )
  p + ggplot2::geom_rect(
    data = mask,
    ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
    inherit.aes = FALSE,
    fill = "white",
    color = NA
  )
}

add_point_layer <- function(p, template_id, resolved) {
  point_width <- if (template_id %in% c("beeswarm_summary", "sina_density")) 0.22 else 0.12
  if (isTRUE(resolved$fill_grouped)) {
    return(p + ggplot2::geom_point(
      position = ggplot2::position_jitterdodge(jitter.width = point_width, dodge.width = 0.78),
      alpha = resolved$point_alpha,
      size = resolved$point_size,
      show.legend = FALSE
    ))
  }
  p + ggplot2::geom_jitter(width = point_width, alpha = resolved$point_alpha, size = resolved$point_size, show.legend = FALSE)
}

add_median_segments <- function(p, data, x, y, fill_col, resolved, pal) {
  if (isTRUE(resolved$fill_grouped)) {
    return(p + ggplot2::stat_summary(
      fun = stats::median,
      geom = "crossbar",
      width = 0.18,
      fatten = 0,
      position = ggplot2::position_dodge(width = 0.78),
      color = unname(pal[["line"]]),
      linewidth = 0.55
    ))
  }
  med <- stats::aggregate(data[[y]], list(.x = data[[x]]), stats::median, na.rm = TRUE)
  names(med) <- c(x, ".median")
  med[[x]] <- factor(med[[x]], levels = levels(data[[x]]))
  med$.xnum <- as.numeric(med[[x]])
  p + ggplot2::geom_segment(
    data = med,
    ggplot2::aes(x = .data$.xnum - 0.22, xend = .data$.xnum + 0.22, y = .data$.median, yend = .data$.median),
    inherit.aes = FALSE,
    color = unname(pal[["line"]]),
    linewidth = 0.65
  )
}

add_comparison_annotations <- function(p, data, x, y, comparisons, p_label, pal) {
  groups <- levels(data[[x]])
  if (identical(p_label, "letters")) {
    annotations <- letter_table(data, x, y)
    if (nrow(annotations) == 0L) {
      return(p)
    }
    return(p + ggplot2::geom_text(
      data = annotations,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      color = unname(pal[["line"]]),
      fontface = "bold",
      size = 3.3
    ))
  }
  annotations <- comparison_table(data, x, y, comparisons, p_label)
  if (nrow(annotations) == 0L) {
    return(p)
  }
  p +
    ggplot2::geom_segment(
      data = annotations,
      ggplot2::aes(x = .data$x_start, xend = .data$x_end, y = .data$y, yend = .data$y),
      inherit.aes = FALSE,
      color = unname(pal[["line"]]),
      linewidth = 0.38
    ) +
    ggplot2::geom_segment(
      data = annotations,
      ggplot2::aes(x = .data$x_start, xend = .data$x_start, y = .data$y, yend = .data$y_tip),
      inherit.aes = FALSE,
      color = unname(pal[["line"]]),
      linewidth = 0.38
    ) +
    ggplot2::geom_segment(
      data = annotations,
      ggplot2::aes(x = .data$x_end, xend = .data$x_end, y = .data$y, yend = .data$y_tip),
      inherit.aes = FALSE,
      color = unname(pal[["line"]]),
      linewidth = 0.38
    ) +
    ggplot2::geom_text(
      data = annotations,
      ggplot2::aes(x = .data$x_mid, y = .data$y_label, label = .data$label),
      inherit.aes = FALSE,
      color = unname(pal[["line"]]),
      size = 3.2
    )
}

letter_table <- function(data, x, y, alpha = 0.05) {
  groups <- levels(data[[x]])
  if (is.null(groups)) {
    groups <- unique(as.character(data[[x]]))
  }
  if (length(groups) == 0L) {
    return(data.frame())
  }
  values <- data[[y]]
  yrange <- range(values, na.rm = TRUE)
  span <- diff(yrange)
  if (!is.finite(span) || span == 0) {
    span <- max(abs(yrange), 1)
  }
  labels <- compact_letter_labels(data, x, y, groups, alpha = alpha)
  group_max <- stats::aggregate(values, list(.group = data[[x]]), max, na.rm = TRUE)
  names(group_max) <- c("group", "group_max")
  group_max$group <- as.character(group_max$group)
  data.frame(
    x = match(groups, groups),
    y = group_max$group_max[match(groups, group_max$group)] + span * 0.10,
    label = labels[groups],
    stringsAsFactors = FALSE
  )
}

compact_letter_labels <- function(data, x, y, groups, alpha = 0.05) {
  medians <- vapply(groups, function(group) {
    stats::median(data[data[[x]] == group, y, drop = TRUE], na.rm = TRUE)
  }, numeric(1))
  ordered_groups <- names(sort(medians, decreasing = TRUE))
  sig <- pairwise_significance_matrix(data, x, y, groups, alpha = alpha)
  letters_pool <- c(letters, paste0("a", letters))
  letter_groups <- list()
  labels <- stats::setNames(rep("", length(groups)), groups)

  for (group in ordered_groups) {
    assigned <- FALSE
    if (length(letter_groups) > 0L) {
      for (idx in seq_along(letter_groups)) {
        members <- letter_groups[[idx]]
        can_share <- all(!sig[group, members])
        if (isTRUE(can_share)) {
          letter_groups[[idx]] <- c(members, group)
          labels[[group]] <- paste0(labels[[group]], letters_pool[[idx]])
          assigned <- TRUE
          break
        }
      }
    }
    if (!assigned) {
      letter_groups[[length(letter_groups) + 1L]] <- group
      labels[[group]] <- paste0(labels[[group]], letters_pool[[length(letter_groups)]])
    }
  }
  labels
}

pairwise_significance_matrix <- function(data, x, y, groups, alpha = 0.05) {
  sig <- matrix(FALSE, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
  if (length(groups) < 2L) {
    return(sig)
  }
  pairs <- utils::combn(groups, 2L, simplify = FALSE)
  p_values <- vapply(pairs, function(pair) {
    a <- data[data[[x]] == pair[[1]], y, drop = TRUE]
    b <- data[data[[x]] == pair[[2]], y, drop = TRUE]
    safe_wilcox_p(a, b)
  }, numeric(1))
  p_adj <- stats::p.adjust(p_values, method = "BH")
  for (idx in seq_along(pairs)) {
    pair <- pairs[[idx]]
    is_sig <- is.finite(p_adj[[idx]]) && p_adj[[idx]] < alpha
    sig[pair[[1]], pair[[2]]] <- is_sig
    sig[pair[[2]], pair[[1]]] <- is_sig
  }
  sig
}

comparison_table <- function(data, x, y, comparisons, p_label) {
  groups <- levels(data[[x]])
  if (is.null(groups)) {
    groups <- unique(as.character(data[[x]]))
  }
  yrange <- range(data[[y]], na.rm = TRUE)
  span <- diff(yrange)
  if (!is.finite(span) || span == 0) {
    span <- max(abs(yrange), 1)
  }
  if (is.null(comparisons)) {
    comparisons <- strongest_comparison(data, x, y, groups)
  }
  rows <- list()
  for (idx in seq_along(comparisons)) {
    pair <- comparisons[[idx]]
    if (length(pair) != 2L || !all(pair %in% groups)) {
      next
    }
    a <- data[data[[x]] == pair[[1]], y, drop = TRUE]
    b <- data[data[[x]] == pair[[2]], y, drop = TRUE]
    p_value <- safe_wilcox_p(a, b)
    y_base <- yrange[[2]] + span * (0.10 + idx * 0.10)
    rows[[length(rows) + 1L]] <- data.frame(
      x_start = match(pair[[1]], groups),
      x_end = match(pair[[2]], groups),
      x_mid = mean(match(pair, groups)),
      y = y_base,
      y_tip = y_base - span * 0.035,
      y_label = y_base + span * 0.055,
      label = if (identical(p_label, "p.signif")) p_to_stars(p_value) else format.pval(p_value, digits = 2, eps = 0.001),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

strongest_comparison <- function(data, x, y, groups) {
  if (length(groups) < 2L) {
    return(list())
  }
  pairs <- utils::combn(groups, 2L, simplify = FALSE)
  scores <- lapply(pairs, function(pair) {
    a <- data[data[[x]] == pair[[1]], y, drop = TRUE]
    b <- data[data[[x]] == pair[[2]], y, drop = TRUE]
    list(
      p_value = safe_wilcox_p(a, b),
      effect = abs(stats::median(a, na.rm = TRUE) - stats::median(b, na.rm = TRUE))
    )
  })
  p_values <- vapply(scores, `[[`, numeric(1), "p_value")
  effects <- vapply(scores, `[[`, numeric(1), "effect")
  if (all(is.na(p_values))) {
    return(list(pairs[[which.max(effects)]]))
  }
  order_idx <- order(replace(p_values, is.na(p_values), Inf), -effects)
  list(pairs[[order_idx[[1L]]]])
}

safe_wilcox_p <- function(a, b) {
  out <- tryCatch(stats::wilcox.test(a, b, exact = FALSE)$p.value, error = function(e) NA_real_)
  if (is.na(out)) {
    out <- tryCatch(stats::t.test(a, b)$p.value, error = function(e) NA_real_)
  }
  out
}

p_to_stars <- function(p) {
  if (is.na(p)) return("n.s.")
  if (p <= 0.001) return("***")
  if (p <= 0.01) return("**")
  if (p <= 0.05) return("*")
  "n.s."
}

print_violin_params <- function(params) {
  msg <- paste(
    paste0(names(params), "=", vapply(params, function(x) paste(x, collapse = ","), character(1))),
    collapse = ", "
  )
  message("violinplus parameters: ", msg)
}

theme_violinplus <- function(base_size = 10.5, base_family = "") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "#27313D", linewidth = 0.35),
      axis.ticks = ggplot2::element_line(color = "#27313D", linewidth = 0.32),
      axis.text = ggplot2::element_text(color = "#475467"),
      axis.title = ggplot2::element_text(color = "#202A36"),
      plot.title = ggplot2::element_text(color = "#17202B", face = "bold", size = base_size * 1.12),
      plot.subtitle = ggplot2::element_text(color = "#667085", margin = ggplot2::margin(t = 3, b = 7)),
      plot.caption = ggplot2::element_text(color = "#667085"),
      plot.margin = ggplot2::margin(12, 30, 12, 22),
      legend.position = "none",
      strip.background = ggplot2::element_rect(fill = "#F5F7FA", color = "#D5DAE3", linewidth = 0.32),
      strip.text = ggplot2::element_text(color = "#27313D", face = "bold")
  )
}

theme_violinplus_facets <- function(enabled = FALSE) {
  if (!isTRUE(enabled)) {
    return(ggplot2::theme())
  }
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, color = "#27313D", linewidth = 0.35),
    strip.background = ggplot2::element_rect(fill = "#F5F7FA", color = "#27313D", linewidth = 0.35)
  )
}

theme_violinplus_legend <- function(resolved) {
  if (identical(resolved$legend_position, "none")) {
    return(ggplot2::theme(legend.position = "none"))
  }
  horizontal <- resolved$legend_position %in% c("bottom", "top")
  ggplot2::theme(
    legend.position = resolved$legend_position,
    legend.direction = if (horizontal) "horizontal" else "vertical",
    legend.justification = if (horizontal) "center" else "top",
    legend.title = ggplot2::element_text(color = "#27313D", size = resolved$base_size * 0.86),
    legend.text = ggplot2::element_text(color = "#475467", size = resolved$base_size * 0.82),
    legend.key.height = grid::unit(0.28, "cm"),
    legend.key.width = grid::unit(0.42, "cm"),
    legend.spacing.x = grid::unit(0.14, "cm"),
    legend.box.spacing = grid::unit(0.08, "cm"),
    legend.margin = ggplot2::margin(t = 2, r = 0, b = 0, l = 0)
  )
}

resolve_legend_position <- function(legend_position, fill_grouped) {
  if (is.null(legend_position)) {
    return(if (isTRUE(fill_grouped)) "bottom" else "none")
  }
  legend_position <- check_scalar_string(legend_position, "legend_position")
  choices <- c("none", "bottom", "right", "left", "top")
  match.arg(legend_position, choices)
}

resolve_orientation <- function(orientation) {
  choices <- c("vertical", "horizontal")
  if (is.character(orientation) && length(orientation) > 1L) {
    orientation <- match.arg(orientation, choices)
  }
  orientation <- check_scalar_string(orientation, "orientation")
  if (!orientation %in% choices) {
    stop("`orientation` must be one of: ", paste(choices, collapse = ", "), ".", call. = FALSE)
  }
  orientation
}

legend_guide <- function(resolved, pal) {
  args <- list(
    byrow = TRUE,
    override.aes = list(alpha = 0.85, color = unname(pal[["line"]]))
  )
  if (resolved$legend_position %in% c("bottom", "top")) {
    args$nrow <- 1
  } else {
    args$ncol <- 1
  }
  do.call(ggplot2::guide_legend, args)
}

plot_coordinate <- function(resolved) {
  if (identical(resolved$orientation, "horizontal")) {
    return(ggplot2::coord_flip(clip = "off"))
  }
  ggplot2::coord_cartesian(clip = "off")
}

#' Generate deterministic demo data for violinplus templates
#'
#' @param seed Random seed.
#'
#' @return A data frame with sample, subject, group, value, facet, cohort, response, batch, timepoint, and quality columns.
#' @export
violinplus_demo_data <- function(seed = 42) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  groups <- c("Control", "Low dose", "High dose")
  facets <- c("IL6", "CRP", "TNF-alpha", "Albumin")
  feature_ids <- c("cytokine_il6", "acute_phase_crp", "cytokine_tnf", "serum_albumin")
  cohorts <- c("Discovery", "Validation")
  responses <- c("Responder", "Stable", "Progressor")
  batches <- c("Batch 1", "Batch 2")
  timepoints <- c("Baseline", "Week 8")
  units <- c("pg/mL", "mg/L", "pg/mL", "g/L")
  n_subject <- 12L

  rows <- list()
  idx <- 1L
  subject_index <- 1L
  for (cohort in cohorts) {
    for (group_idx in seq_along(groups)) {
      group <- groups[[group_idx]]
      for (subject_i in seq_len(n_subject)) {
        subject <- sprintf("S%03d", subject_index)
        response <- responses[((subject_i + group_idx + ifelse(cohort == "Validation", 1L, 0L)) %% length(responses)) + 1L]
        batch <- batches[((subject_i + group_idx) %% length(batches)) + 1L]
        subject_effect <- stats::rnorm(1L, 0, 0.18)
        for (timepoint in timepoints) {
          for (facet_idx in seq_along(facets)) {
            facet <- facets[[facet_idx]]
            baseline <- c(2.15, 3.3, 2.65, 4.1)[[facet_idx]]
            group_effect <- c(0, 0.34, 0.72)[[group_idx]]
            time_effect <- if (timepoint == "Week 8") c(-0.05, 0.22, 0.12, -0.18)[[facet_idx]] else 0
            response_effect <- c(Responder = -0.18, Stable = 0, Progressor = 0.24)[[response]]
            response_time_effect <- 0
            if (timepoint == "Week 8") {
              response_time_effect <- switch(
                response,
                Responder = c(-0.62, -0.50, -0.42, 0.34)[[facet_idx]],
                Stable = c(-0.04, 0.02, 0.00, 0.02)[[facet_idx]],
                Progressor = c(0.50, 0.42, 0.34, -0.24)[[facet_idx]]
              )
            }
            cohort_effect <- if (cohort == "Validation") c(0.08, -0.05, 0.04, 0.03)[[facet_idx]] else 0
            batch_effect <- if (batch == "Batch 2") c(0.04, 0.08, -0.04, 0.02)[[facet_idx]] else 0
            value <- stats::rnorm(
              1L,
              mean = baseline + group_effect + time_effect + response_effect + response_time_effect + cohort_effect + batch_effect + subject_effect,
              sd = c(0.34, 0.45, 0.38, 0.28)[[facet_idx]]
            )
            is_outlier <- subject_i %in% c(3L, 10L) && group == "High dose" && timepoint == "Week 8" && facet %in% c("IL6", "CRP")
            if (is_outlier) {
              value <- value + if (facet == "IL6") 1.35 else 1.1
            }
            is_missing <- subject_i == 5L && cohort == "Validation" && timepoint == "Week 8" && facet %in% c("TNF-alpha", "Albumin")
            rows[[idx]] <- data.frame(
              sample_id = sprintf("%s_%s_%s_%s", subject, gsub(" ", "", timepoint), gsub("[^A-Za-z0-9]", "", facet), gsub(" ", "", group)),
              subject = subject,
              group = group,
              value = if (is_missing) NA_real_ else value,
              facet = facet,
              pair = timepoint,
              cohort = cohort,
              response = response,
              batch = batch,
              timepoint = timepoint,
              unit = units[[facet_idx]],
              feature_id = feature_ids[[facet_idx]],
              is_outlier = is_outlier,
              stringsAsFactors = FALSE
            )
            idx <- idx + 1L
          }
        }
        subject_index <- subject_index + 1L
      }
    }
  }
  do.call(rbind, rows)
}

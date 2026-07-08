test_that("violinplus_demo_data provides platform-ready columns", {
  data <- violinplus_demo_data()

  expect_s3_class(data, "data.frame")
  expect_true(all(c(
    "sample_id", "subject", "group", "value", "facet", "pair",
    "cohort", "response", "batch", "timepoint", "unit", "feature_id", "is_outlier"
  ) %in% names(data)))
  expect_gt(nrow(data), 250)
  expect_true(is.numeric(data$value))
  expect_gte(length(unique(data$facet)), 4)
  expect_gte(length(unique(data$cohort)), 2)
  expect_gte(length(unique(data$response)), 3)
  expect_gte(sum(data$is_outlier, na.rm = TRUE), 3)
  expect_gt(sum(is.na(data$value)), 0)
  expect_true(any(table(data$subject, data$pair) > 1))
})

test_that("demo data contains responder and progressor longitudinal signals", {
  data <- violinplus_demo_data()
  complete <- data[!is.na(data$value), , drop = FALSE]

  il6 <- complete[complete$facet == "IL6", , drop = FALSE]
  responder_baseline <- stats::median(il6$value[il6$response == "Responder" & il6$pair == "Baseline"])
  responder_week8 <- stats::median(il6$value[il6$response == "Responder" & il6$pair == "Week 8"])
  progressor_baseline <- stats::median(il6$value[il6$response == "Progressor" & il6$pair == "Baseline"])
  progressor_week8 <- stats::median(il6$value[il6$response == "Progressor" & il6$pair == "Week 8"])

  expect_lt(responder_week8, responder_baseline)
  expect_gt(progressor_week8, progressor_baseline)
})

test_that("one demo table can drive every template", {
  data <- violinplus_demo_data()
  example_map <- violin_template_examples()

  for (idx in seq_len(nrow(example_map))) {
    template_id <- example_map$template[[idx]]
    template_data <- data[example_map$filter[[idx]](data), , drop = FALSE]
    expect_gt(nrow(template_data), 20)
    p <- violin_plot(
      template_data,
      x = example_map$x[[idx]],
      y = example_map$y[[idx]],
      facet = if (nzchar(example_map$facet[[idx]])) example_map$facet[[idx]] else NULL,
      subject = if (nzchar(example_map$subject[[idx]])) example_map$subject[[idx]] else NULL,
      template = template_id,
      print_params = FALSE
    )
    expect_s3_class(p, "ggplot")
    expect_equal(attr(p, "violinplus_params")$template, template_id)
  }
})

test_that("save_violin uses attached auto dimensions", {
  data <- violinplus_demo_data()
  p <- violin_plot(data, x = "group", y = "value", template = "violin_box", print_params = FALSE)
  out <- tempfile(fileext = ".png")

  result <- save_violin(p, out, width = "auto", height = "auto", dpi = 96)

  expect_true(file.exists(out))
  expect_equal(result, normalizePath(out, mustWork = FALSE))
})

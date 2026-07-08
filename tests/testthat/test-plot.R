test_that("violin_plot returns ggplot objects for every template", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    value = c(rnorm(10, 2), rnorm(10, 3), rnorm(10, 4)),
    facet = rep(c("M1", "M2"), 15),
    subject = rep(seq_len(10), 3)
  )

  for (id in violin_templates()$id) {
    p <- violin_plot(
      data,
      x = "group",
      y = "value",
      facet = if (id == "facet_grid") "facet" else NULL,
      subject = if (id == "paired_change") "subject" else NULL,
      template = id,
      print_params = FALSE
    )
    expect_s3_class(p, "ggplot")
    expect_equal(attr(p, "violinplus_params")$template, id)
  }
})

test_that("violin_plot prints resolved parameters", {
  data <- data.frame(group = rep(c("A", "B"), each = 8), value = rnorm(16))

  expect_message(
    p <- violin_plot(data, x = "group", y = "value", template = "two_group_sig", print_params = TRUE),
    "violinplus parameters:"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "violinplus_params")$comparison_method, "wilcox.test")
})

test_that("violin_plot supports manual palette and comparison label controls", {
  data <- data.frame(group = rep(c("A", "B"), each = 8), value = c(rnorm(8, 1), rnorm(8, 3)))

  p <- violin_plot(
    data,
    x = "group",
    y = "value",
    template = "two_group_sig",
    palette = "jama_cyan_red",
    comparisons = list(c("A", "B")),
    p_label = "p.signif",
    print_params = FALSE
  )

  params <- attr(p, "violinplus_params")
  expect_equal(params$palette, "jama_cyan_red")
  expect_equal(params$p_label, "p.signif")
  expect_true(params$compare)
})

test_that("multi-group comparison automatically selects the strongest pair", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 12),
    value = c(rep(1, 12), rep(1.1, 12), rep(4, 12))
  )

  annotations <- violinplus:::comparison_table(data, "group", "value", comparisons = NULL, p_label = "p.signif")

  expect_equal(nrow(annotations), 1)
  expect_equal(annotations$x_start, 1)
  expect_equal(annotations$x_end, 3)
  expect_equal(annotations$label, "***")

  p <- violin_plot(data, x = "group", y = "value", template = "multi_group_sig", print_params = FALSE)
  built <- ggplot2::ggplot_build(p)
  text_layers <- Filter(function(layer) "label" %in% names(layer), built$data)
  expect_true(any(vapply(text_layers, function(layer) any(layer$label == "***"), logical(1))))
})

test_that("letter templates compute compact group labels", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 16),
    value = c(rnorm(16, 1, 0.08), rnorm(16, 2, 0.08), rnorm(16, 3, 0.08))
  )

  letters <- violinplus:::letter_table(data, "group", "value")

  expect_equal(nrow(letters), 3)
  expect_setequal(letters$label, c("a", "b", "c"))

  p <- violin_plot(data, x = "group", y = "value", template = "violin_box_letter", print_params = FALSE)
  built <- ggplot2::ggplot_build(p)
  text_layers <- Filter(function(layer) "label" %in% names(layer), built$data)
  expect_true(any(vapply(text_layers, function(layer) any(layer$label %in% letters$label), logical(1))))
})

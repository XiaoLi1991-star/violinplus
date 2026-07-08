test_that("inspect_violin_plot recommends dimensions and visual density defaults", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 8),
    value = seq_len(24),
    facet = rep(c("M1", "M2"), 12)
  )

  inspection <- inspect_violin_plot(data, x = "group", y = "value", facet = "facet", template = "facet_grid")

  expect_s3_class(inspection, "violinplus_inspection")
  expect_equal(inspection$metrics$n_groups, 3L)
  expect_equal(inspection$metrics$n_facets, 2L)
  expect_equal(inspection$resolved_params$width, 5)
  expect_equal(inspection$resolved_params$height, 4.4)
  expect_equal(inspection$resolved_params$facet_cols, 2L)
  expect_false(inspection$resolved_params$show_points)
})

test_that("inspect_violin_plot chooses comparison methods from group count", {
  two_group <- data.frame(group = rep(c("Control", "Treatment"), each = 6), value = rnorm(12))
  multi_group <- data.frame(group = rep(c("A", "B", "C", "D"), each = 5), value = rnorm(20))

  two <- inspect_violin_plot(two_group, x = "group", y = "value", template = "two_group_sig")
  multi <- inspect_violin_plot(multi_group, x = "group", y = "value", template = "multi_group_sig")

  expect_equal(two$resolved_params$comparison_method, "wilcox.test")
  expect_equal(two$resolved_params$p_adjust_method, "none")
  expect_equal(multi$resolved_params$comparison_method, "kruskal.test")
  expect_equal(multi$resolved_params$p_adjust_method, "BH")
})

test_that("inspect_violin_plot keeps raw points for small samples", {
  data <- data.frame(group = rep(c("A", "B"), each = 5), value = rnorm(10))

  inspection <- inspect_violin_plot(data, x = "group", y = "value", template = "violin_box")

  expect_true(inspection$resolved_params$show_points)
  expect_equal(inspection$resolved_params$point_size, 1.7)
  expect_lte(inspection$resolved_params$width, 4.5)
  expect_lte(inspection$resolved_params$height, 3.4)
  expect_true(inspection$risks$small_sample)
})

test_that("inspect_violin_plot keeps four-facet supplementary figures compact", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 40),
    value = rnorm(120),
    facet = rep(c("M1", "M2", "M3", "M4"), each = 30)
  )

  inspection <- inspect_violin_plot(data, x = "group", y = "value", facet = "facet", template = "facet_grid")

  expect_equal(inspection$metrics$n_facets, 4L)
  expect_equal(inspection$resolved_params$facet_cols, 2L)
  expect_lte(inspection$resolved_params$width, 5.8)
  expect_lte(inspection$resolved_params$height, 5.4)
})

test_that("violin_templates exposes stable public template metadata", {
  templates <- violin_templates()

  expected <- c(
    "violin_box",
    "violin_jitter",
    "box_jitter",
    "violin_only",
    "raincloud",
    "half_violin_box",
    "beeswarm_summary",
    "sina_density",
    "two_group_sig",
    "multi_group_sig",
    "violin_box_letter",
    "split_violin_letter",
    "paired_change",
    "facet_grid"
  )

  expect_s3_class(templates, "data.frame")
  expect_identical(templates$id, expected)
  expect_true(all(nzchar(templates$title)))
  expect_true(all(nzchar(templates$description)))
  expect_true(all(nzchar(templates$palette)))
  expect_true(all(nzchar(templates$thumbnail)))
  expect_true(all(vapply(templates$default_params, is.list, logical(1))))
  expect_equal(length(unique(templates$palette)), length(expected))
})

test_that("violin_palettes returns named colors for every template palette", {
  templates <- violin_templates()
  palettes <- violin_palettes()

  expect_s3_class(palettes, "data.frame")
  expect_setequal(palettes$name, templates$palette)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palettes$fill)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palettes$accent)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palettes$point)))
  expect_true(all(nzchar(palettes$source)))
  expect_true(any(grepl("NPG|Lancet|NEJM|JAMA|JCO|AAAS", palettes$source)))
  expect_false(any(palettes$fill %in% c("#5B6472", "#6E88A8", "#87964F")))
})

test_that("template resolution accepts numeric ids and names", {
  expect_equal(resolve_violin_template("violin_box")$id, "violin_box")
  expect_equal(resolve_violin_template(1)$id, "violin_box")
  expect_equal(resolve_violin_template(14)$id, "facet_grid")
  expect_error(resolve_violin_template("missing_template"), "Unknown template")
})

test_that("group colors use direct journal palette colors before interpolation", {
  groups <- factor(c("Control", "Low dose", "High dose"), levels = c("Control", "Low dose", "High dose"))
  pal <- violinplus:::resolve_violin_palette("npg_red_blue")
  colors <- violinplus:::template_fill_values(groups, pal)

  expect_identical(unname(colors), unname(pal[c("fill", "accent", "point")]))
})

test_that("template examples define one-table scientific stories", {
  examples <- violin_template_examples()
  templates <- violin_templates()

  expect_s3_class(examples, "data.frame")
  expect_identical(examples$template, templates$id)
  expect_true(all(nzchar(examples$claim)))
  expect_true(all(nzchar(examples$filter_label)))
  expect_true(all(examples$x %in% c("group", "response", "pair", "cohort")))
  expect_true(all(examples$y == "value"))
  expect_true(all(vapply(examples$filter, is.function, logical(1))))
  expect_true(any(examples$x == "response"))
  expect_true(any(examples$x == "pair"))
  expect_true(any(nzchar(examples$facet)))
  expect_true(any(grepl("letter", examples$template)))
})

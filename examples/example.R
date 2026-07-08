suppressPackageStartupMessages(library(violinplus))

dir.create("examples", showWarnings = FALSE)
demo_file <- file.path("examples", "violinplus-demo-data.csv")
utils::write.csv(violinplus_demo_data(), demo_file, row.names = FALSE)
data <- utils::read.csv(demo_file, stringsAsFactors = FALSE)

example_map <- violin_template_examples()
for (idx in seq_len(nrow(example_map))) {
  template_id <- example_map$template[[idx]]
  template_data <- data[example_map$filter[[idx]](data), , drop = FALSE]
  plot <- violin_plot(
    template_data,
    x = example_map$x[[idx]],
    y = example_map$y[[idx]],
    fill_col = if (nzchar(example_map$fill_col[[idx]])) example_map$fill_col[[idx]] else NULL,
    facet = if (nzchar(example_map$facet[[idx]])) example_map$facet[[idx]] else NULL,
    subject = if (nzchar(example_map$subject[[idx]])) example_map$subject[[idx]] else NULL,
    template = template_id,
    title = example_map$title[[idx]],
    ylab = example_map$ylab[[idx]],
    print_params = TRUE
  )
  save_violin(plot, file.path("examples", paste0("violinplus-", template_id, ".png")), dpi = 180)
}

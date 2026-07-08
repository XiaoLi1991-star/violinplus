\name{violin_plot}
\alias{violin_plot}
\title{Draw a violinplus distribution plot}
\usage{
violin_plot(
  data,
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
  width = "auto",
  height = "auto",
  print_params = TRUE,
  ...
)
}
\arguments{
\item{data}{A data frame.}
\item{x, y}{Column names for group and numeric value.}
\item{fill_col}{Optional column used for fill and color grouping. Defaults to \code{x}.}
\item{template}{Template ID from \code{\link[=violin_templates]{violin_templates()}}.}
\item{facet}{Optional facet column.}
\item{subject}{Optional subject ID column for paired templates.}
\item{palette}{Palette name from \code{\link[=violin_palettes]{violin_palettes()}}. Defaults to the template palette.}
\item{comparisons}{Optional list of length-two character vectors naming groups to compare.}
\item{p_label}{Use \code{"p.signif"} for stars, \code{"p.format"} for formatted p values, or \code{"letters"} for compact group letters.}
\item{title, subtitle, caption}{Plot text.}
\item{xlab, ylab}{Axis labels.}
\item{show_points, show_box}{Optional layer overrides. Defaults come from template and inspection.}
\item{width, height}{Output dimensions or \code{"auto"} for attached metadata.}
\item{print_params}{Print resolved parameters for reproducible platform tuning.}
\item{...}{Reserved for future template controls.}
}
\value{
A ggplot object with \code{violinplus_params} metadata.
}
\description{
Draw a violinplus distribution plot.
}

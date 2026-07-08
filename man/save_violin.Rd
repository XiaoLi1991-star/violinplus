\name{save_violin}
\alias{save_violin}
\title{Save a violinplus plot}
\usage{
save_violin(plot, filename, width = "auto", height = "auto", dpi = 320, bg = "white", ...)
}
\arguments{
\item{plot}{A plot returned by \code{\link[=violin_plot]{violin_plot()}}.}
\item{filename}{Output file path.}
\item{width, height}{Plot size in inches. Use \code{"auto"} for attached recommendations.}
\item{dpi}{Raster resolution.}
\item{bg}{Background color.}
\item{...}{Additional arguments passed to \code{ggplot2::ggsave()}.}
}
\value{
The normalized output filename, invisibly.
}
\description{
Save a violinplus plot.
}

\name{inspect_violin_plot}
\alias{inspect_violin_plot}
\title{Inspect data and recommend violinplus plot defaults}
\usage{
inspect_violin_plot(data, x, y, facet = NULL, subject = NULL, template = "violin_box")
}
\arguments{
\item{data}{A data frame.}
\item{x, y}{Column names for group and numeric value.}
\item{facet}{Optional facet column.}
\item{subject}{Optional subject ID column for paired templates.}
\item{template}{Template ID passed to \code{\link[=resolve_violin_template]{resolve_violin_template()}}.}
}
\value{
A \code{violinplus_inspection} list with metrics, resolved parameters, and risk flags.
}
\description{
Inspect data and recommend violinplus plot defaults.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bar.R
\name{plot_bar}
\alias{plot_bar}
\title{Plot - bar}
\usage{
plot_bar(
  data,
  y,
  theme,
  bar_width,
  code,
  fill,
  outline,
  position,
  labels,
  row,
  prop,
  column,
  title,
  subtitle,
  xlab,
  ylab,
  caption
)
}
\arguments{
\item{data}{data}

\item{y}{variable on y axis}

\item{bar_width}{width of bar}

\item{fill}{fill}

\item{outline}{outline}

\item{position}{position of bars}

\item{labels}{data labels}

\item{row}{facet row in grid}

\item{column}{facet column in grid}

\item{title}{title of plot}

\item{subtitle}{subtitle of plot}

\item{xlab}{x-axis}

\item{ylab}{y-axis label}

\item{caption}{caption}
}
\value{
No return value. This function is called for the side effect of
launching a shiny application.
}
\description{
Plot - bar
}
\examples{
if (interactive()) {
  plot_bar(mtcars)
}
}

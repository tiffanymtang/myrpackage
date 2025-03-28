% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot-color-themes.R
\name{scale_color_custom}
\alias{scale_color_custom}
\alias{scale_colour_custom}
\alias{scale_fill_custom}
\title{Custom color scales}
\usage{
scale_colour_custom(
  discrete = FALSE,
  palette = "viridis",
  viridis_option = "plasma",
  ...
)

scale_color_custom(
  discrete = FALSE,
  palette = "viridis",
  viridis_option = "plasma",
  ...
)

scale_fill_custom(
  discrete = FALSE,
  palette = "viridis",
  viridis_option = "plasma",
  ...
)
}
\arguments{
\item{discrete}{Logical. If \code{TRUE}, use a discrete color scale. If
\code{FALSE}, use a continuous color scale.}

\item{palette}{Character. The name of the color palette to use. Options are
"viridis", "high_contrast", and "redblue".}

\item{viridis_option}{Character. The viridis color palette option to use.
Only used if \code{palette == "viridis"}. Default is "plasma".
See \code{\link[viridis]{viridis}} for more information.}

\item{...}{Additional arguments to pass to \code{ggplot2::discrete_scale()}
if \code{discrete == TRUE} and to \code{viridis::scale_color_viridis()} or
\code{viridis::scale_fill_viridis()} if \code{discrete == FALSE}.}
}
\description{
Custom scale functions (fill and colour/color) for \code{ggplot2}.
For \code{discrete == FALSE}, the custom scale uses the \code{viridis} color
palette. For \code{discrete == TRUE}, custom color palettes are used.
}
\examples{
library(ggplot2)
ggplot(iris) +
  aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length) +
  geom_point() +
  scale_color_custom(discrete = FALSE)
ggplot(iris) +
  aes(x = Sepal.Length, fill = Species) +
  geom_density() +
  scale_fill_custom(discrete = TRUE)

}

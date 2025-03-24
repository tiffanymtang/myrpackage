#' Custom color palettes for `ggplot2`
#'
#' @description Generates various customized color palettes for `ggplot2`
#'
#' @param palette Character. The name of the color palette to use. Options are
#'   "viridis", "high_contrast", and "redblue".
#' @param viridis_option Character. The viridis color palette option to use.
#'   Only used if \code{palette == "viridis"}. Default is "plasma".
#'   See \code{\link[viridis]{viridis}} for more information.
#'
#' @returns A function that generates a color palette with the specified number
#'   of colors.
#'
#' @examples
#' library(scales)
#' show_col(custom_palette(palette = "high_contrast")(8))
#' show_col(custom_palette(palette = "redblue")(5))
#' show_col(custom_palette(palette = "viridis")(8))
#'
#' @export
custom_palette <- function(palette = c("viridis", "high_contrast", "redblue"),
                           viridis_option = "plasma") {
  # check that palette is a valid argument
  palette <- match.arg(palette)

  f <- function(n) {
    # get max number of colors for each palette
    max_n <- dplyr::case_when(
      palette == "high_contrast" ~ 8,
      palette == "redblue" ~ Inf,
      palette == "viridis" ~ Inf
    )
    if ((n > max_n) || (palette == "viridis")) {
      if (n > max_n) {
        warning(
          "Number of levels > number of colors in requested palette. ",
          "Using viridis color palette instead."
        )
      }
      pal <- viridisLite::viridis(n, option = viridis_option)
    } else if (palette == "high_contrast") {
      pal <- c("#FF9300", "#1B9E77", "#7570B3", "#E7298A",
               "#66A61E", "#E6AB02", "#A6761D", "#666666")[1:n]
    } else if (palette == "redblue") {
      pal <- colorRampPalette(
        c("#319CD7",  "#A8CDEB", "#F4F4F4", "#FFB5B0", "#F76063")
      )(n)
    }
    return(pal)
  }
  return(f)
}


#' Custom color scales
#'
#' @description Custom scale functions (fill and colour/color) for `ggplot2`.
#'   For \code{discrete == FALSE}, the custom scale uses the `viridis` color
#'   palette. For \code{discrete == TRUE}, custom color palettes are used.
#'
#' @inheritParams custom_palette
#' @param discrete Logical. If \code{TRUE}, use a discrete color scale. If
#'   \code{FALSE}, use a continuous color scale.
#' @param ... Additional arguments to pass to \code{ggplot2::discrete_scale()}
#'   if \code{discrete == TRUE} and to \code{viridis::scale_color_viridis()} or
#'   \code{viridis::scale_fill_viridis()} if \code{discrete == FALSE}.
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length) +
#'   geom_point() +
#'   scale_color_custom(discrete = FALSE)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   scale_fill_custom(discrete = TRUE)
#'
#' @name scale_color_custom
#' @rdname scale_color_custom
#'
NULL


#' @rdname scale_color_custom
#'
#' @export
scale_colour_custom <- function(discrete = FALSE,
                                palette = "viridis",
                                viridis_option = "plasma", ...) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      palette = custom_palette(
        palette = palette,
        viridis_option = viridis_option
      ),
      ...
    )
  } else {
    viridis::scale_color_viridis(
      discrete = FALSE,
      option = viridis_option,
      ...
    )
  }
}


#' @rdname scale_color_custom
#'
#' @export
scale_color_custom <- scale_colour_custom


#' @rdname scale_color_custom
#'
#' @export
scale_fill_custom <- function(discrete = FALSE,
                              palette = "viridis",
                              viridis_option = "plasma", ...) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      palette = custom_palette(
        palette = palette,
        viridis_option = viridis_option
      ),
      ...
    )
  } else {
    viridis::scale_fill_viridis(
      discrete = FALSE,
      option = viridis_option,
      ...
    )
  }
}

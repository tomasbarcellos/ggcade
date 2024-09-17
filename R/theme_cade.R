## Template v2.3 for themes in `theme_park` https://github.com/MatthewBJane/theme_park   <<<  Remove this line

## YOUR NAME
## theme_cade
## Date 14/09/2024


# COLOR: add, remove, or edit the colors to fit your scheme (hex code preferred, but you can use any type). Names should be
#' cade Inspired Theme Color Palette
#'
#' @format character vector of hex code strings
#' @export
#' @concept cade
#'
#' @examples
#' cade_theme_colors
#'
cade_theme_colors <- c(
  lighter    = '#8ac706',
  light      = '#5f2972',
  medium     = '#ffde00',
  dark       = '#003466'
)

# DISCRETE PALETTE COLORS: Please update to some colors that work with your theme
#' Cade Inspired Color Palette
#'
#' @format character vector of hex code strings
#' @export
#' @concept cade
#'
#' @examples
#' cade_palette
#'
cade_palette <- c(
  '#003466', '#ffde00', '#5f2972', '#8ac706', '#b19883','#3f3f3f'
)

boletim_palette <- c(
  "#31486e", "#546c84", "#897767", "#92a5b8", "#2082cd", "#b0786c", "#dcd8d5"
)

#' cade Inspired Theme
#'
#' @param cade_font should `theme_cade` use custom font? Default is `TRUE`.
#' @param ... additional parameters to pass to `ggplot2::theme()`
#'
#' @return a `ggplot2` `theme` element
#' @export
#' @concept cade
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = data.frame(x = rnorm(50, 0, 1), y = rnorm(50,0,1)), aes(x = x, y = y)) +
#'   geom_smooth(method = 'lm') +
#'   geom_point() +
#'   labs(title = 'cade Scatter Plot') +
#'   theme_cade()
#'
#' ggplot(mpg, aes(cty)) +
#' geom_density(aes(fill=factor(cyl)), alpha=0.8) +
#'   labs(title="Density plot",
#'        subtitle="City Mileage Grouped by Number of cylinders",
#'        caption="Source: mpg",
#'        x="City Mileage",
#'        fill="# Cylinders") +
#'   theme_cade() +
#'   scale_fill_cade_d()
#'
#'
theme_cade <- function(cade_font = FALSE, ...) {

  # CUSTOM FONT: add a custom font from google fonts
  font_family <- ifelse(cade_font, 'cade', 'sans') # use this line if you have a custom font - change cade to match the font name used
  if (cade_font) {
    initialize_font(name = "GOOGLE FONT NAME", family = "cade")
  }

  # CUSTOM THEME:
  ggplot2::theme_classic() +
    ggplot2::theme(
      ...,
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(linetype = 4),
    axis.line = ggplot2::element_line(
      arrow = grid::arrow(type = "open",
                          length = ggplot2::unit(0.15, "inches"))
    ),
    legend.position = "bottom"
    )
}

#' cade Inspired Color Scales
#'
#' @param ... Additional arguments to pass to `ggplot2::binned_scale` for `_b`,
#' `ggplot2::scale_[fill/color]_gradient` for `_c`, or `ggplot2::discrete_scale`
#' for `_d`
#'
#' @return a `ggplot` scale object
#'
#' @rdname scale_cade
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(cty)) +
#' geom_density(aes(fill=factor(cyl)), alpha=0.8) +
#'   labs(title="Density plot",
#'        subtitle="City Mileage Grouped by Number of cylinders",
#'        caption="Source: mpg",
#'        x="City Mileage",
#'        fill="# Cylinders") +
#'   facet_wrap(~(hwy > 29)) +
#'   theme_cade() +
#'   scale_fill_cade_d()
#'
scale_color_cade_c <- function(...) {
  ggplot2::scale_color_gradient(..., low = cade_theme_colors["lighter"], high = cade_theme_colors["dark"])
}

#' @rdname scale_cade
#' @export
scale_fill_cade_c <- function(...) {
  ggplot2::scale_fill_gradient(..., low = cade_theme_colors["lighter"], high = cade_theme_colors["dark"])
}

#' @rdname scale_cade
#' @export
scale_color_cade_b <- function(...) {
  if (!requireNamespace('scales', quietly = TRUE)) {
    stop('This function requires the `scales` R package.')
  }
  ramp <- scales::colour_ramp(c(cade_theme_colors["lighter"], cade_theme_colors["dark"]))
  ggplot2::binned_scale('color', 'cade', palette = ramp, ...)
}

#' @rdname scale_cade
#' @export
scale_fill_cade_b <- function(...) {
  if (!requireNamespace('scales', quietly = TRUE)) {
    stop('This function requires the `scales` R package.')
  }
  ramp <- scales::colour_ramp(c(cade_theme_colors["lighter"], cade_theme_colors["dark"]))
  ggplot2::binned_scale('fill', 'cade', palette = ramp, ...)
}
#' @rdname scale_cade
#' @export
scale_color_cade_d <- function(...) {
  ggplot2::discrete_scale(aesthetics = 'color',
                          palette = rot_pal(cade_palette), ...)
}

#' Cria paleta
#'
#' @param pal paleta
#'
#' @return paleta
rot_pal <- function(pal) {
  pal <- unname(pal)
  function(n) {
    if (n <= length(pal)) {
      pal[seq_len(n)]
    } else {
      rep(pal, ceiling(n / length(pal)))[seq_len(n)]
    }
  }
}

#' @rdname scale_cade
#' @export
scale_fill_cade_d <- function(...) {
  ggplot2::discrete_scale(aesthetics = 'fill', ...,
                          palette = rot_pal(cade_palette))
}

#' @rdname scale_cade
#' @export
scale_colour_cade_d <- scale_color_cade_d

#' @rdname scale_cade
#' @export
scale_colour_cade_c <- scale_color_cade_c

#' @rdname scale_cade
#' @export
scale_colour_cade_b <- scale_color_cade_b

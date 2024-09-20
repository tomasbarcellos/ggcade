#' @title Tema do Cade
#'
#' @description Clean ggplot theme with no panel background, black axis lines
#'   and grey fill colour for chart elements.
#'
#' @author Tomas Barcellos \email{tomas.barcellos@@cade.gov.br}
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @family themes
#' @export
#'
#' @example inst/examples/ex-theme_clean.R
theme_cade <- function(base_size = 12,
                       base_family = "sans") {
  (
    ggthemes::theme_foundation(
      base_size = base_size,
      base_family = base_family
    ) + theme(
      axis.line.x = element_line(
        colour = "black", size = 0.5, linetype = "solid"
      ),
      axis.text = element_text(size = ceiling(base_size * 0.7), colour = "black"),
      axis.title = element_text(size = ceiling(base_size * 0.8)),

      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linetype = 3),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),

      strip.background = element_rect(linetype = 0),
      strip.text = element_text(),
      strip.text.x = element_text(vjust = 0.5),
      strip.text.y = element_text(angle = -90),

      legend.position = "top",
      legend.justification = "center",
      legend.spacing = unit(base_size * 1.5, "points"),
      legend.key = element_rect(linetype = 0),
      legend.key.size = unit(1.3, "lines"),

      plot.background = element_rect(colour = "transparent"),
      plot.title = element_text(size = ceiling(base_size * 1.1), face = "bold"),
      plot.subtitle = element_text(size = ceiling(base_size * 1.05)),
      plot.caption = element_text(hjust = -0.1)
    )
  )
}

#' @rdname theme_cade
#' @export
theme_cade_flip <- function(base_size = 12,
                       base_family = "sans") {
  (
    theme_cade() +
      theme(
      panel.grid.major.x = element_line(colour = "gray", linetype = 3),
      panel.grid.major.y = element_blank()
    )
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

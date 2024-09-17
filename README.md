
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggcade

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

O objetivo do ggcade é implantar a identidade visual do Cade em
gráficos, mapas e etc.

## Instalação

Para instalar o pacote ….

``` r
# ainda nao e possivel
```

## Exemplo

Digamos que temos o seguinte gráfico:

``` r
library(ggplot2)
library(ggcade)

p1 <- ggplot(mtcars, aes(wt, mpg, col = factor(cyl))) + 
  geom_point(size = 5) 
p1
```

<img src="man/figures/README-example-1.png" width="100%" />

Para adicionar o tema do Cade basta

``` r
p1 + 
  theme_cade()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Para adicionar a escala de cores do Cade,

``` r
p1 + 
  theme_cade() + 
  scale_color_cade_d()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Escala continua

``` r
ggplot(mtcars, aes(wt, mpg, col = cyl)) + 
  geom_point(size = 5) + 
  theme_cade() + 
  scale_color_cade_c()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Escala com intervalos (bins)

``` r
ggplot(mtcars, aes(wt, mpg, col = cyl)) + 
  geom_point(size = 5) + 
  theme_cade() + 
  scale_color_cade_b()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

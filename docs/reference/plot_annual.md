# Plot annual ICES type of graphic returning central model data

Plot annual ICES type of graphic returning central model data

## Usage

``` r
plot_annual(
  fit,
  harvest_rate = TRUE,
  color_palette = scales::hue_pal(),
  base_size = 8,
  ...
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- harvest_rate:

  A Logical value, if `TRUE` the harvest rate will be plotted, if
  `FALSE` the fishing mortality will be plotted.

- color_palette:

  A function defining the color palette to be used or a vector of colors
  which is 1 longer than the number of stocks in the model. The extra
  color will be used for total estimate. See
  [scale_color_manual](https://ggplot2.tidyverse.org/reference/scale_manual.html).

- base_size:

  Base size parameter for ggplot. See ggtheme.

- ...:

  Additional arguments passed to `plot_*` functions.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
If `fleet = NULL`, a list of ggplot objects.

## Examples

``` r
data(fit)
# Annual plot with custom colors
plot_annual(fit, color_palette = scales::brewer_pal(palette = "Spectral"))
```

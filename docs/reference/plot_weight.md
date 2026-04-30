# Plot parameter weights

Plot parameter weights

## Usage

``` r
plot_weight(fit, log_scale = FALSE, base_size = 8)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- log_scale:

  Logical indicating whether the value axis should be log10 transformed.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(fit)
plot_weight(fit)

plot_weight(fit, log_scale = TRUE)
```
